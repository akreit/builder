package builder.extraction

import builder.domain.Leistung
import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.Random
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import org.http4s.Header
import org.http4s.Headers
import org.http4s.MediaType
import org.http4s.Method
import org.http4s.Request
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.`Content-Type`
import org.http4s.implicits.*
import org.typelevel.ci.CIString

import java.util.Base64
import scala.concurrent.duration.*

/** API key for the Anthropic Messages API. Never rendered: `toString` hides the value. */
opaque type ApiKey = String
object ApiKey:
  def from(raw: String): Option[ApiKey] = Some(raw.trim).filter(_.nonEmpty)
  def fromEnv: Option[ApiKey] = sys.env.lift("ANTHROPIC_API_KEY").flatMap(from)
  extension (k: ApiKey) private[extraction] def secret: String = k

/** [[OfferExtractor]] backed by Claude: the file goes to the Messages API as a `document` (PDF) or
  * `image` block, and structured outputs (`output_config.format`) make the answer JSON matching
  * [[RawOffer]].
  */
/** Retries of rate limits (429), server errors (5xx, incl. 529 overloaded) and connection errors:
  * the n-th retry waits about `base * 2^n` (equal jitter, at most `maxDelay`), or longer if the API
  * asks for it via `retry-after`.
  */
final case class RetryPolicy(
    maxRetries: Int,
    base: FiniteDuration,
    maxDelay: FiniteDuration,
    overallTimeout: FiniteDuration
):
  def delay(
      retry: Int,
      retryAfter: Option[FiniteDuration],
      random: Random[IO]
  ): IO[FiniteDuration] =
    val backoff = (base * (1L << retry.min(20))).min(maxDelay)
    val half = backoff.toMillis / 2
    random
      .betweenLong(0L, half + 1)
      .map(j => (half + j).millis.max(retryAfter.fold(Duration.Zero)(_.min(maxDelay))))

object RetryPolicy:
  val default: RetryPolicy = RetryPolicy(
    maxRetries = 3,
    base = 2.seconds,
    maxDelay = 60.seconds,
    overallTimeout = 6.minutes
  )

/** Outcome of one HTTP attempt. */
private enum Attempt:
  case Done(result: Either[ExtractionError, RawOffer])
  case Retry(error: ExtractionError, retryAfter: Option[FiniteDuration])

final class ClaudeOfferExtractor(
    client: Client[IO],
    key: Option[ApiKey],
    model: String,
    retry: RetryPolicy = RetryPolicy.default
) extends OfferExtractor[IO]:
  import ClaudeOfferExtractor.*

  def extract(
      file: Array[Byte],
      fileType: OfferFileType,
      known: List[Leistung]
  ): IO[Either[ExtractionError, RawOffer]] =
    key match
      case None    => IO.pure(Left(ExtractionError.MissingApiKey))
      case Some(k) =>
        val req = Request[IO](Method.POST, endpoint)
          .withHeaders(
            Headers(
              Header.Raw(CIString("x-api-key"), k.secret),
              Header.Raw(CIString("anthropic-version"), "2023-06-01"),
              `Content-Type`(MediaType.application.json)
            )
          )
          .withEntity(requestBody(model, file, fileType, known).noSpaces)
        Random
          .scalaUtilRandom[IO]
          .flatMap(random => withRetries(req, random, 0))
          .timeoutTo(retry.overallTimeout, IO.pure(Left(ExtractionError.TimedOut)))

  private def withRetries(
      req: Request[IO],
      random: Random[IO],
      retries: Int
  ): IO[Either[ExtractionError, RawOffer]] =
    once(req).flatMap {
      case Attempt.Done(result)                                   => IO.pure(result)
      case Attempt.Retry(error, _) if retries >= retry.maxRetries => IO.pure(Left(error))
      case Attempt.Retry(_, after)                                =>
        retry.delay(retries, after, random).flatMap(IO.sleep) >> withRetries(
          req,
          random,
          retries + 1
        )
    }

  private def once(req: Request[IO]): IO[Attempt] =
    client
      .run(req)
      .use(resp =>
        resp.bodyText.compile.string.map(body =>
          if resp.status.isSuccess then Attempt.Done(parseResponse(body))
          else
            val error = ExtractionError.HttpStatus(resp.status.code, errorType(body))
            if isRetryable(resp.status.code) then Attempt.Retry(error, retryAfter(resp.headers))
            else Attempt.Done(Left(error))
        )
      )
      .attempt
      .map {
        case Right(attempt) => attempt
        case Left(e)        =>
          val error = ExtractionError.Transport(e.getClass.getSimpleName)
          // A timeout may mean Claude is still working (and billing): never sent twice.
          if isConnectionError(e) then Attempt.Retry(error, None) else Attempt.Done(Left(error))
      }

object ClaudeOfferExtractor:
  val DefaultModel = "claude-sonnet-5-5"
  private val endpoint: Uri = uri"https://api.anthropic.com/v1/messages"

  /** Failures before the request reached Claude: safe to send again. Timeouts and broken responses
    * (e.g. Ember's ReadTimeout, ReachedEndOfStream) are not: Claude may have read the file already
    * and would bill twice.
    */
  private val connectionErrors: List[Class[? <: Throwable]] = List(
    classOf[java.net.ConnectException],
    classOf[java.net.UnknownHostException],
    classOf[java.nio.channels.ClosedChannelException]
  )

  private def isConnectionError(e: Throwable): Boolean = connectionErrors.exists(_.isInstance(e))

  private def isRetryable(code: Int): Boolean = code == 429 || (code >= 500 && code <= 599)

  private def retryAfter(headers: Headers): Option[FiniteDuration] =
    headers
      .get(CIString("retry-after"))
      .flatMap(_.head.value.trim.toLongOption)
      .filter(_ >= 0)
      .map(_.seconds)

  /** `error.type` of an API error body (e.g. `overloaded_error`); the message is not kept. */
  private def errorType(body: String): Option[String] =
    parse(body).toOption.flatMap(_.hcursor.downField("error").downField("type").as[String].toOption)

  /** Ember client with timeouts long enough for reading a multi-page PDF. */
  def resource(key: Option[ApiKey], model: String): Resource[IO, ClaudeOfferExtractor] =
    EmberClientBuilder
      .default[IO]
      .withTimeout(4.minutes)
      .withIdleConnectionTime(5.minutes)
      .build
      .map(ClaudeOfferExtractor(_, key, model))

  private val prompt: String =
    """Du liest ein Handwerkerangebot (Deutschland) für ein Renovierungsprojekt aus.
      |Antworte ausschließlich mit dem JSON gemäß Schema.
      |Regeln:
      |- Gib nur Werte an, die im Dokument stehen. Rechne nichts aus und rate nichts. Fehlt ein Wert, setze value auf "" (leerer String).
      |- Beträge, Mengen und den MwSt.-Satz als Dezimalzahl-String mit Punkt, ohne Tausendertrennzeichen und ohne Währung, z. B. "1234.50", "2.5", "19".
      |- Datumsangaben als ISO-Datum "JJJJ-MM-TT". "Gültig bis": wenn nur eine Bindefrist wie "4 Wochen" angegeben ist, value = "" und den Text in notes übernehmen.
      |- page ist die 1-basierte Seitenzahl, auf der der Wert steht (bei Bildern 1); 0 wenn der Wert fehlt.
      |- confidence: "high" wenn eindeutig lesbar und eindeutig zugeordnet; "medium" bei kleinen Unsicherheiten; "low" wenn schlecht lesbar, mehrdeutig, nur indirekt erschlossen oder wenn du den Wert doch berechnen musstest.
      |- lineItems: jede bepreiste Position einzeln (auch Rabatte als negative Positionen). Alternativ- und Eventualpositionen ohne Einrechnung in die Summe NICHT als lineItem, sondern in notes.
      |- unit z. B. "Stk", "m²", "m", "psch"; sizes z. B. "78 x 140 cm"; manufacturer und model nur wenn genannt.
      |- notes: Hinweise, Zahlungsbedingungen, Bindefristen, Alternativen. exclusions: ausdrücklich nicht enthaltene Leistungen.
      |
      |Leistungen (Vergleich mehrerer Angebote derselben Kategorie):
      |- Eine Leistung ist ein GROBER Arbeitsschritt bzw. eine Leistungsgruppe, wie ein Handwerker sie auf der Baustelle benennen würde, z. B. bei einem Dach: "Gerüst", "Abdecken", "Dämmen", "Neu decken", "Spenglerarbeiten", "Entsorgung"; bei Dachfenstern: "Fenster inkl. Zubehör", "Einbau", "Innenausbau", "Entsorgung". Pro Kategorie sind es typischerweise nur 4 bis 8 Leistungen.
      |- NICHT jede Position ist eine eigene Leistung: Material, Zubehör und Kleinteile gehören zu dem Arbeitsschritt, zu dem sie verbaut werden.
      |- Ordne jede Position (lineItems[].leistung) einer vorhandenen Leistung zu (deren id). Bevorzuge immer eine vorhandene Leistung, auch wenn sie etwas allgemeiner ist.
      |- Nur wenn eine Position zu keiner vorhandenen Leistung passt, schlage in neueLeistungen eine neue grobe Leistung vor (key z. B. "neu-1", kurzer deutscher name) und verwende den key. Lege nie zwei neue Leistungen für denselben Arbeitsschritt an.
      |- abdeckung: für JEDE vorhandene und jede neue Leistung genau ein Eintrag: status "enthalten" (Positionen dafür vorhanden), "ausgeschlossen" (ausdrücklich als nicht enthalten genannt), "nicht_enthalten" (eindeutig nicht angeboten, z. B. Angebot listet vollständig andere Leistungen) oder "unklar" (nicht sicher erkennbar). Im Zweifel "unklar" – nicht raten. page = Seite des Belegs (0 wenn keiner), confidence wie oben.""".stripMargin

  /** The category's existing Leistungen as prompt text. */
  def knownText(known: List[Leistung]): String =
    if known.isEmpty then
      "Vorhandene Leistungen dieser Kategorie: keine – schlage die passenden groben Leistungen neu vor."
    else
      "Vorhandene Leistungen dieser Kategorie (id: Name):\n" +
        known.map(l => s"- ${l.id.value}: ${l.name.value}").mkString("\n")

  /** Shared via `$defs`: inlining it 17 times makes the compiled grammar too large. */
  private val fieldDef: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      // No union types: structured outputs limit them. "" / 0 mean "not in the document".
      "value" -> Json.obj("type" -> "string".asJson),
      "page" -> Json.obj("type" -> "integer".asJson),
      "confidence" -> Json.obj(
        "type" -> "string".asJson,
        "enum" -> List("high", "medium", "low").asJson
      )
    ),
    "required" -> List("value", "page", "confidence").asJson,
    "additionalProperties" -> false.asJson
  )

  private val field: Json = Json.obj("$ref" -> "#/$defs/feld".asJson)

  private def obj(fields: List[(String, Json)]): Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.fromFields(fields),
    "required" -> fields.map(_._1).asJson,
    "additionalProperties" -> false.asJson
  )

  private def fields(names: String*): List[(String, Json)] = names.toList.map(_ -> field)

  private def arrayOf(item: Json): Json = Json.obj("type" -> "array".asJson, "items" -> item)

  val inputSchema: Json = obj(
    fields("contractor", "date", "validUntil") ++
      List(
        "lineItems" -> arrayOf(
          obj(
            fields(
              "description",
              "quantity",
              "unit",
              "sizes",
              "manufacturer",
              "model",
              "unitPrice",
              "total"
            ) :+ ("leistung" -> Json.obj("type" -> "string".asJson))
          )
        )
      ) ++
      fields("net", "vatRate", "vat", "gross") ++
      List(
        "notes" -> arrayOf(field),
        "exclusions" -> arrayOf(field),
        "neueLeistungen" -> arrayOf(
          obj(
            List(
              "key" -> Json.obj("type" -> "string".asJson),
              "name" -> Json.obj("type" -> "string".asJson)
            )
          )
        ),
        "abdeckung" -> arrayOf(
          obj(
            List(
              "leistung" -> Json.obj("type" -> "string".asJson),
              "status" -> Json.obj(
                "type" -> "string".asJson,
                "enum" -> List("enthalten", "nicht_enthalten", "ausgeschlossen", "unklar").asJson
              ),
              "page" -> Json.obj("type" -> "integer".asJson),
              "confidence" -> Json.obj(
                "type" -> "string".asJson,
                "enum" -> List("high", "medium", "low").asJson
              )
            )
          )
        )
      )
  ).deepMerge(Json.obj("$defs" -> Json.obj("feld" -> fieldDef)))

  private def fileBlock(file: Array[Byte], fileType: OfferFileType): Json =
    val source = Json.obj(
      "type" -> "base64".asJson,
      "media_type" -> fileType.mediaType.asJson,
      "data" -> Base64.getEncoder.encodeToString(file).asJson
    )
    fileType match
      case OfferFileType.Pdf => Json.obj("type" -> "document".asJson, "source" -> source)
      case OfferFileType.Jpeg | OfferFileType.Png =>
        Json.obj("type" -> "image".asJson, "source" -> source)

  def requestBody(
      model: String,
      file: Array[Byte],
      fileType: OfferFileType,
      known: List[Leistung]
  ): Json = Json.obj(
    "model" -> model.asJson,
    "max_tokens" -> 16000.asJson,
    "output_config" -> Json.obj(
      "format" -> Json.obj("type" -> "json_schema".asJson, "schema" -> inputSchema)
    ),
    "messages" -> List(
      Json.obj(
        "role" -> "user".asJson,
        "content" -> List(
          fileBlock(file, fileType),
          Json.obj("type" -> "text".asJson, "text" -> s"$prompt\n\n${knownText(known)}".asJson)
        ).asJson
      )
    ).asJson
  )

  /** Reads the structured JSON answer (first `text` block) out of a Messages API response. */
  def parseResponse(body: String): Either[ExtractionError, RawOffer] =
    for
      json <- parse(body).left.map(_ =>
        ExtractionError.UnexpectedResponse("keine gültige JSON-Antwort")
      )
      _ <- json.hcursor.downField("stop_reason").as[String].toOption match
        case Some("end_turn") | None => Right(())
        case Some(reason)            =>
          Left(ExtractionError.UnexpectedResponse(s"Antwort abgebrochen ($reason)"))
      blocks <- json.hcursor
        .downField("content")
        .as[List[Json]]
        .left
        .map(_ => ExtractionError.UnexpectedResponse("Antwort ohne content"))
      text <- blocks
        .find(_.hcursor.downField("type").as[String].contains("text"))
        .flatMap(_.hcursor.downField("text").as[String].toOption)
        .toRight(ExtractionError.UnexpectedResponse("Antwort ohne Text"))
      raw <- parse(text)
        .flatMap(_.as[RawOffer])
        .left
        .map(e =>
          ExtractionError.UnexpectedResponse(s"Antwort passt nicht zum Schema: ${e.getMessage}")
        )
    yield raw

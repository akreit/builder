package builder.extraction

import cats.effect.IO
import cats.effect.Resource
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
final class ClaudeOfferExtractor(client: Client[IO], key: Option[ApiKey], model: String)
    extends OfferExtractor[IO]:
  import ClaudeOfferExtractor.*

  def extract(file: Array[Byte], fileType: OfferFileType): IO[Either[ExtractionError, RawOffer]] =
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
          .withEntity(requestBody(model, file, fileType).noSpaces)
        client
          .run(req)
          .use(resp =>
            resp.bodyText.compile.string.map(body =>
              if resp.status.isSuccess then parseResponse(body)
              else Left(ExtractionError.HttpStatus(resp.status.code, body.take(500)))
            )
          )
          .attempt
          .map(
            _.left
              .map(e => ExtractionError.Transport(s"${e.getClass.getSimpleName}: ${e.getMessage}"))
              .flatten
          )

object ClaudeOfferExtractor:
  val DefaultModel = "claude-sonnet-5-5"
  private val endpoint: Uri = uri"https://api.anthropic.com/v1/messages"

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
      |- notes: Hinweise, Zahlungsbedingungen, Bindefristen, Alternativen. exclusions: ausdrücklich nicht enthaltene Leistungen.""".stripMargin

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
            )
          )
        )
      ) ++
      fields("net", "vatRate", "vat", "gross") ++
      List("notes" -> arrayOf(field), "exclusions" -> arrayOf(field))
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

  def requestBody(model: String, file: Array[Byte], fileType: OfferFileType): Json = Json.obj(
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
          Json.obj("type" -> "text".asJson, "text" -> prompt.asJson)
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

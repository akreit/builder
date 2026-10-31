package builder.http

import builder.domain.*
import builder.extraction.*
import builder.repo.*
import builder.storage.FileStore
import builder.totals.*
import builder.ui.Pages
import cats.effect.Clock
import cats.effect.IO
import cats.effect.Outcome
import cats.effect.std.UUIDGen
import io.circe.Codec
import org.http4s.HttpRoutes
import sttp.model.Part
import sttp.model.StatusCode
import sttp.monad.MonadError
import sttp.tapir.*
import sttp.tapir.files.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.server.http4s.Http4sServerOptions
import sttp.tapir.server.interceptor.DecodeFailureContext
import sttp.tapir.server.interceptor.decodefailure.DecodeFailureHandler
import sttp.tapir.server.interceptor.decodefailure.DefaultDecodeFailureHandler
import sttp.tapir.server.model.EndpointExtensions.*
import sttp.tapir.server.model.ValuedEndpointOutput

final case class HealthResponse(status: String) derives Codec.AsObject, Schema

final case class CategoryTotalDto(
    categoryId: String,
    category: String,
    status: String,
    offerId: Option[String],
    contractor: Option[String],
    reason: Option[String],
    net: Option[String],
    gross: Option[String],
    candidateOfferIds: List[String]
) derives Codec.AsObject,
      Schema

final case class TotalsResponse(
    projectId: String,
    currency: String,
    net: String,
    gross: String,
    complete: Boolean,
    categories: List[CategoryTotalDto],
    missingCategories: List[String],
    undecidedCategories: List[String]
) derives Codec.AsObject,
      Schema

object TotalsResponse:
  def from(t: ProjectTotals): TotalsResponse =
    TotalsResponse(
      projectId = t.projectId.value,
      currency = t.currency.code,
      net = t.net.show,
      gross = t.gross.show,
      complete = t.isComplete,
      categories = t.categories.map(toDto),
      missingCategories = t.missingCategories.map(_.name.value),
      undecidedCategories = t.undecidedCategories.map(_.name.value)
    )

  private def toDto(ct: CategoryTotal): CategoryTotalDto =
    val cid = ct.category.id.value
    val name = ct.category.name.value
    ct.status match
      case CategoryStatus.Counted(id, c, net, gross, r) =>
        CategoryTotalDto(
          cid,
          name,
          "counted",
          Some(id.value),
          Some(c.name),
          Some(r.toString),
          Some(net.show),
          Some(gross.show),
          Nil
        )
      case CategoryStatus.NoOffer =>
        CategoryTotalDto(cid, name, "no-offer", None, None, None, None, None, Nil)
      case CategoryStatus.NeedsDecision(ids) =>
        CategoryTotalDto(
          cid,
          name,
          "needs-decision",
          None,
          None,
          None,
          None,
          None,
          ids.map(_.value)
        )

/** Errors the JSON API can return; each case maps to one status code and JSON body. */
enum ApiError:
  case InvalidProjectId(projectId: String)
  case ProjectNotFound(projectId: String)
  case StorageFailed(code: Int)

object ApiError:
  given Codec.AsObject[InvalidProjectId] = Codec.AsObject.derived
  given Codec.AsObject[ProjectNotFound] = Codec.AsObject.derived
  given Schema[InvalidProjectId] = Schema.derived
  given Schema[ProjectNotFound] = Schema.derived
  given Codec.AsObject[StorageFailed] = Codec.AsObject.derived
  given Schema[StorageFailed] = Schema.derived

  val output: EndpointOutput.OneOf[ApiError, ApiError] = oneOf[ApiError](
    oneOfVariant(StatusCode.BadRequest, jsonBody[InvalidProjectId]),
    oneOfVariant(StatusCode.NotFound, jsonBody[ProjectNotFound]),
    oneOfVariant(StatusCode.InternalServerError, jsonBody[StorageFailed])
  )

/** Error of an HTML endpoint: status code plus a German HTML message. */
final case class HtmlError(status: StatusCode, html: String)

/** Headers of the CSRF guard; checked (security input) before any body is read. */
final case class HtmxHeaders(
    hxRequest: Option[String],
    origin: Option[String],
    host: Option[String]
)

/** The multipart form of an upload: only the `datei` part is decoded, other parts are skipped. */
final case class UploadForm(datei: Option[Part[Array[Byte]]])

object UploadForm:
  /** Tapir's raw multipart codec, restricted to the `datei` part (no default part type, so other
    * parts are not read into memory). Hand-written: the derived codec warns under -Werror.
    */
  given MultipartCodec[UploadForm] =
    MultipartCodec(
      RawBodyType.MultipartBody(Map("datei" -> RawBodyType.ByteArrayBody), None),
      MultipartCodec.Default.codec.map(parts => UploadForm(parts.find(_.name == "datei")))(form =>
        form.datei.toList
      )
    )

object Endpoints:
  val health =
    endpoint.get.in("health").out(jsonBody[HealthResponse])

  val totals =
    endpoint.get
      .in("projects" / path[String]("id") / "totals")
      .out(jsonBody[TotalsResponse])
      .errorOut(ApiError.output)

  private val htmlError: EndpointOutput[HtmlError] =
    statusCode.and(htmlBodyUtf8).mapTo[HtmlError]

  /** Endpoints whose decode failures are rendered as htmx error fragments. */
  val htmxNames: Set[String] = Set("upload-offer", "mark-favorite")

  /** Projektübersicht of the home project. */
  val overviewPage =
    endpoint.get.in("").out(htmlBodyUtf8).errorOut(htmlError)

  /** htmx fragment error: retargets the swap into the layout's `#fehler` slot. */
  val fragmentError: EndpointOutput[HtmlError] =
    statusCode
      .and(header("HX-Retarget", "#fehler"))
      .and(header("HX-Reswap", "innerHTML"))
      .and(htmlBodyUtf8)
      .mapTo[HtmlError]

  private val htmxHeaders: EndpointInput[HtmxHeaders] =
    header[Option[String]]("HX-Request")
      .and(header[Option[String]]("Origin"))
      .and(header[Option[String]]("Host"))
      .mapTo[HtmxHeaders]

  /** htmx: marks an offer as favorite and returns the re-rendered overview fragment. */
  val markFavorite =
    endpoint.post
      .name("mark-favorite")
      .in(
        "ui" / "projects" / path[String]("projectId") / "categories" / path[String](
          "categoryId"
        ) / "favorite" / path[String]("offerId")
      )
      .securityIn(htmxHeaders)
      .out(htmlBodyUtf8)
      .errorOut(fragmentError)

  /** htmx multipart: uploads an offer file (PDF/JPEG/PNG) into a category, reads it and returns the
    * re-rendered overview fragment.
    */
  val uploadOffer =
    endpoint.post
      .name("upload-offer")
      .in(
        "ui" / "projects" / path[String]("projectId") / "categories" / path[String](
          "categoryId"
        ) / "angebote"
      )
      .securityIn(htmxHeaders)
      .in(multipartBody[UploadForm])
      .out(htmlBodyUtf8)
      .errorOut(fragmentError)

object Api:
  /** Largest file (PDF, 20 MB) plus room for the multipart framing; larger bodies get 413 without
    * being read.
    */
  val MaxRequestBytes: Long = OfferFileType.Pdf.maxBytes.toLong + 64 * 1024

final class Api(
    repo: ProjectRepository,
    home: ProjectId,
    extractor: OfferExtractor[IO],
    files: FileStore
):
  private def totalsLogic(raw: String): IO[Either[ApiError, TotalsResponse]] =
    ProjectId.from(raw) match
      case Left(_)   => IO.pure(Left(ApiError.InvalidProjectId(raw)))
      case Right(id) =>
        repo
          .find(id)
          .map(
            _.map(v => TotalsResponse.from(Totals.compute(v.project))).left.map {
              case RepoError.Storage(code) => ApiError.StorageFailed(code)
              case RepoError.ProjectNotFound(_) | RepoError.CategoryNotFound(_) |
                  RepoError.OfferNotFound(_) | RepoError.Invalid(_) =>
                ApiError.ProjectNotFound(raw)
            }
          )

  private def notFound(what: String): HtmlError =
    HtmlError(StatusCode.NotFound, Pages.message(s"$what nicht gefunden").render)

  private val overview: IO[Either[HtmlError, String]] =
    repo
      .find(home)
      .map(_.map(v => Pages.overview(v)).left.map {
        case RepoError.Storage(code) =>
          HtmlError(
            StatusCode.InternalServerError,
            Pages.message(s"Datenbankfehler (Code $code)").render
          )
        case RepoError.ProjectNotFound(_) | RepoError.CategoryNotFound(_) |
            RepoError.OfferNotFound(_) | RepoError.Invalid(_) =>
          notFound("Projekt")
      })

  private def fragmentError(status: StatusCode, text: String): HtmlError =
    HtmlError(status, Pages.errorFragment(text).render)

  /** CSRF guard: only htmx requests, and an Origin (if sent) must match the Host. */
  private def checkRequest(h: HtmxHeaders): IO[Either[HtmlError, Unit]] = IO.pure {
    val forbidden = fragmentError(StatusCode.Forbidden, _)
    val sameOrigin = h.origin.forall(o => o.split("://", 2).lift(1).exists(h.host.contains))
    if !h.hxRequest.contains("true") then
      Left(forbidden("Anfrage abgelehnt: nur über die Oberfläche erlaubt"))
    else if !sameOrigin then Left(forbidden("Anfrage abgelehnt: fremde Herkunft"))
    else Right(())
  }

  private def favoriteLogic(
      projectId: String,
      categoryId: String,
      offerId: String
  ): IO[Either[HtmlError, String]] =
    val ids = for
      p <- ProjectId.from(projectId)
      c <- CategoryId.from(categoryId)
      o <- OfferId.from(offerId)
    yield (p, c, o)
    ids match
      case Left(e) => IO.pure(Left(fragmentError(StatusCode.BadRequest, Pages.describe(e))))
      case Right((p, c, o)) =>
        repo.setFavorite(p, c, o).map {
          case Right(view) => Right(Pages.projectFragment(view).render)
          case Left(e)     => Left(repoError(e))
        }

  private def repoError(e: RepoError): HtmlError = e match
    case RepoError.ProjectNotFound(_) =>
      fragmentError(StatusCode.NotFound, "Projekt nicht gefunden")
    case RepoError.CategoryNotFound(c) =>
      fragmentError(StatusCode.NotFound, s"Kategorie nicht gefunden: ${c.value}")
    case RepoError.OfferNotFound(o) =>
      fragmentError(StatusCode.NotFound, s"Angebot nicht gefunden: ${o.value}")
    case RepoError.Invalid(e) =>
      fragmentError(StatusCode.UnprocessableEntity, Pages.describe(e))
    case RepoError.Storage(code) =>
      fragmentError(StatusCode.InternalServerError, s"Datenbankfehler (Code $code)")

  private def newOfferId: IO[Either[DomainError, OfferId]] =
    UUIDGen[IO].randomUUID.map(u => OfferId.from(s"upl-${u.toString.replace("-", "").take(12)}"))

  private def uploadLogic(
      projectId: String,
      categoryId: String,
      form: UploadForm
  ): IO[Either[HtmlError, String]] =
    val checked = for
      datei <- form.datei.toRight(fragmentError(StatusCode.BadRequest, "Keine Datei ausgewählt"))
      bytes = datei.body
      ids <- (for
        p <- ProjectId.from(projectId)
        c <- CategoryId.from(categoryId)
      yield (p, c)).left.map(e => fragmentError(StatusCode.BadRequest, Pages.describe(e)))
      fileType <- OfferFileType
        .detect(bytes)
        .toRight(
          fragmentError(
            StatusCode.UnsupportedMediaType,
            "Bitte ein Angebot als PDF, JPEG oder PNG hochladen"
          )
        )
      _ <- Either.cond(
        bytes.length <= fileType.maxBytes,
        (),
        fragmentError(
          StatusCode.PayloadTooLarge,
          s"Datei zu groß (${fileType.extension.toUpperCase} höchstens ${fileType.maxMegabytes} MB)"
        )
      )
    yield (ids._1, ids._2, fileType, bytes, datei.fileName)
    checked match
      case Left(e)                                  => IO.pure(Left(e))
      case Right((p, c, fileType, bytes, fileName)) =>
        repo.find(p).flatMap {
          case Left(e)                                             => IO.pure(Left(repoError(e)))
          case Right(v) if !v.project.categories.exists(_.id == c) =>
            IO.pure(Left(repoError(RepoError.CategoryNotFound(c))))
          case Right(_) =>
            newOfferId.flatMap {
              case Left(e) =>
                IO.pure(Left(fragmentError(StatusCode.InternalServerError, Pages.describe(e))))
              case Right(id) =>
                val name = fileName.getOrElse(s"angebot.${fileType.extension}")
                val upload = files.save(id, fileType, bytes) >>
                  extractor
                    .extract(bytes, fileType)
                    .flatMap(extracted =>
                      Clock[IO].realTimeInstant.map(now =>
                        Upload(
                          id,
                          c,
                          name,
                          fileType,
                          now,
                          extracted.fold(
                            e => UploadOutcome.Failed(Pages.describe(e)),
                            UploadOutcome.Read(_)
                          )
                        )
                      )
                    )
                // Saving and reading may be cancelled (then the file goes); once read, the row
                // is written without interruption and the file is deleted only if that failed.
                IO.uncancelable(poll =>
                  poll(upload)
                    .guaranteeCase {
                      case Outcome.Succeeded(_)                    => IO.unit
                      case Outcome.Errored(_) | Outcome.Canceled() => files.delete(id, fileType)
                    }
                    .flatMap(u =>
                      repo
                        .addUpload(p, u)
                        .onError(_ => files.delete(id, fileType))
                        .flatTap(_.fold(_ => files.delete(id, fileType), _ => IO.unit))
                    )
                ).flatMap {
                  case Left(e)   => IO.pure(Left(repoError(e)))
                  case Right(()) =>
                    repo.find(p).map(_.map(Pages.projectFragment(_).render).left.map(repoError))
                }
            }
        }

  private val defaultDecodeFailures: DefaultDecodeFailureHandler[IO] =
    DefaultDecodeFailureHandler[IO]

  /** Decode failures of the htmx endpoints (e.g. a body over the limit: 413) as German error
    * fragments for `#fehler`.
    */
  private val htmxDecodeFailures: DefaultDecodeFailureHandler[IO] =
    defaultDecodeFailures.copy[IO](response = (status, _, _) =>
      val text = status match
        case StatusCode.PayloadTooLarge =>
          s"Datei zu groß (höchstens ${OfferFileType.Pdf.maxMegabytes} MB)"
        case other => s"Ungültige Anfrage (Status ${other.code})"
      ValuedEndpointOutput(Endpoints.fragmentError, fragmentError(status, text)))

  /** htmx endpoints get fragments, all others tapir's default responses. */
  private val decodeFailureHandler: DecodeFailureHandler[IO] = new DecodeFailureHandler[IO]:
    def apply(ctx: DecodeFailureContext)(implicit
        monad: MonadError[IO]
    ): IO[Option[ValuedEndpointOutput[?]]] =
      if ctx.endpoint.info.name.exists(Endpoints.htmxNames.contains) then htmxDecodeFailures(ctx)
      else defaultDecodeFailures(ctx)

  val routes: HttpRoutes[IO] = Http4sServerInterpreter[IO](
    Http4sServerOptions.customiseInterceptors[IO].decodeFailureHandler(decodeFailureHandler).options
  ).toRoutes(
    List(
      Endpoints.health.serverLogicSuccess[IO](_ => IO.pure(HealthResponse("ok"))),
      Endpoints.totals.serverLogic[IO](totalsLogic),
      Endpoints.overviewPage.serverLogic[IO](_ => overview),
      Endpoints.markFavorite
        .serverSecurityLogic[Unit, IO](checkRequest)
        .serverLogic(_ => favoriteLogic.tupled),
      Endpoints.uploadOffer
        .serverSecurityLogic[Unit, IO](checkRequest)
        .serverLogic(_ => uploadLogic.tupled)
        .maxRequestBodyLength(Api.MaxRequestBytes),
      staticResourcesGetServerEndpoint[IO]("static")(getClass.getClassLoader, "static")
    )
  )

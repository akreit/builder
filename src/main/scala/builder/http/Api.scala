package builder.http

import builder.domain.*
import builder.extraction.*
import builder.repo.*
import builder.storage.FileStore
import builder.totals.*
import builder.ui.Pages
import cats.effect.Clock
import cats.effect.IO
import cats.effect.std.UUIDGen
import io.circe.Codec
import org.http4s.HttpRoutes
import sttp.model.Part
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.files.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

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

object ApiError:
  given Codec.AsObject[InvalidProjectId] = Codec.AsObject.derived
  given Codec.AsObject[ProjectNotFound] = Codec.AsObject.derived
  given Schema[InvalidProjectId] = Schema.derived
  given Schema[ProjectNotFound] = Schema.derived

  val output: EndpointOutput.OneOf[ApiError, ApiError] = oneOf[ApiError](
    oneOfVariant(StatusCode.BadRequest, jsonBody[InvalidProjectId]),
    oneOfVariant(StatusCode.NotFound, jsonBody[ProjectNotFound])
  )

/** Error of an HTML endpoint: status code plus a German HTML message. */
final case class HtmlError(status: StatusCode, html: String)

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

  /** Projektübersicht of the home project. */
  val overviewPage =
    endpoint.get.in("").out(htmlBodyUtf8).errorOut(htmlError)

  /** htmx fragment error: retargets the swap into the layout's `#fehler` slot. */
  private val fragmentError: EndpointOutput[HtmlError] =
    statusCode
      .and(header("HX-Retarget", "#fehler"))
      .and(header("HX-Reswap", "innerHTML"))
      .and(htmlBodyUtf8)
      .mapTo[HtmlError]

  /** htmx: marks an offer as favorite and returns the re-rendered overview fragment. */
  val markFavorite =
    endpoint.post
      .in(
        "ui" / "projects" / path[String]("projectId") / "categories" / path[String](
          "categoryId"
        ) / "favorite" / path[String]("offerId")
      )
      .in(header[Option[String]]("HX-Request"))
      .in(header[Option[String]]("Origin"))
      .in(header[Option[String]]("Host"))
      .out(htmlBodyUtf8)
      .errorOut(fragmentError)

  /** htmx multipart: uploads an offer file (PDF/JPEG/PNG) into a category, reads it and returns the
    * re-rendered overview fragment.
    */
  val uploadOffer =
    endpoint.post
      .in(
        "ui" / "projects" / path[String]("projectId") / "categories" / path[String](
          "categoryId"
        ) / "angebote"
      )
      .in(header[Option[String]]("HX-Request"))
      .in(header[Option[String]]("Origin"))
      .in(header[Option[String]]("Host"))
      .in(multipartBody) // file field `datei`
      .out(htmlBodyUtf8)
      .errorOut(fragmentError)

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
            _.toRight(ApiError.ProjectNotFound(raw))
              .map(v => TotalsResponse.from(Totals.compute(v.project)))
          )

  private def notFound(what: String): HtmlError =
    HtmlError(StatusCode.NotFound, Pages.message(s"$what nicht gefunden").render)

  private val overview: IO[Either[HtmlError, String]] =
    repo.find(home).map(_.toRight(notFound("Projekt")).map(v => Pages.overview(v)))

  private def fragmentError(status: StatusCode, text: String): HtmlError =
    HtmlError(status, Pages.errorFragment(text).render)

  /** CSRF guard: only htmx requests, and an Origin (if sent) must match the Host. */
  private def checkRequest(
      hxRequest: Option[String],
      origin: Option[String],
      host: Option[String]
  ): Either[HtmlError, Unit] =
    val forbidden = fragmentError(StatusCode.Forbidden, _)
    val sameOrigin = origin.forall(o => o.split("://", 2).lift(1).exists(h => host.contains(h)))
    if !hxRequest.contains("true") then
      Left(forbidden("Anfrage abgelehnt: nur über die Oberfläche erlaubt"))
    else if !sameOrigin then Left(forbidden("Anfrage abgelehnt: fremde Herkunft"))
    else Right(())

  private def favoriteLogic(
      projectId: String,
      categoryId: String,
      offerId: String,
      hxRequest: Option[String],
      origin: Option[String],
      host: Option[String]
  ): IO[Either[HtmlError, String]] =
    val ids = for
      p <- ProjectId.from(projectId)
      c <- CategoryId.from(categoryId)
      o <- OfferId.from(offerId)
    yield (p, c, o)
    (checkRequest(hxRequest, origin, host), ids) match
      case (Left(e), _) => IO.pure(Left(e))
      case (_, Left(e)) => IO.pure(Left(fragmentError(StatusCode.BadRequest, Pages.describe(e))))
      case (Right(()), Right((p, c, o))) =>
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

  private val maxUploadBytes = 20 * 1024 * 1024

  private def newOfferId: IO[Either[DomainError, OfferId]] =
    UUIDGen[IO].randomUUID.map(u => OfferId.from(s"upl-${u.toString.replace("-", "").take(12)}"))

  private def uploadLogic(
      projectId: String,
      categoryId: String,
      hxRequest: Option[String],
      origin: Option[String],
      host: Option[String],
      parts: Seq[Part[Array[Byte]]]
  ): IO[Either[HtmlError, String]] =
    val checked = for
      _ <- checkRequest(hxRequest, origin, host)
      datei <- parts
        .find(_.name == "datei")
        .toRight(fragmentError(StatusCode.BadRequest, "Keine Datei ausgewählt"))
      bytes = datei.body
      ids <- (for
        p <- ProjectId.from(projectId)
        c <- CategoryId.from(categoryId)
      yield (p, c)).left.map(e => fragmentError(StatusCode.BadRequest, Pages.describe(e)))
      _ <- Either.cond(
        bytes.length <= maxUploadBytes,
        (),
        fragmentError(StatusCode.PayloadTooLarge, "Datei zu groß (höchstens 20 MB)")
      )
      fileType <- OfferFileType
        .detect(bytes)
        .toRight(
          fragmentError(
            StatusCode.UnsupportedMediaType,
            "Bitte ein Angebot als PDF, JPEG oder PNG hochladen"
          )
        )
    yield (ids._1, ids._2, fileType, bytes, datei.fileName)
    checked match
      case Left(e)                                  => IO.pure(Left(e))
      case Right((p, c, fileType, bytes, fileName)) =>
        repo.find(p).flatMap {
          case None => IO.pure(Left(repoError(RepoError.ProjectNotFound(p))))
          case Some(v) if !v.project.categories.exists(_.id == c) =>
            IO.pure(Left(repoError(RepoError.CategoryNotFound(c))))
          case Some(_) =>
            newOfferId.flatMap {
              case Left(e) =>
                IO.pure(Left(fragmentError(StatusCode.InternalServerError, Pages.describe(e))))
              case Right(id) =>
                for
                  _ <- files.save(id, fileType, bytes)
                  extracted <- extractor.extract(bytes, fileType)
                  now <- Clock[IO].realTimeInstant
                  outcome = extracted.fold(
                    e => UploadOutcome.Failed(Pages.describe(e)),
                    UploadOutcome.Read(_)
                  )
                  name = fileName.getOrElse(s"angebot.${fileType.extension}")
                  stored <- repo.addUpload(p, Upload(id, c, name, fileType, now, outcome))
                yield stored.map(Pages.projectFragment(_).render).left.map(repoError)
            }
        }

  val routes: HttpRoutes[IO] = Http4sServerInterpreter[IO]().toRoutes(
    List(
      Endpoints.health.serverLogicSuccess[IO](_ => IO.pure(HealthResponse("ok"))),
      Endpoints.totals.serverLogic[IO](totalsLogic),
      Endpoints.overviewPage.serverLogic[IO](_ => overview),
      Endpoints.markFavorite.serverLogic[IO](favoriteLogic.tupled),
      Endpoints.uploadOffer.serverLogic[IO](uploadLogic.tupled),
      staticResourcesGetServerEndpoint[IO]("static")(getClass.getClassLoader, "static")
    )
  )

package builder.http

import builder.domain.*
import builder.repo.ProjectRepository
import builder.repo.RepoError
import builder.totals.*
import builder.ui.Pages
import cats.effect.IO
import io.circe.Codec
import org.http4s.HttpRoutes
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

  /** htmx: marks an offer as favorite and returns the re-rendered overview fragment. */
  val markFavorite =
    endpoint.post
      .in(
        "ui" / "projects" / path[String]("projectId") / "categories" / path[String](
          "categoryId"
        ) / "favorite" / path[String]("offerId")
      )
      .out(htmlBodyUtf8)
      .errorOut(htmlError)

final class Api(repo: ProjectRepository, home: ProjectId):
  private def totalsLogic(raw: String): IO[Either[ApiError, TotalsResponse]] =
    ProjectId.from(raw) match
      case Left(_)   => IO.pure(Left(ApiError.InvalidProjectId(raw)))
      case Right(id) =>
        repo
          .find(id)
          .map(
            _.toRight(ApiError.ProjectNotFound(raw))
              .map(p => TotalsResponse.from(Totals.compute(p)))
          )

  private def notFound(what: String): HtmlError =
    HtmlError(StatusCode.NotFound, Pages.message(s"$what nicht gefunden").render)

  private val overview: IO[Either[HtmlError, String]] =
    repo.find(home).map(_.toRight(notFound("Projekt")).map(p => Pages.overview(p)))

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
      case Left(e) =>
        IO.pure(Left(HtmlError(StatusCode.BadRequest, Pages.message(Pages.describe(e)).render)))
      case Right((p, c, o)) =>
        repo.setFavorite(p, c, o).map {
          case Right(project)                     => Right(Pages.projectFragment(project).render)
          case Left(RepoError.ProjectNotFound(_)) => Left(notFound("Projekt"))
          case Left(RepoError.Invalid(e))         =>
            Left(HtmlError(StatusCode.UnprocessableEntity, Pages.message(Pages.describe(e)).render))
        }

  val routes: HttpRoutes[IO] = Http4sServerInterpreter[IO]().toRoutes(
    List(
      Endpoints.health.serverLogicSuccess[IO](_ => IO.pure(HealthResponse("ok"))),
      Endpoints.totals.serverLogic[IO](totalsLogic),
      Endpoints.overviewPage.serverLogic[IO](_ => overview),
      Endpoints.markFavorite.serverLogic[IO](favoriteLogic.tupled),
      staticResourcesGetServerEndpoint[IO]("static")(getClass.getClassLoader, "static")
    )
  )

package builder.http

import builder.domain.*
import builder.repo.ProjectRepository
import builder.totals.*
import cats.effect.IO
import io.circe.Codec
import org.http4s.HttpRoutes
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

final case class HealthResponse(status: String) derives Codec.AsObject, Schema

final case class CategoryTotalDto(
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
      missingCategories = t.missingCategories.map(_.toString),
      undecidedCategories = t.undecidedCategories.map(_.toString)
    )

  private def toDto(ct: CategoryTotal): CategoryTotalDto =
    val name = ct.category.toString
    ct.status match
      case CategoryStatus.Counted(id, c, net, gross, r) =>
        CategoryTotalDto(
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
        CategoryTotalDto(name, "no-offer", None, None, None, None, None, Nil)
      case CategoryStatus.NeedsDecision(ids) =>
        CategoryTotalDto(name, "needs-decision", None, None, None, None, None, ids.map(_.value))

/** Errors the HTTP API can return; each case maps to one status code and JSON body. */
enum ApiError:
  case InvalidProjectId(projectId: String)
  case ProjectNotFound(projectId: String)
  case CurrencyMismatch(offerId: String, expected: String, actual: String)
  case UnknownFavorite(category: String, offerId: String)
  case Internal(message: String)

object ApiError:
  given Codec.AsObject[InvalidProjectId] = Codec.AsObject.derived
  given Codec.AsObject[ProjectNotFound] = Codec.AsObject.derived
  given Codec.AsObject[CurrencyMismatch] = Codec.AsObject.derived
  given Codec.AsObject[UnknownFavorite] = Codec.AsObject.derived
  given Codec.AsObject[Internal] = Codec.AsObject.derived
  given Schema[InvalidProjectId] = Schema.derived
  given Schema[ProjectNotFound] = Schema.derived
  given Schema[CurrencyMismatch] = Schema.derived
  given Schema[UnknownFavorite] = Schema.derived
  given Schema[Internal] = Schema.derived

  def fromTotals(e: TotalsError): ApiError = e match
    case TotalsError.CurrencyMismatch(o, exp, act) =>
      CurrencyMismatch(o.value, exp.code, act.code)
    case TotalsError.UnknownFavorite(c, o) => UnknownFavorite(c.toString, o.value)
    case TotalsError.Arithmetic(err)       => Internal(err.toString)

  val output: EndpointOutput.OneOf[ApiError, ApiError] = oneOf[ApiError](
    oneOfVariant(StatusCode.BadRequest, jsonBody[InvalidProjectId]),
    oneOfVariant(StatusCode.NotFound, jsonBody[ProjectNotFound]),
    oneOfVariant(StatusCode.UnprocessableEntity, jsonBody[CurrencyMismatch]),
    oneOfVariant(StatusCode.UnprocessableEntity, jsonBody[UnknownFavorite]),
    oneOfVariant(StatusCode.InternalServerError, jsonBody[Internal])
  )

object Endpoints:
  val health =
    endpoint.get.in("health").out(jsonBody[HealthResponse])

  val totals =
    endpoint.get
      .in("projects" / path[String]("id") / "totals")
      .out(jsonBody[TotalsResponse])
      .errorOut(ApiError.output)

final class Api(repo: ProjectRepository):
  private def totalsLogic(raw: String): IO[Either[ApiError, TotalsResponse]] =
    ProjectId.from(raw) match
      case Left(_)   => IO.pure(Left(ApiError.InvalidProjectId(raw)))
      case Right(id) =>
        repo.find(id).map {
          case None    => Left(ApiError.ProjectNotFound(raw))
          case Some(p) => Totals.compute(p).map(TotalsResponse.from).left.map(ApiError.fromTotals)
        }

  val routes: HttpRoutes[IO] = Http4sServerInterpreter[IO]().toRoutes(
    List(
      Endpoints.health.serverLogicSuccess[IO](_ => IO.pure(HealthResponse("ok"))),
      Endpoints.totals.serverLogic[IO](totalsLogic)
    )
  )

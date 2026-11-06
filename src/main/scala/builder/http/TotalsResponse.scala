package builder.http

import builder.domain.*
import builder.totals.*
import io.circe.Codec
import sttp.tapir.Schema

/** One category row in the JSON totals response. */
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

/** JSON body of `GET /projects/{id}/totals`: project sums and per-category status. */
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
  /** Maps domain totals into the JSON DTO (German UI strings stay in HTML endpoints). */
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

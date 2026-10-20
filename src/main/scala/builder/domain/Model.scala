package builder.domain

import java.time.LocalDate
import scala.util.Try

private val IdPattern = "[A-Za-z0-9][A-Za-z0-9_-]{0,63}".r

private def validId(kind: String, raw: String): Either[DomainError, String] =
  Either.cond(IdPattern.matches(raw), raw, DomainError.InvalidId(kind, raw))

opaque type ProjectId = String
object ProjectId:
  def from(raw: String): Either[DomainError, ProjectId] = validId("project", raw)
  extension (id: ProjectId) def value: String = id

opaque type OfferId = String
object OfferId:
  def from(raw: String): Either[DomainError, OfferId] = validId("offer", raw)
  extension (id: OfferId) def value: String = id

/** VAT rate in percent, e.g. 19 or 7. */
opaque type VatRate = BigDecimal
object VatRate:
  def from(percent: BigDecimal): Either[DomainError, VatRate] =
    Either.cond(percent >= 0 && percent <= 100, percent, DomainError.InvalidVatRate(percent))
  extension (r: VatRate) def percent: BigDecimal = r

/** Strictly positive quantity of a line item. */
opaque type Quantity = BigDecimal
object Quantity:
  def from(value: BigDecimal): Either[DomainError, Quantity] =
    Either.cond(value > 0, value, DomainError.InvalidQuantity(value))
  extension (q: Quantity) def value: BigDecimal = q

object Dates:
  /** `LocalDate.of` fails on invalid input; keep that at the Java boundary. */
  def of(year: Int, month: Int, day: Int): Either[DomainError, LocalDate] =
    Try(LocalDate.of(year, month, day)).toEither.left.map(_ =>
      DomainError.InvalidDate(year, month, day)
    )

/** Trade a work package or offer belongs to. */
enum Category:
  case Windows, Roofing, Electrical, Plumbing, Flooring, Painting, Other

final case class WorkPackage(category: Category, description: String, areas: List[String])

final case class Contractor(name: String)

final case class LineItem(
    description: Extracted[String],
    quantity: Extracted[Quantity],
    unit: Option[Extracted[String]],
    sizes: Option[Extracted[String]],
    manufacturer: Option[Extracted[String]],
    model: Option[Extracted[String]],
    unitPrice: Extracted[Money],
    total: Extracted[Money]
)

final case class Offer(
    id: OfferId,
    contractor: Extracted[Contractor],
    date: Extracted[LocalDate],
    validUntil: Option[Extracted[LocalDate]],
    category: Category,
    lineItems: List[LineItem],
    net: Extracted[Money],
    vatRate: Extracted[VatRate],
    vat: Extracted[Money],
    gross: Extracted[Money],
    notes: List[String],
    exclusions: List[String]
)

/** A renovation project. `favorites` holds at most one chosen offer per category. */
final case class Project(
    id: ProjectId,
    name: String,
    currency: Currency,
    workPackages: List[WorkPackage],
    offers: List[Offer],
    favorites: Map[Category, OfferId]
)

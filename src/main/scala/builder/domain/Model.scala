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
  extension (r: VatRate)
    def percent: BigDecimal = r

    /** VAT on `net`, rounded HALF_UP to the currency's minor unit. The single rounding rule used
      * everywhere (offer validation, sample data, test fixtures).
      */
    def vatOn(net: Money): Money =
      val minor = (BigDecimal(net.minor.value) * r / 100)
        .setScale(0, BigDecimal.RoundingMode.HALF_UP)
        .toBigInt
      Money(MinorUnits(minor), net.currency)

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

/** A contractor offer whose amounts are known to be consistent. Only [[Offer.from]] builds one.
  *
  * Sign rules: `net`, `vat` and `gross` must be non-negative. Individual line items MAY be
  * negative, so that discount lines (e.g. "Rabatt") can be represented; they still have to sum to
  * `net`.
  */
final case class Offer private (
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

object Offer:
  private def nonNegative(field: String, m: Money): Either[DomainError, Money] =
    Either.cond(!m.isNegative, m, DomainError.NegativeAmount(field, m))

  /** Validates sign rules, `gross == net + vat`, `vat == round_HALF_UP(net * rate)` and that the
    * line-item totals sum to `net`.
    */
  def from(
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
  ): Either[DomainError, Offer] =
    val n = net.value
    val v = vat.value
    val g = gross.value
    val expectedVat = vatRate.value.vatOn(n)
    for
      _ <- nonNegative("net", n)
      _ <- nonNegative("vat", v)
      _ <- nonNegative("gross", g)
      sum <- n.plus(v).left.map(DomainError.InvalidMoney(_))
      _ <- Either.cond(sum == g, (), DomainError.GrossMismatch(n, v, g))
      _ <- Either.cond(expectedVat == v, (), DomainError.VatMismatch(expectedVat, v))
      items <- Money
        .sum(n.currency, lineItems.map(_.total.value))
        .left
        .map(DomainError.InvalidMoney(_))
      _ <- Either.cond(items == n, (), DomainError.LineItemsMismatch(items, n))
    yield Offer(
      id,
      contractor,
      date,
      validUntil,
      category,
      lineItems,
      net,
      vatRate,
      vat,
      gross,
      notes,
      exclusions
    )

/** A renovation project. Offer ids are unique; `favorites` holds at most one chosen offer per
  * category. Only [[Project.from]] builds one.
  */
final case class Project private (
    id: ProjectId,
    name: String,
    currency: Currency,
    workPackages: List[WorkPackage],
    offers: List[Offer],
    favorites: Map[Category, OfferId]
)

object Project:
  def from(
      id: ProjectId,
      name: String,
      currency: Currency,
      workPackages: List[WorkPackage],
      offers: List[Offer],
      favorites: Map[Category, OfferId]
  ): Either[DomainError, Project] =
    offers
      .groupBy(_.id)
      .collectFirst { case (oid, os) if os.sizeIs > 1 => DomainError.DuplicateOfferId(oid.value) }
      .toLeft(Project(id, name, currency, workPackages, offers, favorites))

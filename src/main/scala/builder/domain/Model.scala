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

opaque type CategoryId = String
object CategoryId:
  def from(raw: String): Either[DomainError, CategoryId] = validId("category", raw)
  extension (id: CategoryId) def value: String = id

/** Human-readable category name, trimmed and non-empty (e.g. "Dämmung"). */
opaque type CategoryName = String
object CategoryName:
  def from(raw: String): Either[DomainError, CategoryName] =
    val trimmed = raw.trim
    Either.cond(trimmed.nonEmpty, trimmed, DomainError.InvalidCategoryName(raw))
  extension (n: CategoryName) def value: String = n

/** A project-specific category (trade) that offers, favorites and work packages belong to. */
final case class Category(id: CategoryId, name: CategoryName)

final case class WorkPackage(category: CategoryId, description: String, areas: List[String])

final case class Contractor(name: String)

/** One offer position whose total is `round_HALF_UP(unitPrice * quantity)` in minor units. Only
  * [[LineItem.from]] builds one. Negative unit prices are allowed (discount lines).
  */
final case class LineItem private (
    description: Extracted[String],
    quantity: Extracted[Quantity],
    unit: Option[Extracted[String]],
    sizes: Option[Extracted[String]],
    manufacturer: Option[Extracted[String]],
    model: Option[Extracted[String]],
    unitPrice: Extracted[Money],
    total: Extracted[Money]
)

object LineItem:
  def from(
      description: Extracted[String],
      quantity: Extracted[Quantity],
      unit: Option[Extracted[String]],
      sizes: Option[Extracted[String]],
      manufacturer: Option[Extracted[String]],
      model: Option[Extracted[String]],
      unitPrice: Extracted[Money],
      total: Extracted[Money]
  ): Either[DomainError, LineItem] =
    val price = unitPrice.value
    val t = total.value
    val expected = price.times(quantity.value.value)
    for
      _ <- Either.cond(
        price.currency == t.currency,
        (),
        DomainError.InvalidMoney(MoneyError.CurrencyMismatch(price.currency, t.currency))
      )
      _ <- Either.cond(expected == t, (), DomainError.LineItemTotalMismatch(expected, t))
    yield LineItem(description, quantity, unit, sizes, manufacturer, model, unitPrice, total)

/** A contractor offer whose amounts are known to be consistent. Only [[Offer.from]] builds one.
  *
  * Sign rules: `net`, `vat` and `gross` must be non-negative. Individual line items MAY be
  * negative, so that discount lines (e.g. "Rabatt") can be represented; they still have to sum to
  * `net`. An offer without line items (Pauschalangebot, lump-sum offer) skips the line-sum check.
  */
final case class Offer private (
    id: OfferId,
    contractor: Extracted[Contractor],
    date: Extracted[LocalDate],
    validUntil: Option[Extracted[LocalDate]],
    category: CategoryId,
    lineItems: List[LineItem],
    net: Extracted[Money],
    vatRate: Extracted[VatRate],
    vat: Extracted[Money],
    gross: Extracted[Money],
    notes: List[String],
    exclusions: List[String]
):
  /** Every amount on the offer, for currency checks. */
  def amounts: List[Money] =
    List(net.value, vat.value, gross.value) ++
      lineItems.flatMap(li => List(li.unitPrice.value, li.total.value))

object Offer:
  private def nonNegative(field: String, m: Money): Either[DomainError, Money] =
    Either.cond(!m.isNegative, m, DomainError.NegativeAmount(field, m))

  private def checkLineItems(n: Money, lineItems: List[LineItem]): Either[DomainError, Unit] =
    lineItems match
      case Nil              => Right(()) // Pauschalangebot: only the lump sum is given
      case items @ (_ :: _) =>
        Money
          .sum(n.currency, items.map(_.total.value))
          .left
          .map(DomainError.InvalidMoney(_))
          .flatMap(sum => Either.cond(sum == n, (), DomainError.LineItemsMismatch(sum, n)))

  /** Validates sign rules, `gross == net + vat`, `vat == round_HALF_UP(net * rate)` and, if there
    * are line items, that their totals sum to `net`.
    */
  def from(
      id: OfferId,
      contractor: Extracted[Contractor],
      date: Extracted[LocalDate],
      validUntil: Option[Extracted[LocalDate]],
      category: CategoryId,
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
      _ <- checkLineItems(n, lineItems)
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

/** A renovation project. Only [[Project.from]] builds one, which guarantees:
  *   - category ids are unique, and every work package, offer and favorite references one of them;
  *   - offer ids are unique and every offer amount is in the project currency;
  *   - each favorite points to an offer of its own category (at most one per category).
  */
final case class Project private (
    id: ProjectId,
    name: String,
    currency: Currency,
    categories: List[Category],
    workPackages: List[WorkPackage],
    offers: List[Offer],
    favorites: Map[CategoryId, OfferId]
):
  /** Adds `offer`, re-validating the project (unique id, known category, project currency). */
  def withOffer(offer: Offer): Either[DomainError, Project] =
    Project.from(id, name, currency, categories, workPackages, offers :+ offer, favorites)

  /** Marks `offer` as the favorite of `category`, re-validating the project. */
  def withFavorite(category: CategoryId, offer: OfferId): Either[DomainError, Project] =
    Project.from(
      id,
      name,
      currency,
      categories,
      workPackages,
      offers,
      favorites + (category -> offer)
    )

object Project:
  private def firstDuplicate[A](values: List[A]): Option[A] =
    values.groupBy(identity).collectFirst { case (a, as) if as.sizeIs > 1 => a }

  private def checkRefs(
      known: Set[CategoryId],
      kind: String,
      refs: List[CategoryId]
  ): Either[DomainError, Unit] =
    refs
      .find(r => !known.contains(r))
      .map(r => DomainError.UnknownCategory(kind, r.value))
      .toLeft(())

  private def checkCurrency(expected: Currency, o: Offer): Either[DomainError, Unit] =
    o.amounts
      .find(_.currency != expected)
      .map(m => DomainError.OfferCurrencyMismatch(o.id.value, expected, m.currency))
      .toLeft(())

  private def checkFavorite(
      offers: List[Offer],
      category: CategoryId,
      fav: OfferId
  ): Either[DomainError, Unit] =
    Either.cond(
      offers.exists(o => o.id == fav && o.category == category),
      (),
      DomainError.FavoriteNotInCategory(category.value, fav.value)
    )

  def from(
      id: ProjectId,
      name: String,
      currency: Currency,
      categories: List[Category],
      workPackages: List[WorkPackage],
      offers: List[Offer],
      favorites: Map[CategoryId, OfferId]
  ): Either[DomainError, Project] =
    val known = categories.map(_.id).toSet
    for
      _ <- firstDuplicate(categories.map(_.id))
        .map(c => DomainError.DuplicateCategoryId(c.value))
        .toLeft(())
      _ <- firstDuplicate(offers.map(_.id))
        .map(o => DomainError.DuplicateOfferId(o.value))
        .toLeft(())
      _ <- checkRefs(known, "work package", workPackages.map(_.category))
      _ <- checkRefs(known, "offer", offers.map(_.category))
      _ <- checkRefs(known, "favorite", favorites.keys.toList)
      _ <- offers.foldLeft[Either[DomainError, Unit]](Right(()))((acc, o) =>
        acc.flatMap(_ => checkCurrency(currency, o))
      )
      _ <- favorites.toList.foldLeft[Either[DomainError, Unit]](Right(()))((acc, cf) =>
        acc.flatMap(_ => checkFavorite(offers, cf._1, cf._2))
      )
    yield Project(id, name, currency, categories, workPackages, offers, favorites)

package builder

import builder.domain.*
import org.scalatest.EitherValues

object Fixtures extends EitherValues:
  def offerId(s: String): OfferId = OfferId.from(s).value

  def category(id: String, name: String): Category =
    Category(CategoryId.from(id).value, CategoryName.from(name).value)

  val Windows: Category = category("windows", "Fenster")
  val Roofing: Category = category("roofing", "Dach")
  val Electrical: Category = category("electrical", "Elektro")
  val Plumbing: Category = category("plumbing", "Sanitär")
  val Flooring: Category = category("flooring", "Boden")
  val Painting: Category = category("painting", "Maler")
  val Other: Category = category("other", "Sonstiges")
  val allCategories: List[Category] =
    List(Windows, Roofing, Electrical, Plumbing, Flooring, Painting, Other)

  val vat19: VatRate = VatRate.from(19).value

  def lineItem(description: String, total: Money): LineItem =
    LineItem
      .from(
        description = Extracted.manual(description),
        quantity = Extracted.manual(Quantity.from(1).value),
        unit = None,
        sizes = None,
        manufacturer = None,
        model = None,
        unitPrice = Extracted.manual(total),
        total = Extracted.manual(total)
      )
      .value

  /** Raw arguments to [[Offer.from]], so tests can build inconsistent variants. */
  final case class OfferArgs(
      id: OfferId,
      category: Category,
      lineItems: List[LineItem],
      net: Money,
      vat: Money,
      gross: Money
  ):
    def build: Either[DomainError, Offer] =
      Offer.from(
        id = id,
        contractor = Extracted.manual(Contractor(s"Contractor ${id.value}")),
        date = Extracted.manual(Dates.of(2026, 10, 1).value),
        validUntil = None,
        category = category.id,
        lineItems = lineItems,
        net = Extracted.manual(net),
        vatRate = Extracted.manual(vat19),
        vat = Extracted.manual(vat),
        gross = Extracted.manual(gross),
        notes = Nil,
        exclusions = Nil
      )

  /** Consistent arguments: one line item equal to net, VAT via the shared HALF_UP helper. */
  def offerArgs(
      id: String,
      cat: Category,
      netCents: Long,
      cur: Currency = Currency.EUR
  ): OfferArgs =
    val net = Money.ofMinor(netCents, cur)
    val vat = vat19.vatOn(net)
    OfferArgs(offerId(id), cat, List(lineItem("work", net)), net, vat, net.plus(vat).value)

  def offer(id: String, cat: Category, netCents: Long, cur: Currency = Currency.EUR): Offer =
    offerArgs(id, cat, netCents, cur).build.value

  def projectE(
      wps: List[Category],
      offers: List[Offer],
      favorites: Map[Category, OfferId] = Map.empty,
      id: String = "p1",
      categories: List[Category] = allCategories
  ): Either[DomainError, Project] =
    Project.from(
      ProjectId.from(id).value,
      "Test",
      Currency.EUR,
      categories,
      wps.map(c => WorkPackage(c.id, "wp", Nil)),
      offers,
      favorites.map((c, o) => c.id -> o)
    )

  def project(
      wps: List[Category],
      offers: List[Offer],
      favorites: Map[Category, OfferId] = Map.empty,
      id: String = "p1",
      categories: List[Category] = allCategories
  ): Project = projectE(wps, offers, favorites, id, categories).value

  def eur(cents: Long): Money = Money.ofMinor(cents, Currency.EUR)

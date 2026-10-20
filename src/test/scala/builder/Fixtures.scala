package builder

import builder.domain.*
import org.scalatest.EitherValues

object Fixtures extends EitherValues:
  def offerId(s: String): OfferId = OfferId.from(s).value

  val vat19: VatRate = VatRate.from(19).value

  def lineItem(description: String, total: Money): LineItem =
    LineItem(
      description = Extracted.manual(description),
      quantity = Extracted.manual(Quantity.from(1).value),
      unit = None,
      sizes = None,
      manufacturer = None,
      model = None,
      unitPrice = Extracted.manual(total),
      total = Extracted.manual(total)
    )

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
        category = category,
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

  def project(
      wps: List[Category],
      offers: List[Offer],
      favorites: Map[Category, OfferId] = Map.empty,
      id: String = "p1"
  ): Project =
    Project
      .from(
        ProjectId.from(id).value,
        "Test",
        Currency.EUR,
        wps.map(WorkPackage(_, "wp", Nil)),
        offers,
        favorites
      )
      .value

  def eur(cents: Long): Money = Money.ofMinor(cents, Currency.EUR)

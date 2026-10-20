package builder

import builder.domain.*
import org.scalatest.EitherValues

object Fixtures extends EitherValues:
  def offerId(s: String): OfferId = OfferId.from(s).value

  def offer(id: String, cat: Category, netCents: Long, cur: Currency = Currency.EUR): Offer =
    def m(c: Long) = Extracted.manual(Money.ofMinor(c, cur))
    val vat = netCents * 19 / 100
    Offer(
      id = offerId(id),
      contractor = Extracted.manual(Contractor(s"Contractor $id")),
      date = Extracted.manual(Dates.of(2026, 10, 1).value),
      validUntil = None,
      category = cat,
      lineItems = Nil,
      net = m(netCents),
      vatRate = Extracted.manual(VatRate.from(19).value),
      vat = m(vat),
      gross = m(netCents + vat),
      notes = Nil,
      exclusions = Nil
    )

  def project(
      wps: List[Category],
      offers: List[Offer],
      favorites: Map[Category, OfferId] = Map.empty
  ): Project =
    Project(
      ProjectId.from("p1").value,
      "Test",
      Currency.EUR,
      wps.map(WorkPackage(_, "wp", Nil)),
      offers,
      favorites
    )

  def eur(cents: Long): Money = Money.ofMinor(cents, Currency.EUR)

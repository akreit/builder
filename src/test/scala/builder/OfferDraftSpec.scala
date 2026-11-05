package builder

import builder.domain.*
import builder.extraction.*
import builder.ui.Pages
import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class OfferDraftSpec extends AnyFreeSpec with Matchers with EitherValues:
  import RawOffers.*

  private def validate(raw: RawOffer) =
    OfferDraft.validate(
      raw,
      Fixtures.offerId("upl-1"),
      CategoryId.from("fenster").value,
      Currency.EUR
    )

  "a consistent extraction becomes an Offer that keeps pages and confidence" in {
    val offer = validate(dachfenster).value
    offer.net.value shouldBe Money.ofMinor(501_000, Currency.EUR)
    offer.lineItems.map(_.total).collect { case PriceInfo.Known(e) =>
      e.value.show
    } shouldBe List("3750.00", "1260.00")
    offer.lineItems.headOption.flatMap(_.model).map(_.confidence) shouldBe Some(Confidence.Low)
    offer.net.page.map(_.value) shouldBe Some(3)
    offer.exclusions shouldBe List("Innenfutter")
  }

  "every missing or unreadable field is reported, not just the first" in {
    val raw = dachfenster.copy(contractor = absent, net = f("fünftausend"), vatRate = absent)
    validate(raw).left.value.toList shouldBe List(
      Issue.Missing("Firma"),
      Issue.Unreadable("Netto", "fünftausend"),
      Issue.Missing("MwSt.-Satz")
    )
  }

  "a line item whose total is not quantity × unit price is reported with its position" in {
    val raw = dachfenster.copy(lineItems =
      List(item("Dachfenster liefern", "3", "Stk", "1250.00", "3700.00"))
    )
    validate(raw).left.value.toList shouldBe List(
      Issue.Invalid(
        "Position 1",
        DomainError.LineItemTotalMismatch(
          Money.ofMinor(375_000, Currency.EUR),
          Money.ofMinor(370_000, Currency.EUR)
        )
      )
    )
  }

  "line items that do not sum to net are reported" in {
    val raw = dachfenster.copy(lineItems = dachfenster.lineItems.take(1))
    validate(raw).left.value.toList.map(Pages.describe) shouldBe List(
      "Angebot: Summe der Positionen 3.750,00 € ≠ Netto 5.010,00 €"
    )
  }

  "low-confidence fields are listed by German label" in {
    dachfenster.lowConfidenceFields shouldBe List("Position 1: Modell")
  }

  "an offer with a line item missing price and quantity is accepted" in {
    val raw = dachfenster.copy(
      lineItems = List(
        item("Dachfenster liefern", "3", "Stk", "1250.00", "3750.00"),
        RawLineItem(
          f("Einbau ohne Mengen/Preise", 2),
          absent,
          f("Stk", 2),
          absent,
          absent,
          absent,
          absent,
          absent
        )
      )
    )
    val offer = validate(raw).value
    offer.lineItems.size shouldBe 2
    offer.lineItems.lift(1).map(_.quantity) shouldBe Some(QuantityInfo.Missing)
    offer.lineItems.lift(1).map(_.unitPrice) shouldBe Some(PriceInfo.Missing)
    offer.lineItems.lift(1).map(_.total) shouldBe Some(PriceInfo.Missing)
    offer.net.value shouldBe Money.ofMinor(501_000, Currency.EUR)
  }

  "a fully absent line quantity/price does not produce Missing issues" in {
    val raw = dachfenster.copy(lineItems =
      List(
        RawLineItem(
          f("Nur Beschreibung"),
          absent,
          absent,
          absent,
          absent,
          absent,
          absent,
          absent
        )
      )
    )
    // Offer-level amounts still required and consistent; line-sum skipped.
    val ok = raw.copy(net = f("100.00", 3), vat = f("19.00", 3), gross = f("119.00", 3))
    validate(ok).isRight shouldBe true
  }

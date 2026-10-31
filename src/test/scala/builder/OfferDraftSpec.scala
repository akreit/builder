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
      CategoryId.from("dachfenster").value,
      Currency.EUR
    )

  "a consistent extraction becomes an Offer that keeps pages and confidence" in {
    val offer = validate(dachfenster).value
    offer.net.value shouldBe Money.ofMinor(501_000, Currency.EUR)
    offer.lineItems.map(_.total.value.show) shouldBe List("3750.00", "1260.00")
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

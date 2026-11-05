package builder

import builder.domain.*
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class OfferSpec extends AnyFunSuite with Matchers with EitherValues with ScalaCheckPropertyChecks:
  import Fixtures.*

  private val netGen = Gen.choose(0L, 10_000_000L)
  private val deltaGen = Gen.oneOf(Gen.choose(-1000L, -1L), Gen.choose(1L, 1000L))

  test("VAT is rounded HALF_UP to minor units") {
    vat19.vatOn(eur(50)) shouldBe eur(10) // 9.5 -> 10
    vat19.vatOn(eur(150)) shouldBe eur(29) // 28.5 -> 29
    vat19.vatOn(eur(1)) shouldBe eur(0) // 0.19 -> 0
  }

  test("consistent offers are accepted") {
    forAll(netGen)(n => offerArgs("a", Other, n).build.isRight shouldBe true)
  }

  test("offers whose gross != net + vat are rejected") {
    forAll(netGen, deltaGen) { (n, d) =>
      val a = offerArgs("a", Other, n)
      val bad = a.copy(gross = eur(a.gross.minor.value.toLong + d))
      bad.build.left.value should (be(DomainError.GrossMismatch(a.net, a.vat, bad.gross)) or be(
        DomainError.NegativeAmount("gross", bad.gross)
      ))
    }
  }

  test("offers whose vat is not HALF_UP(net * rate) are rejected") {
    forAll(netGen, deltaGen) { (n, d) =>
      val a = offerArgs("a", Other, n)
      val badVat = eur(a.vat.minor.value.toLong + d)
      val bad = a.copy(vat = badVat, gross = a.net.plus(badVat).value)
      bad.build.left.value should (be(DomainError.VatMismatch(a.vat, badVat)) or be(
        DomainError.NegativeAmount("vat", badVat)
      ))
    }
  }

  test("offers whose line items do not sum to net are rejected") {
    forAll(netGen, deltaGen) { (n, d) =>
      val a = offerArgs("a", Other, n)
      val items = a.lineItems :+ lineItem("extra", eur(d))
      a.copy(lineItems = items).build.left.value shouldBe
        DomainError.LineItemsMismatch(eur(n + d), a.net)
    }
  }

  test("a negative discount line item is allowed if the items still sum to net") {
    val a = offerArgs("a", Windows, 90_000)
    val items = List(lineItem("Fenster", eur(100_000)), lineItem("Rabatt", eur(-10_000)))
    a.copy(lineItems = items).build.value.lineItems.map(_.total) shouldBe List(
      PriceInfo.Known(Extracted.manual(eur(100_000))),
      PriceInfo.Known(Extracted.manual(eur(-10_000)))
    )
  }

  test("negative offer net is rejected") {
    val net = eur(-100)
    val vat = vat19.vatOn(net)
    val a = OfferArgs(
      offerId("a"),
      Other,
      List(lineItem("Rabatt", net)),
      net,
      vat,
      net.plus(vat).value
    )
    a.build.left.value shouldBe DomainError.NegativeAmount("net", net)
  }

  test("line items in another currency are rejected") {
    val a = offerArgs("a", Other, 100)
    a.copy(lineItems = List(lineItem("x", Money.ofMinor(100, Currency.CHF))))
      .build
      .left
      .value shouldBe DomainError.InvalidMoney(
      MoneyError.CurrencyMismatch(Currency.EUR, Currency.CHF)
    )
  }

  test("a Pauschalangebot without line items is accepted and skips the line-sum check") {
    val a = offerArgs("pausch", Other, 480_000).copy(lineItems = Nil)
    val o = a.build.value
    o.lineItems shouldBe Nil
    o.net.value shouldBe eur(480_000)
    o.gross.value shouldBe eur(571_200)
  }

  test("a Pauschalangebot still has to satisfy gross == net + vat") {
    val a = offerArgs("pausch", Other, 480_000).copy(lineItems = Nil)
    a.copy(gross = eur(1)).build.left.value shouldBe DomainError.GrossMismatch(a.net, a.vat, eur(1))
  }

  private def item(qty: String, unitCents: Long, totalCents: Long) =
    LineItem.from(
      description = Extracted.manual("Dämmung"),
      quantity = QuantityInfo.Known(Extracted.manual(Quantity.from(BigDecimal(qty)).value)),
      unit = Some(Extracted.manual("m²")),
      sizes = None,
      manufacturer = None,
      model = None,
      unitPrice = PriceInfo.Known(Extracted.manual(eur(unitCents))),
      total = PriceInfo.Known(Extracted.manual(eur(totalCents)))
    )

  test("a line item total must equal unit price × quantity") {
    item("120", 4_850, 582_000).value.total shouldBe PriceInfo.Known(Extracted.manual(eur(582_000)))
    item("120", 4_850, 582_001).left.value shouldBe
      DomainError.LineItemTotalMismatch(eur(582_000), eur(582_001))
  }

  test("a line item total is rounded HALF_UP to cents") {
    item("18.5", 8_901, 164_669).isRight shouldBe true // 164668.5 -> 164669
    item("0.5", 1, 1).isRight shouldBe true // 0.5 -> 1
    item("0.5", 1, 0).left.value shouldBe DomainError.LineItemTotalMismatch(eur(1), eur(0))
  }

  test("a line item with unit price and total in different currencies is rejected") {
    LineItem
      .from(
        Extracted.manual("x"),
        QuantityInfo.Known(Extracted.manual(Quantity.from(1).value)),
        None,
        None,
        None,
        None,
        PriceInfo.Known(Extracted.manual(eur(100))),
        PriceInfo.Known(Extracted.manual(Money.ofMinor(100, Currency.CHF)))
      )
      .left
      .value shouldBe DomainError.InvalidMoney(
      MoneyError.CurrencyMismatch(Currency.EUR, Currency.CHF)
    )
  }

  test("a line item with missing quantity and price is accepted") {
    val li = LineItem
      .from(
        Extracted.manual("Dachfenster"),
        QuantityInfo.Missing,
        Some(Extracted.manual("Stk")),
        None,
        None,
        None,
        PriceInfo.Missing,
        PriceInfo.Missing
      )
      .value
    li.quantity shouldBe QuantityInfo.Missing
    li.unitPrice shouldBe PriceInfo.Missing
    li.total shouldBe PriceInfo.Missing
  }

  test("an offer with a line item missing price is accepted and counted via net/gross") {
    val net = eur(5_000)
    val vat = vat19.vatOn(net)
    val incomplete = LineItem
      .from(
        Extracted.manual("Position ohne Preis"),
        QuantityInfo.Known(Extracted.manual(Quantity.from(1).value)),
        None,
        None,
        None,
        None,
        PriceInfo.Missing,
        PriceInfo.Missing
      )
      .value
    val o = Offer
      .from(
        offerId("missing-price"),
        Extracted.manual(Contractor("Firma")),
        Extracted.manual(Dates.of(2026, 10, 1).value),
        None,
        Other.id,
        List(incomplete),
        Extracted.manual(net),
        Extracted.manual(vat19),
        Extracted.manual(vat),
        Extracted.manual(net.plus(vat).value),
        Nil,
        Nil
      )
      .value
    o.net.value shouldBe net
    o.lineItems.headOption.map(_.unitPrice) shouldBe Some(PriceInfo.Missing)
  }

  test("line-sum check is skipped when any line total is missing") {
    val net = eur(10_000)
    val vat = vat19.vatOn(net)
    val withTotal = lineItem("mit Preis", eur(3_000))
    val without = LineItem
      .from(
        Extracted.manual("ohne Preis"),
        QuantityInfo.Missing,
        None,
        None,
        None,
        None,
        PriceInfo.Missing,
        PriceInfo.Missing
      )
      .value
    Offer
      .from(
        offerId("partial"),
        Extracted.manual(Contractor("Firma")),
        Extracted.manual(Dates.of(2026, 10, 1).value),
        None,
        Other.id,
        List(withTotal, without),
        Extracted.manual(net),
        Extracted.manual(vat19),
        Extracted.manual(vat),
        Extracted.manual(net.plus(vat).value),
        Nil,
        Nil
      )
      .isRight shouldBe true
  }

  test("lineSumCheck is Consistent when every position has a total that sums to net") {
    offer("a", Other, 90_000).lineSumCheck shouldBe Some(LineSumCheck.Consistent)
  }

  test("lineSumCheck is empty when a position has no price") {
    val net = eur(5_000)
    val vat = vat19.vatOn(net)
    val incomplete = LineItem
      .from(
        Extracted.manual("ohne Preis"),
        QuantityInfo.Missing,
        None,
        None,
        None,
        None,
        PriceInfo.Missing,
        PriceInfo.Missing
      )
      .value
    val o = Offer
      .from(
        offerId("nc"),
        Extracted.manual(Contractor("Firma")),
        Extracted.manual(Dates.of(2026, 10, 1).value),
        None,
        Other.id,
        List(lineItem("mit Preis", eur(3_000)), incomplete),
        Extracted.manual(net),
        Extracted.manual(vat19),
        Extracted.manual(vat),
        Extracted.manual(net.plus(vat).value),
        Nil,
        Nil
      )
      .value
    o.lineSumCheck shouldBe None
  }

  test("a Pauschalangebot has Consistent lineSumCheck") {
    offerArgs("pausch", Other, 480_000).copy(lineItems = Nil).build.value.lineSumCheck shouldBe
      Some(LineSumCheck.Consistent)
  }

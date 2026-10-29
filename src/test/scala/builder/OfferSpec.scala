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
    forAll(netGen)(n => offerArgs("a", Category.Other, n).build.isRight shouldBe true)
  }

  test("offers whose gross != net + vat are rejected") {
    forAll(netGen, deltaGen) { (n, d) =>
      val a = offerArgs("a", Category.Other, n)
      val bad = a.copy(gross = eur(a.gross.minor.value.toLong + d))
      bad.build.left.value should (be(DomainError.GrossMismatch(a.net, a.vat, bad.gross)) or be(
        DomainError.NegativeAmount("gross", bad.gross)
      ))
    }
  }

  test("offers whose vat is not HALF_UP(net * rate) are rejected") {
    forAll(netGen, deltaGen) { (n, d) =>
      val a = offerArgs("a", Category.Other, n)
      val badVat = eur(a.vat.minor.value.toLong + d)
      val bad = a.copy(vat = badVat, gross = a.net.plus(badVat).value)
      bad.build.left.value should (be(DomainError.VatMismatch(a.vat, badVat)) or be(
        DomainError.NegativeAmount("vat", badVat)
      ))
    }
  }

  test("offers whose line items do not sum to net are rejected") {
    forAll(netGen, deltaGen) { (n, d) =>
      val a = offerArgs("a", Category.Other, n)
      val items = a.lineItems :+ lineItem("extra", eur(d))
      a.copy(lineItems = items).build.left.value shouldBe
        DomainError.LineItemsMismatch(eur(n + d), a.net)
    }
  }

  test("a negative discount line item is allowed if the items still sum to net") {
    val a = offerArgs("a", Category.Windows, 90_000)
    val items = List(lineItem("Fenster", eur(100_000)), lineItem("Rabatt", eur(-10_000)))
    a.copy(lineItems = items).build.value.lineItems.map(_.total.value) shouldBe
      List(eur(100_000), eur(-10_000))
  }

  test("negative offer net is rejected") {
    val net = eur(-100)
    val vat = vat19.vatOn(net)
    val a = OfferArgs(
      offerId("a"),
      Category.Other,
      List(lineItem("Rabatt", net)),
      net,
      vat,
      net.plus(vat).value
    )
    a.build.left.value shouldBe DomainError.NegativeAmount("net", net)
  }

  test("line items in another currency are rejected") {
    val a = offerArgs("a", Category.Other, 100)
    a.copy(lineItems = List(lineItem("x", Money.ofMinor(100, Currency.CHF))))
      .build
      .left
      .value shouldBe DomainError.InvalidMoney(
      MoneyError.CurrencyMismatch(Currency.EUR, Currency.CHF)
    )
  }

  test("projects reject duplicate offer ids") {
    val a = offer("dup", Category.Windows, 100)
    val b = offer("dup", Category.Roofing, 200)
    Project
      .from(ProjectId.from("p").value, "P", Currency.EUR, Nil, List(a, b), Map.empty)
      .left
      .value shouldBe DomainError.DuplicateOfferId("dup")
  }

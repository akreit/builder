package builder

import builder.domain.*
import builder.totals.*
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TotalsSpec extends AnyFunSuite with Matchers with EitherValues with ScalaCheckPropertyChecks:
  import Fixtures.*

  private def statusOf(t: ProjectTotals, c: Category): Option[CategoryStatus] =
    t.categories.find(_.category == c).map(_.status)

  test("favorite offer is counted when a category has several offers") {
    val a = offer("a", Windows, 10000)
    val b = offer("b", Windows, 20000)
    val t =
      Totals.compute(project(Nil, List(a, b), Map(Windows -> b.id), categories = List(Windows)))
    statusOf(t, Windows) shouldBe Some(
      CategoryStatus.Counted(
        b.id,
        b.contractor.value,
        eur(20000),
        eur(23800),
        CountedReason.Favorite
      )
    )
    t.net shouldBe eur(20000)
    t.gross shouldBe eur(23800)
    t.isComplete shouldBe true
  }

  test("single offer is counted without being a favorite") {
    val a = offer("a", Roofing, 50000)
    val t = Totals.compute(project(Nil, List(a), categories = List(Roofing)))
    statusOf(t, Roofing) shouldBe Some(
      CategoryStatus.Counted(
        a.id,
        a.contractor.value,
        eur(50000),
        eur(59500),
        CountedReason.OnlyOffer
      )
    )
    t.net shouldBe eur(50000)
    t.gross shouldBe eur(59500)
    t.isComplete shouldBe true
  }

  test("project categories without an offer are listed as missing, in project order") {
    val t = Totals.compute(
      project(Nil, List(offer("a", Roofing, 100)), categories = List(Plumbing, Roofing, Other))
    )
    t.categories.map(_.category) shouldBe List(Plumbing, Roofing, Other)
    t.missingCategories shouldBe List(Plumbing, Other)
    statusOf(t, Plumbing) shouldBe Some(CategoryStatus.NoOffer)
    t.net shouldBe eur(100)
    t.isComplete shouldBe false
  }

  test("several offers without favorite are reported, not silently picked") {
    val offers =
      List(offer("a", Electrical, 100), offer("b", Electrical, 200), offer("c", Painting, 300))
    val t = Totals.compute(project(Nil, offers, categories = List(Electrical, Painting)))
    statusOf(t, Electrical) shouldBe Some(
      CategoryStatus.NeedsDecision(List(offerId("a"), offerId("b")))
    )
    t.undecidedCategories shouldBe List(Electrical)
    t.net shouldBe eur(300)
    t.isComplete shouldBe false
  }

  test("money addition across currencies fails") {
    Money.ofMinor(1, Currency.EUR).plus(Money.ofMinor(1, Currency.CHF)).left.value shouldBe
      MoneyError.CurrencyMismatch(Currency.EUR, Currency.CHF)
  }

  test("money rejects amounts finer than the currency's minor unit") {
    Money.fromDecimal(BigDecimal("1.005"), Currency.EUR).isLeft shouldBe true
    Money.fromDecimal(BigDecimal("1.05"), Currency.EUR).value shouldBe eur(105)
  }

  test("overall net is the sum of the single offers of distinct categories") {
    val gen = for
      cats <- Gen.someOf(allCategories).map(_.toList)
      cents <- Gen.listOfN(cats.size, Gen.choose(0L, 10_000_000L))
    yield cats.zip(cents)
    forAll(gen) { pairs =>
      val offers = pairs.map((c, n) => offer(s"o-${c.id.value}", c, n))
      val t = Totals.compute(project(Nil, offers))
      t.net shouldBe eur(pairs.map(_._2).sum)
      t.categories.map(_.category) shouldBe allCategories
      t.missingCategories.toSet shouldBe allCategories.toSet -- pairs.map(_._1)
    }
  }

  test("sample project Dach: favorite, single offers and a Pauschalangebot are counted") {
    val t = Totals.compute(SampleData.project.value)
    t.net shouldBe eur(1_414_650 + 217_200 + 480_000)
    t.undecidedCategories.map(_.name.value) shouldBe List("Dämmung")
    t.missingCategories.map(_.name.value) shouldBe List("Dachfenster", "Sonstiges")
  }

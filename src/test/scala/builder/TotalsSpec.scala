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
    val a = offer("a", Category.Windows, 10000)
    val b = offer("b", Category.Windows, 20000)
    val t = Totals
      .compute(project(List(Category.Windows), List(a, b), Map(Category.Windows -> b.id)))
      .value
    statusOf(t, Category.Windows) shouldBe Some(
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
    val a = offer("a", Category.Roofing, 50000)
    val t = Totals.compute(project(Nil, List(a))).value
    statusOf(t, Category.Roofing) shouldBe Some(
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

  test("category with a work package but no offer is listed as missing and adds nothing") {
    val t = Totals
      .compute(
        project(List(Category.Plumbing, Category.Roofing), List(offer("a", Category.Roofing, 100)))
      )
      .value
    t.missingCategories shouldBe List(Category.Plumbing)
    statusOf(t, Category.Plumbing) shouldBe Some(CategoryStatus.NoOffer)
    t.net shouldBe eur(100)
    t.isComplete shouldBe false
  }

  test("several offers without favorite are reported, not silently picked") {
    val offers = List(
      offer("a", Category.Electrical, 100),
      offer("b", Category.Electrical, 200),
      offer("c", Category.Painting, 300)
    )
    val t = Totals.compute(project(Nil, offers)).value
    statusOf(t, Category.Electrical) shouldBe Some(
      CategoryStatus.NeedsDecision(List(offerId("a"), offerId("b")))
    )
    t.undecidedCategories shouldBe List(Category.Electrical)
    t.net shouldBe eur(300)
    t.isComplete shouldBe false
  }

  test("offer in a different currency than the project is an error") {
    val r = Totals.compute(
      project(
        Nil,
        List(offer("a", Category.Other, 100), offer("x", Category.Other, 100, Currency.CHF))
      )
    )
    r.left.value shouldBe TotalsError.CurrencyMismatch(offerId("x"), Currency.EUR, Currency.CHF)
  }

  test("favorite pointing to an offer outside its category is an error") {
    val a = offer("a", Category.Flooring, 100)
    val r = Totals.compute(project(Nil, List(a), Map(Category.Painting -> a.id)))
    r.left.value shouldBe TotalsError.UnknownFavorite(Category.Painting, a.id)
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
      cats <- Gen.someOf(Category.values.toList).map(_.toList)
      cents <- Gen.listOfN(cats.size, Gen.choose(0L, 10_000_000L))
    yield cats.zip(cents)
    forAll(gen) { pairs =>
      val offers = pairs.map((c, n) => offer(s"o-${c.ordinal}", c, n))
      val t = Totals.compute(project(Nil, offers)).value
      t.net shouldBe eur(pairs.map(_._2).sum)
      t.categories.map(_.category).toSet shouldBe pairs.map(_._1).toSet
    }
  }

package builder

import builder.domain.*
import builder.extraction.*
import builder.ranking.*
import builder.repo.*
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.Instant

/** Properties of [[Ranking.build]] over random matrices where every offer covers every Leistung. */
class RankingPropertiesSpec
    extends AnyFreeSpec
    with Matchers
    with EitherValues
    with ScalaCheckPropertyChecks:
  import RawOffers.*

  private val cat = Fixtures.category("dachfenster", "Dachfenster")

  private def column(i: Int): MatrixColumn =
    val raw = dachfenster.copy(contractor = f(s"Firma $i"))
    MatrixColumn(
      UploadView(
        Upload(
          Fixtures.offerId(s"o$i"),
          cat.id,
          s"o$i.pdf",
          OfferFileType.Pdf,
          Instant.EPOCH,
          UploadOutcome.Read(raw)
        ),
        UploadStatus.Accepted(raw, Fixtures.offer(s"o$i", cat, 100))
      ),
      raw
    )

  private def has(cents: Long) =
    Cell(
      Coverage.Enthalten,
      CoverageSource.FromItems,
      List(0),
      Some(Fixtures.eur(cents)),
      flagged = false
    )

  /** prices(offer)(leistung) in cents. */
  private def matrix(prices: List[List[Long]]): Matrix =
    val rows = prices.headOption.fold(0)(_.size)
    Matrix(
      cat.id,
      prices.indices.toList.map(column),
      (0 until rows).toList.map(j =>
        MatrixRow(
          Leistung(LeistungId.from(s"l$j").value, cat.id, LeistungName.from(s"L$j").value),
          prices.map(p => has(p(j)))
        )
      ),
      Nil,
      Nil,
      Nil
    )

  private def ratings(stars: List[Int]): Map[ContractorKey, Rating] =
    stars.zipWithIndex.flatMap { (s, i) =>
      ContractorKey.of(s"Firma $i").map(_ -> Rating(Stars.from(s).value, ""))
    }.toMap

  private val priceOnly = Weights.from(Map(Criterion.Preis -> 100, Criterion.Referenzen -> 0)).value

  private val offers: Gen[(List[List[Long]], List[Int])] =
    for
      n <- Gen.choose(1, 5)
      m <- Gen.choose(1, 4)
      prices <- Gen.listOfN(n, Gen.listOfN(m, Gen.choose(1L, 10000000L)))
      stars <- Gen.listOfN(n, Gen.choose(1, 5))
    yield (prices, stars)

  private def totals(r: Ranking): Map[String, Option[BigDecimal]] =
    (r.ranked ++ r.unranked).map(e => e.contractor -> e.total).toMap

  private def pricePoints(r: Ranking): Map[String, Option[BigDecimal]] =
    (r.ranked ++ r.unranked)
      .map(e =>
        e.contractor -> e.criteria.collectFirst {
          case CriterionScore(Criterion.Preis, _, CriterionResult.Scored(p, _)) => p
        }
      )
      .toMap

  "scaling all prices by the same factor changes no score" in {
    forAll(offers, Gen.choose(2L, 50L)) { case ((prices, stars), k) =>
      val before = Ranking.build(matrix(prices), ratings(stars), Weights.default, Currency.EUR)
      val after =
        Ranking.build(
          matrix(prices.map(_.map(_ * k))),
          ratings(stars),
          Weights.default,
          Currency.EUR
        )
      totals(after) shouldBe totals(before)
    }
  }

  "a higher price never earns more price points" in {
    forAll(offers, Gen.choose(1L, 1000000L)) { case ((prices, stars), delta) =>
      val dearer = prices.updated(0, prices.head.updated(0, prices.head.head + delta))
      val before =
        pricePoints(Ranking.build(matrix(prices), ratings(stars), priceOnly, Currency.EUR))
      val after =
        pricePoints(Ranking.build(matrix(dearer), ratings(stars), priceOnly, Currency.EUR))
      after("Firma 0").zip(before("Firma 0")).forall((a, b) => a <= b) shouldBe true
    }
  }

  "more stars never lower the total" in {
    forAll(offers) { case (prices, stars) =>
      val better = stars.updated(0, (stars.head + 1).min(5))
      val before =
        totals(Ranking.build(matrix(prices), ratings(stars), Weights.default, Currency.EUR))
      val after =
        totals(Ranking.build(matrix(prices), ratings(better), Weights.default, Currency.EUR))
      after("Firma 0").zip(before("Firma 0")).forall((a, b) => a >= b) shouldBe true
    }
  }

  "the order of the columns does not matter" in {
    forAll(offers) { case (prices, stars) =>
      val m = matrix(prices)
      val reversed =
        m.copy(columns = m.columns.reverse, rows = m.rows.map(r => r.copy(cells = r.cells.reverse)))
      totals(Ranking.build(reversed, ratings(stars), Weights.default, Currency.EUR)) shouldBe
        totals(Ranking.build(m, ratings(stars), Weights.default, Currency.EUR))
    }
  }

  "a single offer, or offers with equal prices, get 100 price points" in {
    forAll(Gen.choose(1, 5), Gen.listOfN(3, Gen.choose(1L, 10000000L))) { (n, row) =>
      val r = Ranking.build(matrix(List.fill(n)(row)), Map.empty, priceOnly, Currency.EUR)
      pricePoints(r).values.toSet shouldBe Set(Some(BigDecimal("100.0")))
    }
  }

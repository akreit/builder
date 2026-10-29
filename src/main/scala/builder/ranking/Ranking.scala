package builder.ranking

import builder.domain.*
import builder.repo.*

import scala.math.BigDecimal.RoundingMode

/** Price of an offer on the Leistungen all compared offers cover (`common`), so an offer is not
  * cheaper just because something is missing. `missing`: Leistungen other offers include and this
  * one does not (or without a readable price).
  */
final case class PriceBasis(comparable: Option[Money], missing: List[Leistung])

/** Points (0–100) of one criterion, or why there are none. */
enum CriterionResult:
  case Scored(points: BigDecimal, detail: String)
  case NotAvailable(reason: String)

final case class CriterionScore(criterion: Criterion, weight: Int, result: CriterionResult)

/** One offer with its score breakdown. `total` only if every weighted criterion has points; offers
  * without it are listed but not ranked.
  */
final case class RankedOffer(
    column: MatrixColumn,
    contractor: String,
    rating: Option[Rating],
    price: PriceBasis,
    criteria: List[CriterionScore],
    total: Option[BigDecimal]
)

final case class Ranking(
    common: List[Leistung],
    ranked: List[RankedOffer],
    unranked: List[RankedOffer]
)

object Ranking:
  private def round(d: BigDecimal): BigDecimal = d.setScale(1, RoundingMode.HALF_UP)

  private def included(c: Cell): Boolean = c.coverage match
    case Coverage.Enthalten                                                  => c.price.isDefined
    case Coverage.NichtEnthalten | Coverage.Ausgeschlossen | Coverage.Unklar => false

  private def sum(ms: List[Money], cur: Currency): Option[Money] = Money.sum(cur, ms).toOption

  def contractor(c: MatrixColumn): String =
    c.raw.contractor.value.map(_.trim).filter(_.nonEmpty).getOrElse(c.upload.upload.fileName)

  def build(
      matrix: Matrix,
      ratings: Map[ContractorKey, Rating],
      weights: Weights,
      currency: Currency
  ): Ranking =
    val common =
      matrix.rows.filter(r => r.cells.nonEmpty && r.cells.forall(included)).map(_.leistung)
    val commonIds = common.map(_.id).toSet
    val bases = matrix.columns.indices.toList.map { i =>
      val comparable =
        if common.isEmpty then None
        else
          sum(
            matrix.rows
              .filter(r => commonIds.contains(r.leistung.id))
              .flatMap(_.cells.lift(i))
              .flatMap(_.price),
            currency
          )
      val missing = matrix.rows.collect {
        case r if r.cells.lift(i).exists(c => !included(c)) && r.cells.exists(included) =>
          r.leistung
      }
      PriceBasis(comparable, missing)
    }
    val cheapest = bases.flatMap(_.comparable).minByOption(_.minor.value)
    val entries = matrix.columns.zip(bases).map { (col, basis) =>
      val name = contractor(col)
      val rating = ContractorKey.of(name).flatMap(ratings.get)
      val criteria = Criterion.values.toList.map { c =>
        val result = c match
          case Criterion.Preis =>
            (basis.comparable, cheapest) match
              case (Some(own), Some(min)) if own.minor.value > 0 =>
                CriterionResult.Scored(
                  round(BigDecimal(min.minor.value) * 100 / BigDecimal(own.minor.value)),
                  s"Vergleichspreis auf ${common.size} gemeinsamen Leistungen"
                )
              case (Some(_), Some(_)) | (None, Some(_)) | (Some(_), None) | (None, None) =>
                CriterionResult.NotAvailable(
                  if common.isEmpty then "keine Leistung, die alle Angebote mit Preis enthalten"
                  else "kein Vergleichspreis"
                )
          case Criterion.Referenzen =>
            rating.fold(CriterionResult.NotAvailable("nicht bewertet"))(r =>
              CriterionResult
                .Scored(BigDecimal(r.stars.value * 20), s"${r.stars.value} von 5 Sternen")
            )
        CriterionScore(c, weights.of(c), result)
      }
      val weighted = criteria.filter(_.weight > 0)
      val points = weighted.collect { case CriterionScore(_, w, CriterionResult.Scored(p, _)) =>
        p * w
      }
      val total =
        if points.size == weighted.size then Some(round(points.sum / weighted.map(_.weight).sum))
        else None
      RankedOffer(col, name, rating, basis, criteria, total)
    }
    val (ranked, unranked) = entries.partition(_.total.isDefined)
    Ranking(common, ranked.sortBy(e => (-e.total.getOrElse(BigDecimal(0)), e.contractor)), unranked)

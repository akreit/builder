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
    firm: Option[ContractorKey],
    comparable: Boolean,
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

  /** Display name: the recognised contractor, else the file name. */
  def contractor(c: MatrixColumn): String = firmName(c).getOrElse(c.upload.upload.fileName)

  /** The contractor as read from the offer; ratings are only ever keyed by this, never by file. */
  def firmName(c: MatrixColumn): Option[String] =
    c.raw.contractor.value.map(_.trim).filter(_.nonEmpty)

  val NotComparable: String = "Abweichungen – nicht vergleichbar"

  private def accepted(c: MatrixColumn): Boolean = c.upload.status match
    case UploadStatus.Accepted(_, _)                                    => true
    case UploadStatus.Rejected(_, _) | UploadStatus.ExtractionFailed(_) => false

  private def score(points: BigDecimal, detail: String) = CriterionResult.Scored(points, detail)

  def build(
      matrix: Matrix,
      ratings: Map[ContractorKey, Rating],
      weights: Weights,
      currency: Currency
  ): Ranking =
    // Only accepted offers are compared; rejected ones neither shape `common` nor get a rank.
    val (good, bad) = matrix.columns.zipWithIndex.partition((c, _) => accepted(c))
    val rows = matrix.rows.map(r => r.leistung -> good.flatMap((_, i) => r.cells.lift(i)))
    val common =
      rows.collect { case (l, cells) if cells.nonEmpty && cells.forall(included) => l }
    val commonIds = common.map(_.id).toSet
    val bases = good.indices.toList.map { j =>
      val comparable =
        if common.isEmpty then None
        else
          sum(
            rows
              .collect { case (l, cells) if commonIds.contains(l.id) => cells.lift(j) }
              .flatten
              .flatMap(_.price),
            currency
          )
      val missing = rows.collect {
        case (l, cells) if cells.lift(j).exists(c => !included(c)) && cells.exists(included) => l
      }
      PriceBasis(comparable, missing)
    }
    val cheapest = bases.flatMap(_.comparable).filter(_.minor.value > 0).minByOption(_.minor.value)
    def rated(col: MatrixColumn): (Option[ContractorKey], Option[Rating]) =
      val key = firmName(col).flatMap(ContractorKey.of)
      (key, key.flatMap(ratings.get))
    val entries = good.map(_._1).zip(bases).map { (col, basis) =>
      val (key, rating) = rated(col)
      val criteria = Criterion.values.toList.map { c =>
        val result = c match
          case Criterion.Preis =>
            (basis.comparable, cheapest) match
              case (Some(own), _) if own.minor.value <= 0 =>
                CriterionResult.NotAvailable("Vergleichspreis ≤ 0")
              case (Some(own), Some(min)) =>
                score(
                  round(BigDecimal(min.minor.value) * 100 / BigDecimal(own.minor.value)),
                  s"Vergleichspreis auf ${common.size} gemeinsamen Leistungen"
                )
              case (None, Some(_)) | (Some(_), None) | (None, None) =>
                CriterionResult.NotAvailable(
                  if common.isEmpty then "keine Leistung, die alle Angebote mit Preis enthalten"
                  else "kein Vergleichspreis"
                )
          case Criterion.Referenzen =>
            (key, rating) match
              case (None, _)          => CriterionResult.NotAvailable("Firma nicht erkannt")
              case (Some(_), None)    => CriterionResult.NotAvailable("nicht bewertet")
              case (Some(_), Some(r)) =>
                score(BigDecimal(r.stars.value * 20), s"${r.stars.value} von 5 Sternen")
        CriterionScore(c, weights.of(c), result)
      }
      val weighted = criteria.filter(_.weight > 0)
      val points = weighted.collect { case CriterionScore(_, w, CriterionResult.Scored(p, _)) =>
        p * w
      }
      val total =
        if weighted.nonEmpty && points.size == weighted.size then
          Some(round(points.sum / weighted.map(_.weight).sum))
        else None
      RankedOffer(col, contractor(col), key, true, rating, basis, criteria, total)
    }
    val excluded = bad.map { (col, _) =>
      val (key, rating) = rated(col)
      RankedOffer(
        col,
        contractor(col),
        key,
        false,
        rating,
        PriceBasis(None, Nil),
        Criterion.values.toList.map(c =>
          CriterionScore(c, weights.of(c), CriterionResult.NotAvailable(NotComparable))
        ),
        None
      )
    }
    val (ranked, unranked) = entries.partition(_.total.isDefined)
    Ranking(
      common,
      ranked.sortBy(e => (-e.total.getOrElse(BigDecimal(0)), e.contractor)),
      unranked ++ excluded
    )

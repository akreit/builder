package builder.ranking

import builder.domain.*
import builder.domain.OfferLabel
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

/** Weighted result of one ranking criterion for one offer. */
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

/** Ranking of a category matrix: shared Leistungen (`common`), scored offers, and those without a
  * total (including rejected uploads).
  */
final case class Ranking(
    common: List[Leistung],
    ranked: List[RankedOffer],
    unranked: List[RankedOffer]
)

/** Builds a category ranking from the Leistungen matrix, firm ratings and criterion weights. */
object Ranking:
  private def round(d: BigDecimal): BigDecimal = d.setScale(1, RoundingMode.HALF_UP)

  private def included(c: Cell): Boolean = c.coverage match
    case Coverage.Enthalten                                                  => c.price.isDefined
    case Coverage.NichtEnthalten | Coverage.Ausgeschlossen | Coverage.Unklar => false

  private def sum(ms: List[Money], cur: Currency): Option[Money] = Money.sum(cur, ms).toOption

  /** Display name: the recognised contractor, else the file name. */
  def contractor(c: MatrixColumn): String =
    val label = OfferLabel.display(c.raw)
    if label.nonEmpty then label else c.upload.upload.fileName

  /** The contractor as read from the offer; ratings are only ever keyed by this, never by file. */
  def firmName(c: MatrixColumn): Option[String] =
    c.raw.contractor.value.map(_.trim).filter(_.nonEmpty)

  val NotComparable: String = "Abweichungen – nicht vergleichbar"

  private def accepted(c: MatrixColumn): Boolean = c.upload.status match
    case UploadStatus.Accepted(_, _)                                    => true
    case UploadStatus.Rejected(_, _) | UploadStatus.ExtractionFailed(_) => false

  private def score(points: BigDecimal, detail: String) = CriterionResult.Scored(points, detail)

  /** Cells of accepted columns only, per Leistung, in the order of `good`. */
  private def rowsForAccepted(
      matrix: Matrix,
      good: List[(MatrixColumn, Int)]
  ): List[(Leistung, List[Cell])] =
    matrix.rows.map(r => r.leistung -> good.flatMap((_, i) => r.cells.lift(i)))

  /** Leistungen every accepted offer covers with a readable included price. */
  private def commonLeistungen(rows: List[(Leistung, List[Cell])]): List[Leistung] =
    rows.collect { case (l, cells) if cells.nonEmpty && cells.forall(included) => l }

  /** Comparable price sum on `common` plus Leistungen this column lacks relative to others. */
  private def priceBasis(
      rows: List[(Leistung, List[Cell])],
      common: List[Leistung],
      columnIndex: Int,
      currency: Currency
  ): PriceBasis =
    val commonIds = common.map(_.id).toSet
    val comparable =
      if common.isEmpty then None
      else
        sum(
          rows
            .collect { case (l, cells) if commonIds.contains(l.id) => cells.lift(columnIndex) }
            .flatten
            .flatMap(_.price),
          currency
        )
    val missing = rows.collect {
      case (l, cells)
          if cells.lift(columnIndex).exists(c => !included(c)) && cells.exists(included) =>
        l
    }
    PriceBasis(comparable, missing)

  private def rated(
      col: MatrixColumn,
      ratings: Map[ContractorKey, Rating]
  ): (Option[ContractorKey], Option[Rating]) =
    val key = firmName(col).flatMap(ContractorKey.of)
    (key, key.flatMap(ratings.get))

  /** Points for one criterion given this offer's price basis and firm rating. */
  private def criterionResult(
      c: Criterion,
      basis: PriceBasis,
      commonSize: Int,
      cheapest: Option[Money],
      key: Option[ContractorKey],
      rating: Option[Rating]
  ): CriterionResult =
    c match
      case Criterion.Preis =>
        (basis.comparable, cheapest) match
          case (Some(own), _) if own.minor.value <= 0 =>
            CriterionResult.NotAvailable("Vergleichspreis ≤ 0")
          case (Some(own), Some(min)) =>
            score(
              round(BigDecimal(min.minor.value) * 100 / BigDecimal(own.minor.value)),
              s"Vergleichspreis auf $commonSize gemeinsamen Leistungen"
            )
          case (None, Some(_)) | (Some(_), None) | (None, None) =>
            CriterionResult.NotAvailable(
              if commonSize == 0 then "keine Leistung, die alle Angebote mit Preis enthalten"
              else "kein Vergleichspreis"
            )
      case Criterion.Referenzen =>
        (key, rating) match
          case (None, _)          => CriterionResult.NotAvailable("Firma nicht erkannt")
          case (Some(_), None)    => CriterionResult.NotAvailable("nicht bewertet")
          case (Some(_), Some(r)) =>
            score(BigDecimal(r.stars.value * 20), s"${r.stars.value} von 5 Sternen")

  /** Weighted total when every positive-weight criterion scored; otherwise `None`. */
  private def weightedTotal(criteria: List[CriterionScore]): Option[BigDecimal] =
    val weighted = criteria.filter(_.weight > 0)
    val points = weighted.collect { case CriterionScore(_, w, CriterionResult.Scored(p, _)) =>
      p * w
    }
    if weighted.nonEmpty && points.size == weighted.size then
      Some(round(points.sum / weighted.map(_.weight).sum))
    else None

  /** Scores one accepted offer against `common` and the cheapest comparable price. */
  private def scoreAccepted(
      col: MatrixColumn,
      basis: PriceBasis,
      common: List[Leistung],
      cheapest: Option[Money],
      ratings: Map[ContractorKey, Rating],
      weights: Weights
  ): RankedOffer =
    val (key, rating) = rated(col, ratings)
    val criteria = Criterion.values.toList.map { c =>
      CriterionScore(
        c,
        weights.of(c),
        criterionResult(c, basis, common.size, cheapest, key, rating)
      )
    }
    RankedOffer(
      col,
      contractor(col),
      key,
      true,
      rating,
      basis,
      criteria,
      weightedTotal(criteria)
    )

  /** Rejected or failed uploads: listed but never ranked or used for `common`. */
  private def excludedOffer(
      col: MatrixColumn,
      ratings: Map[ContractorKey, Rating],
      weights: Weights
  ): RankedOffer =
    val (key, rating) = rated(col, ratings)
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

  /** Ranking for one category matrix: shared priced Leistungen, scored offers, rest unranked. */
  def build(
      matrix: Matrix,
      ratings: Map[ContractorKey, Rating],
      weights: Weights,
      currency: Currency
  ): Ranking =
    // Only accepted offers are compared; rejected ones neither shape `common` nor get a rank.
    val (good, bad) = matrix.columns.zipWithIndex.partition((c, _) => accepted(c))
    val rows = rowsForAccepted(matrix, good)
    val common = commonLeistungen(rows)
    val bases = good.indices.toList.map(j => priceBasis(rows, common, j, currency))
    val cheapest = bases.flatMap(_.comparable).filter(_.minor.value > 0).minByOption(_.minor.value)
    val entries = good.map(_._1).zip(bases).map { (col, basis) =>
      scoreAccepted(col, basis, common, cheapest, ratings, weights)
    }
    val excluded = bad.map((col, _) => excludedOffer(col, ratings, weights))
    val (ranked, unranked) = entries.partition(_.total.isDefined)
    Ranking(
      common,
      ranked.sortBy(e => (-e.total.getOrElse(BigDecimal(0)), e.contractor)),
      unranked ++ excluded
    )

package builder.repo

import builder.domain.*
import builder.extraction.Field
import builder.extraction.LeistungPlan
import builder.extraction.RawOffer

/** Which Leistung a line item of an offer belongs to; `manual` if the user assigned it. */
final case class ItemAssignment(
    offer: OfferId,
    item: Int,
    leistung: Option[LeistungId],
    manual: Boolean
)

/** A stored coverage status; `source` is [[CoverageSource.Read]] or [[CoverageSource.Manual]]. */
final case class CoverageEntry(
    offer: OfferId,
    leistung: LeistungId,
    coverage: Coverage,
    source: CoverageSource
)

/** One cell: status, where it comes from, summed price of the mapped items, and whether it needs a
  * look (unclear, low confidence, items contradicting the status, unreadable prices).
  */
final case class Cell(
    coverage: Coverage,
    source: CoverageSource,
    items: List[Int],
    price: Option[Money],
    flagged: Boolean
)

/** One offer column of the matrix: upload view plus the raw extraction used for cells. */
final case class MatrixColumn(upload: UploadView, raw: RawOffer)

/** One Leistung row: the Leistung and one cell per matrix column. */
final case class MatrixRow(leistung: Leistung, cells: List[Cell])

/** A line item without a Leistung. */
final case class Unassigned(offer: OfferId, item: Int, description: String)

/** Check sum of one column: cell prices plus unassigned items, compared with the offer's net.
  * `None` where a price is unreadable / missing.
  */
final case class ColumnTotal(cells: Option[Money], unassigned: Option[Money], net: Option[Money]):
  def sum: Option[Money] =
    (cells, unassigned) match
      case (Some(c), Some(u))                               => c.plus(u).toOption
      case (Some(_), None) | (None, Some(_)) | (None, None) => None

  /** True only when every price in the column is known and that sum differs from net. Missing
    * prices are not a mismatch (no footer hint).
    */
  def mismatch: Boolean = (sum, net) match
    case (Some(s), Some(n))                               => s != n
    case (None, None) | (None, Some(_)) | (Some(_), None) => false

/** Leistungen (rows) × read offers (columns) of one category; `totals` and `dropped` (proposals cut
  * by [[builder.extraction.LeistungPlan.MaxNew]]) are per column.
  */
final case class Matrix(
    category: CategoryId,
    columns: List[MatrixColumn],
    rows: List[MatrixRow],
    unassigned: List[Unassigned],
    totals: List[ColumnTotal],
    dropped: List[Int]
)

/** Builds and interprets the category Leistungen matrix (cells, unassigned, column totals). */
object Matrix:
  private def raw(u: UploadView): Option[RawOffer] = u.status match
    case UploadStatus.Accepted(r, _)      => Some(r)
    case UploadStatus.Rejected(r, _)      => Some(r)
    case UploadStatus.ExtractionFailed(_) => None

  /** Effective assignment per (offer, item): a manual one wins over the extracted one. */
  def effective(assignments: List[ItemAssignment]): Map[(OfferId, Int), Option[LeistungId]] =
    assignments
      .groupBy(a => (a.offer, a.item))
      .view
      .mapValues(as => as.find(_.manual).orElse(as.headOption).flatMap(_.leistung))
      .toMap

  /** Effective coverage per (offer, Leistung): a manual entry wins over the read one. */
  def effectiveCoverage(entries: List[CoverageEntry]): Map[(OfferId, LeistungId), CoverageEntry] =
    entries
      .groupBy(e => (e.offer, e.leistung))
      .view
      .mapValues(es =>
        es.find(e =>
          e.source match
            case CoverageSource.Manual => true
            case CoverageSource.Read(_, _) | CoverageSource.FromItems | CoverageSource.Unknown =>
              false
        ).orElse(es.headOption)
      )
      .collect { case (k, Some(e)) => k -> e }
      .toMap

  def cell(
      read: Option[CoverageEntry],
      items: List[Int],
      prices: List[Option[Money]],
      currency: Currency
  ): Cell =
    val price =
      if items.isEmpty || prices.exists(_.isEmpty) then None
      else Money.sum(currency, prices.flatten).toOption
    val pricesUnreadable = items.nonEmpty && price.isEmpty
    val conflict = (c: Coverage) =>
      c match
        case Coverage.Enthalten                                                  => false
        case Coverage.NichtEnthalten | Coverage.Ausgeschlossen | Coverage.Unklar => items.nonEmpty
    read match
      case Some(CoverageEntry(_, _, c, CoverageSource.Manual)) =>
        Cell(c, CoverageSource.Manual, items, price, flagged = false)
      case Some(CoverageEntry(_, _, c, s @ CoverageSource.Read(_, conf))) =>
        val unclear = c match
          case Coverage.Unklar                                                        => true
          case Coverage.Enthalten | Coverage.NichtEnthalten | Coverage.Ausgeschlossen => false
        val low = conf match
          case Confidence.Low                      => true
          case Confidence.High | Confidence.Medium => false
        Cell(c, s, items, price, unclear || low || conflict(c) || pricesUnreadable)
      case Some(CoverageEntry(_, _, _, CoverageSource.FromItems | CoverageSource.Unknown)) | None =>
        if items.nonEmpty then
          Cell(Coverage.Enthalten, CoverageSource.FromItems, items, price, pricesUnreadable)
        else Cell(Coverage.Unklar, CoverageSource.Unknown, items, None, flagged = true)

  /** Line index → assigned Leistung (if any) for every position of `col`. */
  private def itemsOf(
      col: MatrixColumn,
      assigned: Map[(OfferId, Int), Option[LeistungId]]
  ): List[(Int, Option[LeistungId])] =
    col.raw.lineItems.indices.toList.map(i => i -> assigned.get((col.upload.upload.id, i)).flatten)

  /** Native uploads of `category` plus foreign uploads that map at least one item here. */
  private def columnsFor(
      view: ProjectView,
      category: CategoryId,
      assigned: Map[(OfferId, Int), Option[LeistungId]],
      known: Set[LeistungId]
  ): List[MatrixColumn] =
    val nativeColumns = view.uploads
      .filter(_.upload.category == category)
      .flatMap(u => raw(u).map(MatrixColumn(u, _)))
    val nativeIds = nativeColumns.map(_.upload.upload.id).toSet
    val foreignColumns = view.uploads
      .filterNot(u => nativeIds.contains(u.upload.id))
      .flatMap(u => raw(u).map(MatrixColumn(u, _)))
      .filter(col => itemsOf(col, assigned).exists { case (_, l) => l.exists(known.contains) })
    nativeColumns ++ foreignColumns

  /** One matrix row per Leistung: cells filled from assignments and coverage. */
  private def buildRows(
      leistungen: List[Leistung],
      columns: List[MatrixColumn],
      assigned: Map[(OfferId, Int), Option[LeistungId]],
      coverage: Map[(OfferId, LeistungId), CoverageEntry],
      currency: Currency
  ): List[MatrixRow] =
    leistungen.map(l =>
      MatrixRow(
        l,
        columns.map { col =>
          val items = itemsOf(col, assigned).collect { case (i, Some(id)) if id == l.id => i }
          val prices =
            items.map(i => col.raw.lineItems.lift(i).flatMap(li => amount(li.total, currency)))
          cell(coverage.get((col.upload.upload.id, l.id)), items, prices, currency)
        }
      )
    )

  /** Unmapped positions of native-category offers only (cross-category targets leave this list). */
  private def unassignedOf(
      nativeColumns: List[MatrixColumn],
      assigned: Map[(OfferId, Int), Option[LeistungId]]
  ): List[Unassigned] =
    nativeColumns.flatMap(col =>
      itemsOf(col, assigned).collect { case (i, None) =>
        Unassigned(
          col.upload.upload.id,
          i,
          col.raw.lineItems.lift(i).flatMap(_.description.value).getOrElse(s"Position ${i + 1}")
        )
      }
    )

  private def sumPrices(currency: Currency, prices: List[Option[Money]]): Option[Money] =
    if prices.exists(_.isEmpty) then None else Money.sum(currency, prices.flatten).toOption

  /** Per-column check sums; foreign columns omit offer-net comparison. */
  private def columnTotals(
      category: CategoryId,
      columns: List[MatrixColumn],
      rows: List[MatrixRow],
      unassigned: List[Unassigned],
      assigned: Map[(OfferId, Int), Option[LeistungId]],
      known: Set[LeistungId],
      currency: Currency
  ): List[ColumnTotal] =
    columns.zipWithIndex.map { (col, j) =>
      val native = col.upload.upload.category == category
      val cellSum =
        sumPrices(currency, rows.flatMap(_.cells.lift(j)).filter(_.items.nonEmpty).map(_.price))
      if native then
        val exported = itemsOf(col, assigned).collect {
          case (i, Some(id)) if !known.contains(id) =>
            col.raw.lineItems.lift(i).flatMap(li => amount(li.total, currency))
        }
        val localUnassigned = unassigned
          .filter(_.offer == col.upload.upload.id)
          .map(u => col.raw.lineItems.lift(u.item).flatMap(li => amount(li.total, currency)))
        ColumnTotal(
          cellSum,
          sumPrices(currency, localUnassigned ++ exported),
          amount(col.raw.net, currency)
        )
      else ColumnTotal(cellSum, Some(Money.zero(currency)), None)
    }

  /** Builds the Leistungen × offers matrix for one category, including cross-category assignments.
    */
  def build(view: ProjectView, category: CategoryId): Matrix =
    val currency = view.project.currency
    val leistungen = view.leistungen.filter(_.category == category)
    val known = leistungen.map(_.id).toSet
    val assigned = effective(view.assignments)
    val coverage = effectiveCoverage(view.coverage)
    val columns = columnsFor(view, category, assigned, known)
    val nativeColumns = columns.filter(_.upload.upload.category == category)
    val rows = buildRows(leistungen, columns, assigned, coverage, currency)
    val unassigned = unassignedOf(nativeColumns, assigned)
    Matrix(
      category,
      columns,
      rows,
      unassigned,
      columnTotals(category, columns, rows, unassigned, assigned, known, currency),
      columns.map(c => LeistungPlan.dropped(c.raw))
    )

  /** A price field as money; `None` if absent, unparseable or too precise. */
  def amount(f: Field, currency: Currency): Option[Money] =
    f.text
      .flatMap(v => scala.util.Try(BigDecimal(v.replace(" ", ""))).toOption)
      .flatMap(d => Money.fromDecimal(d, currency).toOption)

package builder.extraction

import builder.domain.Confidence

/** Merges the partial answers of several passes (in page order) into one [[RawOffer]]:
  *
  *   - contractor, date, validUntil: first non-empty value; a different value in a later part is
  *     kept as a low-confidence note, so conflicts stay visible.
  *   - line items: concatenated in page order; where a page was cut into pieces of two adjacent
  *     parts, an item identical (all fields and page) to one of the previous part on that shared
  *     page is a repeat and kept once. Equal items on different pages are kept.
  *   - notes, exclusions: concatenated in page order (identical ones once).
  *   - net, VAT rate, VAT, gross: last non-empty value (totals are printed at the end); a different
  *     value in an earlier part is kept as a low-confidence note.
  *   - new Leistungen: first proposal per key; coverage: per Leistung the most specific status.
  */
object RawOfferMerge:
  def merge(parts: List[(TextChunk, RawOffer)]): RawOffer =
    parts match
      case (_, only) :: Nil => only
      case _                =>
        val offers = parts.map(_._2)
        RawOffer(
          contractor = first(offers.map(_.contractor)),
          date = first(offers.map(_.date)),
          validUntil = first(offers.map(_.validUntil)),
          lineItems = lineItems(parts),
          net = last(offers.map(_.net)),
          vatRate = last(offers.map(_.vatRate)),
          vat = last(offers.map(_.vat)),
          gross = last(offers.map(_.gross)),
          notes = (offers.flatMap(_.notes) ++ conflicts(parts)).distinct,
          exclusions = offers.flatMap(_.exclusions).distinct,
          neueLeistungen =
            optional(offers.flatMap(_.neueLeistungen.toList.flatten).distinctBy(_.key)),
          abdeckung = optional(coverage(offers.flatMap(_.abdeckung.toList.flatten)))
        )

  private val absent: Field = Field(None, None, Confidence.Low)

  private def first(fields: List[Field]): Field = fields.find(_.text.isDefined).getOrElse(absent)
  private def last(fields: List[Field]): Field = first(fields.reverse)

  private def optional[A](as: List[A]): Option[List[A]] = Some(as).filter(_.nonEmpty)

  /** Which value of several parts is used. */
  private enum Pick:
    case First, Last

  private def conflicts(parts: List[(TextChunk, RawOffer)]): List[Field] =
    List[(String, Pick, RawOffer => Field)](
      ("Firma", Pick.First, _.contractor),
      ("Angebotsdatum", Pick.First, _.date),
      ("Gültig bis", Pick.First, _.validUntil),
      ("Netto", Pick.Last, _.net),
      ("MwSt.-Satz", Pick.Last, _.vatRate),
      ("MwSt.", Pick.Last, _.vat),
      ("Brutto", Pick.Last, _.gross)
    ).flatMap((label, pick, get) => conflictsOf(label, pick, parts.map((c, o) => (c, get(o)))))

  private def picked(pick: Pick, fields: List[Field]): Field = pick match
    case Pick.First => first(fields)
    case Pick.Last  => last(fields)

  private def conflictsOf(
      label: String,
      pick: Pick,
      fields: List[(TextChunk, Field)]
  ): List[Field] =
    picked(pick, fields.map(_._2)).text.toList.flatMap(chosen =>
      fields.collect {
        case (chunk, f) if f.text.exists(_ != chosen) =>
          Field(
            f.text.map(t =>
              s"Abweichende Angabe $label in ${chunk.label}: $t (verwendet: $chosen)"
            ),
            f.page,
            Confidence.Low
          )
      }
    )

  /** Most specific first: what a part says explicitly beats "unklar" from another part. */
  private val statusOrder: List[String] =
    List("enthalten", "ausgeschlossen", "nicht_enthalten", "unklar")

  private def rank(status: String): Int =
    Some(statusOrder.indexOf(status)).filter(_ >= 0).getOrElse(statusOrder.size)

  private def coverage(entries: List[RawAbdeckung]): List[RawAbdeckung] =
    entries
      .groupBy(_.leistung)
      .values
      .flatMap(_.minByOption(a => rank(a.status)))
      .toList
      .sortBy(a => entries.indexWhere(_.leistung == a.leistung))

  /** Identity of a line item for repeat detection: every field's text and page. */
  private def itemKey(item: RawLineItem): List[(Option[String], Option[Int])] =
    item.labelled.map((_, f) => (f.text, f.page)) :+ ((item.leistung, None))

  private def pagesOf(chunk: TextChunk): Set[Int] = chunk.pages.map(_.number).toSet

  /** Line items in page order, dropping repeats on pages shared with the previous part. */
  private def lineItems(parts: List[(TextChunk, RawOffer)]): List[RawLineItem] =
    parts.headOption.toList.flatMap(_._2.lineItems) ++
      parts.zip(parts.drop(1)).flatMap { case ((prevChunk, prev), (chunk, offer)) =>
        val shared = pagesOf(prevChunk).intersect(pagesOf(chunk))
        val seen = prev.lineItems
          .filter(_.description.page.exists(shared.contains))
          .map(itemKey)
          .toSet
        offer.lineItems.filterNot(li =>
          li.description.page.exists(shared.contains) && seen.contains(itemKey(li))
        )
      }

package builder.extraction

import builder.domain.Confidence

/** Merges the partial answers of several passes (in page order) into one [[RawOffer]]:
  *
  *   - contractor, date, validUntil: first non-empty value; a different value in a later part is
  *     kept as a low-confidence note, so conflicts stay visible.
  *   - line items, notes, exclusions: concatenated in page order (identical notes once).
  *   - net, VAT rate, VAT, gross: last non-empty value (totals are printed at the end).
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
          lineItems = offers.flatMap(_.lineItems),
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

  private def conflicts(parts: List[(TextChunk, RawOffer)]): List[Field] =
    List[(String, RawOffer => Field)](
      "Firma" -> (_.contractor),
      "Angebotsdatum" -> (_.date),
      "Gültig bis" -> (_.validUntil)
    ).flatMap((label, get) => conflictsOf(label, parts.map((c, o) => (c, get(o)))))

  private def conflictsOf(label: String, fields: List[(TextChunk, Field)]): List[Field] =
    first(fields.map(_._2)).text.toList.flatMap(chosen =>
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

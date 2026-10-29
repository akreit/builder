package builder.extraction

import builder.domain.Confidence
import builder.domain.PageNumber
import builder.extraction.Field.given
import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder

/** One value as read from an offer document: the text the extractor saw (if any), the 1-based page
  * it is printed on, and how sure the extractor is. Nothing here is trusted yet; [[OfferDraft]]
  * turns it into a validated [[builder.domain.Offer]].
  */
final case class Field(value: Option[String], page: Option[Int], confidence: Confidence):
  def text: Option[String] = value.map(_.trim).filter(_.nonEmpty)

  /** The source page; absent, 0 or negative means the document does not say. */
  def pageNumber: Option[PageNumber] = page.flatMap(PageNumber.from(_).toOption)
  def isLowConfidence: Boolean = confidence match
    case Confidence.Low                      => true
    case Confidence.Medium | Confidence.High => false

object Field:
  private def wire(c: Confidence): String = c.toString.toLowerCase

  given Encoder[Confidence] = Encoder.encodeString.contramap(wire)
  given Decoder[Confidence] = Decoder.decodeString.emap(s =>
    Confidence.values.find(c => wire(c) == s.trim.toLowerCase).toRight(s"unknown confidence: $s")
  )
  given Codec.AsObject[Field] = Codec.AsObject.derived

final case class RawLineItem(
    description: Field,
    quantity: Field,
    unit: Field,
    sizes: Field,
    manufacturer: Field,
    model: Field,
    unitPrice: Field,
    total: Field,
    /** Id of an existing Leistung or key of a proposed new one ([[RawNeueLeistung.key]]). */
    leistung: Option[String] = None
) derives Codec.AsObject:
  /** (German label, field) pairs, in display order. */
  def labelled: List[(String, Field)] = List(
    "Beschreibung" -> description,
    "Menge" -> quantity,
    "Einheit" -> unit,
    "Maße" -> sizes,
    "Hersteller" -> manufacturer,
    "Modell" -> model,
    "Einzelpreis" -> unitPrice,
    "Gesamtpreis" -> total
  )

/** A coarse Leistung the extractor proposes for the category; `key` is how line items and coverage
  * entries of this answer refer to it.
  */
final case class RawNeueLeistung(key: String, name: String) derives Codec.AsObject

/** Whether the offer covers a Leistung (existing id or new key). `status` is a
  * [[builder.domain.Coverage]] code; anything else is read as "unklar".
  */
final case class RawAbdeckung(leistung: String, status: String, page: Int, confidence: Confidence)
    derives Codec.AsObject

/** Everything the extractor read from one offer document. Amounts, quantities and the VAT rate are
  * decimal strings ("1234.50"), dates ISO strings ("2026-10-01").
  */
final case class RawOffer(
    contractor: Field,
    date: Field,
    validUntil: Field,
    lineItems: List[RawLineItem],
    net: Field,
    vatRate: Field,
    vat: Field,
    gross: Field,
    notes: List[Field],
    exclusions: List[Field],
    neueLeistungen: Option[List[RawNeueLeistung]] = None,
    abdeckung: Option[List[RawAbdeckung]] = None
) derives Codec.AsObject:
  /** Offer-level (German label, field) pairs, in display order. */
  def labelled: List[(String, Field)] = List(
    "Firma" -> contractor,
    "Angebotsdatum" -> date,
    "Gültig bis" -> validUntil,
    "Netto" -> net,
    "MwSt.-Satz" -> vatRate,
    "MwSt." -> vat,
    "Brutto" -> gross
  )

  /** German labels of every field the extractor was unsure about. */
  def lowConfidenceFields: List[String] =
    labelled.collect { case (l, f) if f.isLowConfidence => l } ++
      lineItems.zipWithIndex.flatMap((li, i) =>
        li.labelled.collect { case (l, f) if f.isLowConfidence => s"Position ${i + 1}: $l" }
      ) ++
      notes.filter(_.isLowConfidence).flatMap(_.text).map(n => s"Hinweis: $n") ++
      exclusions.filter(_.isLowConfidence).flatMap(_.text).map(n => s"Ausschluss: $n")

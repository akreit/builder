package builder.domain

import builder.extraction.Field
import builder.extraction.RawOffer

/** Normalised addressee (Angebotsadressat) for replace identity. Absent/blank recipients share one
  * sentinel so they only match other unknowns — never invent a name and never collide with a known
  * recipient.
  */
opaque type RecipientKey = String

/** Construction and sentinel for [[RecipientKey]]. */
object RecipientKey:
  /** Sentinel for missing recipient text. */
  val Unknown: RecipientKey = "\u0000unknown"

  /** Normalises recipient text; blank becomes [[Unknown]]. */
  def of(name: String): RecipientKey =
    Option(name.trim.toLowerCase.replaceAll("\\s+", " ")).filter(_.nonEmpty).getOrElse(Unknown)

  /** Recipient key from an optional extractor field. */
  def fromField(field: Option[Field]): RecipientKey =
    field.flatMap(_.text).map(of).getOrElse(Unknown)

  /** Recipient key from a single extractor field. */
  def fromField(field: Field): RecipientKey = fromField(Some(field))

  extension (k: RecipientKey)
    /** Underlying normalised string of this key. */
    def value: String = k

    /** True for the missing-recipient sentinel. */
    def isUnknown: Boolean = k == Unknown

/** Replace / upsert identity within a project: firm + recipient + category. */
final case class OfferReplaceKey(firm: ContractorKey, recipient: RecipientKey)

/** UI label: original contractor stays on the offer; this builds the compact display form
  * `Unternehmen_Adressat` when a recipient was extracted, otherwise the contractor name unchanged.
  */
object OfferLabel:
  /** Drop whitespace for the compact UI token (e.g. ZimmereiMüller_Andreas). */
  def compact(s: String): String = s.trim.replaceAll("\\s+", "")

  /** Compact UI label `Firma_Adressat`, or the contractor alone when no recipient. */
  def display(contractor: String, recipient: Option[String]): String =
    recipient.map(_.trim).filter(_.nonEmpty) match
      case Some(r) => s"${compact(contractor)}_${compact(r)}"
      case None    => contractor

  /** Label derived from a [[builder.extraction.RawOffer]]. */
  def display(raw: RawOffer): String =
    display(raw.contractor.text.getOrElse(""), raw.recipient.flatMap(_.text))

  /** Label derived from a validated [[Offer]]. */
  def display(offer: Offer): String =
    display(offer.contractor.value.name, offer.recipient.map(_.value))

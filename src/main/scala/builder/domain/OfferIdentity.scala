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

  def of(name: String): RecipientKey =
    Option(name.trim.toLowerCase.replaceAll("\\s+", " ")).filter(_.nonEmpty).getOrElse(Unknown)

  def fromField(field: Option[Field]): RecipientKey =
    field.flatMap(_.text).map(of).getOrElse(Unknown)

  def fromField(field: Field): RecipientKey = fromField(Some(field))

  extension (k: RecipientKey)
    def value: String = k
    def isUnknown: Boolean = k == Unknown

/** Replace / upsert identity within a project: firm + recipient + category. */
final case class OfferReplaceKey(firm: ContractorKey, recipient: RecipientKey)

/** UI label: original contractor stays on the offer; this builds the compact display form
  * `Unternehmen_Adressat` when a recipient was extracted, otherwise the contractor name unchanged.
  */
object OfferLabel:
  /** Drop whitespace for the compact UI token (e.g. ZimmereiMüller_Andreas). */
  def compact(s: String): String = s.trim.replaceAll("\\s+", "")

  def display(contractor: String, recipient: Option[String]): String =
    recipient.map(_.trim).filter(_.nonEmpty) match
      case Some(r) => s"${compact(contractor)}_${compact(r)}"
      case None    => contractor

  def display(raw: RawOffer): String =
    display(raw.contractor.text.getOrElse(""), raw.recipient.flatMap(_.text))

  def display(offer: Offer): String =
    display(offer.contractor.value.name, offer.recipient.map(_.value))

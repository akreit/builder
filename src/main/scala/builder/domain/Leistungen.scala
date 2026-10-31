package builder.domain

/** Id of a Leistung (a coarse work step of a category, e.g. "Gerüst" or "Neu decken"). */
opaque type LeistungId = String
object LeistungId:
  def from(raw: String): Either[DomainError, LeistungId] = validId("leistung", raw)
  extension (id: LeistungId) def value: String = id

/** Display name of a Leistung, trimmed and non-empty. */
opaque type LeistungName = String
object LeistungName:
  def from(raw: String): Either[DomainError, LeistungName] =
    val trimmed = raw.trim
    Either.cond(
      trimmed.nonEmpty && trimmed.length <= 80,
      trimmed,
      DomainError.InvalidLeistungName(raw)
    )
  extension (n: LeistungName) def value: String = n

/** A coarse work step of a category that offers are compared on. */
final case class Leistung(id: LeistungId, category: CategoryId, name: LeistungName)

/** Whether an offer covers a Leistung. */
enum Coverage:
  case Enthalten
  case NichtEnthalten

  /** The offer explicitly excludes it ("nicht im Angebot enthalten: …"). */
  case Ausgeschlossen
  case Unklar

object Coverage:
  /** Wire names used in the extractor JSON, the database and form values. */
  def code(c: Coverage): String = c match
    case Enthalten      => "enthalten"
    case NichtEnthalten => "nicht_enthalten"
    case Ausgeschlossen => "ausgeschlossen"
    case Unklar         => "unklar"

  def fromCode(s: String): Option[Coverage] = values.find(code(_) == s.trim.toLowerCase)

/** Where a coverage status comes from. */
enum CoverageSource:
  /** Stated by the extractor, with the page it saw it on. */
  case Read(page: Option[PageNumber], confidence: Confidence)

  /** Follows from line items mapped to the Leistung. */
  case FromItems

  /** Set by the user; never overwritten by an extraction. */
  case Manual

  /** Nothing known: shown as "unklar", never guessed. */
  case Unknown

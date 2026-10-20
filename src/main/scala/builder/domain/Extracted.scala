package builder.domain

/** 1-based page number in a source document. */
opaque type PageNumber = Int

object PageNumber:
  def from(n: Int): Either[DomainError, PageNumber] =
    Either.cond(n >= 1, n, DomainError.InvalidPageNumber(n))
  extension (p: PageNumber) def value: Int = p

enum Confidence:
  case High, Medium, Low

/** A value read from a document (e.g. an offer PDF), with the page it came from and how sure the
  * extraction is. Values typed in by a human have no page.
  */
final case class Extracted[+A](value: A, page: Option[PageNumber], confidence: Confidence)

object Extracted:
  def fromPage[A](value: A, page: PageNumber, confidence: Confidence): Extracted[A] =
    Extracted(value, Some(page), confidence)

  /** Value entered or confirmed by a human: no document source, full confidence. */
  def manual[A](value: A): Extracted[A] = Extracted(value, None, Confidence.High)

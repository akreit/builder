package builder.domain

/** 1-based page number in a source document. */
opaque type PageNumber = Int

/** Smart constructor for [[PageNumber]]. */
object PageNumber:
  /** Smart constructor: page numbers must be ≥ 1. */
  def from(n: Int): Either[DomainError, PageNumber] =
    Either.cond(n >= 1, n, DomainError.InvalidPageNumber(n))
  extension (p: PageNumber) def value: Int = p

/** How sure the extractor is about a read value. */
enum Confidence:
  case High, Medium, Low

/** A value read from a document (e.g. an offer PDF), with the page it came from and how sure the
  * extraction is. Values typed in by a human have no page.
  */
final case class Extracted[+A](value: A, page: Option[PageNumber], confidence: Confidence)

/** Factories for document-sourced and manual [[Extracted]] values. */
object Extracted:
  /** Document-sourced value carrying page and confidence. */
  def fromPage[A](value: A, page: PageNumber, confidence: Confidence): Extracted[A] =
    Extracted(value, Some(page), confidence)

  /** Value entered or confirmed by a human: no document source, full confidence. */
  def manual[A](value: A): Extracted[A] = Extracted(value, None, Confidence.High)

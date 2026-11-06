package builder.extraction

import builder.domain.Leistung
import cats.effect.IO
import cats.effect.std.Semaphore

/** What one extraction pass sends to the model. */
enum PassContent:
  /** A JPEG/PNG photo of an offer, sent as an `image` block (no text splitting). */
  case Image(bytes: Array[Byte], fileType: OfferFileType)

  /** A PDF without a text layer (e.g. a scan), sent whole as a `document` block. */
  case ScannedPdf(bytes: Array[Byte])

  /** Page-marked text of some pages of a PDF. */
  case Text(chunk: TextChunk)

/** One model call: reads `content` into a (possibly partial) [[RawOffer]]. The provider (Claude)
  * implements this; [[MultiPassOfferExtractor]] turns it into an [[OfferExtractor]].
  */
trait OfferPass[F[_]]:
  def run(content: PassContent, known: List[Leistung]): F[Either[ExtractionError, RawOffer]]

  /** Prompt text (instructions + category context) of a pass, used to size the document budget. */
  def promptText(known: List[Leistung]): String

/** Factories for single-pass Claude [[OfferPass]] implementations. */
object OfferPass:
  /** At most `permits` passes run at the same time across all uploads (the app uses 2). */
  def bounded(underlying: OfferPass[IO], permits: Int): IO[OfferPass[IO]] =
    Semaphore[IO](permits.toLong).map(slots =>
      new OfferPass[IO]:
        def run(content: PassContent, known: List[Leistung]) =
          slots.permit.surround(underlying.run(content, known))
        def promptText(known: List[Leistung]) = underlying.promptText(known)
    )

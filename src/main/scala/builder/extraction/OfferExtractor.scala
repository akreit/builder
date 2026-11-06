package builder.extraction

import builder.domain.Leistung
import cats.effect.IO
import cats.effect.std.Semaphore

/** Kind of an uploaded offer file, recognised by its leading bytes (not by name or header). */
enum OfferFileType(val mediaType: String, val extension: String, val maxMegabytes: Int):
  case Pdf extends OfferFileType("application/pdf", "pdf", 20)

  /** Images above 5 MB are refused by the Claude API, so they are refused before the call. */
  case Jpeg extends OfferFileType("image/jpeg", "jpg", 5)
  case Png extends OfferFileType("image/png", "png", 5)

  /** Maximum upload size in bytes for this file kind. */
  def maxBytes: Int = maxMegabytes * 1024 * 1024

/** Magic-byte detection and extension lookup for [[OfferFileType]]. */
object OfferFileType:
  private val magic: List[(OfferFileType, List[Int])] = List(
    Pdf -> List(0x25, 0x50, 0x44, 0x46), // %PDF
    Jpeg -> List(0xff, 0xd8, 0xff),
    Png -> List(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)
  )

  /** Detects PDF/JPEG/PNG from leading magic bytes (not the file name). */
  def detect(bytes: Array[Byte]): Option[OfferFileType] =
    magic.collectFirst {
      case (t, sig)
          if bytes.lengthIs >= sig.length && sig.zip(bytes).forall((s, b) => (b & 0xff) == s) =>
        t
    }

  /** Looks up a file kind by its usual extension. */
  def fromExtension(ext: String): Option[OfferFileType] = values.find(_.extension == ext)

/** Why reading a file failed. Holds no response bodies: only what is safe to store and show. */
enum ExtractionError:
  case MissingApiKey

  /** Connection-level failure; `kind` is the exception class name. */
  case Transport(kind: String)

  /** Non-2xx answer with the API's `error.type`, if the body had one. */
  case HttpStatus(code: Int, errorType: Option[String])
  case UnexpectedResponse(message: String)

  /** No answer within the overall time limit (all attempts together). */
  case TimedOut

  /** The PDF could not be opened or its text could not be read (PDFBox); `reason` is the exception
    * class name, never document content.
    */
  case UnreadablePdf(reason: String)

/** Reads the contents of an offer document, mapping its line items onto the category's `known`
  * Leistungen (or proposing new coarse ones) in the same pass.
  */
trait OfferExtractor[F[_]]:
  /** Reads an offer file into a [[RawOffer]], mapping items onto `known` Leistungen. */
  def extract(
      file: Array[Byte],
      fileType: OfferFileType,
      known: List[Leistung]
  ): F[Either[ExtractionError, RawOffer]]

/** Concurrency wrappers around an [[OfferExtractor]]. */
object OfferExtractor:
  /** At most `permits` extractions run at the same time; further callers wait for a slot. */
  def bounded(underlying: OfferExtractor[IO], permits: Int): IO[OfferExtractor[IO]] =
    Semaphore[IO](permits.toLong).map(slots =>
      new OfferExtractor[IO]:
        /** Forwards to the underlying extractor while holding a semaphore permit. */
        def extract(file: Array[Byte], fileType: OfferFileType, known: List[Leistung]) =
          slots.permit.surround(underlying.extract(file, fileType, known))
    )

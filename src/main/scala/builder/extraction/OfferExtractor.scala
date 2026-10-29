package builder.extraction

/** Kind of an uploaded offer file, recognised by its leading bytes (not by name or header). */
enum OfferFileType(val mediaType: String, val extension: String):
  case Pdf extends OfferFileType("application/pdf", "pdf")
  case Jpeg extends OfferFileType("image/jpeg", "jpg")
  case Png extends OfferFileType("image/png", "png")

object OfferFileType:
  private val magic: List[(OfferFileType, List[Int])] = List(
    Pdf -> List(0x25, 0x50, 0x44, 0x46), // %PDF
    Jpeg -> List(0xff, 0xd8, 0xff),
    Png -> List(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)
  )

  def detect(bytes: Array[Byte]): Option[OfferFileType] =
    magic.collectFirst {
      case (t, sig)
          if bytes.lengthIs >= sig.length && sig.zip(bytes).forall((s, b) => (b & 0xff) == s) =>
        t
    }

  def fromExtension(ext: String): Option[OfferFileType] = values.find(_.extension == ext)

enum ExtractionError:
  case MissingApiKey
  case Transport(message: String)
  case HttpStatus(code: Int, body: String)
  case UnexpectedResponse(message: String)

/** Reads the contents of an offer document. */
trait OfferExtractor[F[_]]:
  def extract(file: Array[Byte], fileType: OfferFileType): F[Either[ExtractionError, RawOffer]]

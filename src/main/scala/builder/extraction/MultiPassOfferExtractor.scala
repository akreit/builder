package builder.extraction

import builder.domain.Leistung
import cats.effect.IO
import cats.syntax.all.*

/** [[OfferExtractor]] that reads PDFs as text first and splits long ones into page-aware passes
  * within `budget`; the partial answers are merged by [[RawOfferMerge]]. Images take one image
  * pass; PDFs without any text layer (scans) take one whole-document pass. Concurrency of passes is
  * bounded by the given (bounded) [[OfferPass]].
  */
final class MultiPassOfferExtractor(pass: OfferPass[IO], budget: PassBudget = PassBudget.default)
    extends OfferExtractor[IO]:

  def extract(
      file: Array[Byte],
      fileType: OfferFileType,
      known: List[Leistung]
  ): IO[Either[ExtractionError, RawOffer]] =
    fileType match
      case OfferFileType.Jpeg | OfferFileType.Png =>
        pass.run(PassContent.Image(file, fileType), known)
      case OfferFileType.Pdf =>
        // PDFBox parses synchronously and can take long on big files: off the compute pool.
        IO.blocking(PdfText.pages(file)).flatMap {
          case Left(error)                             => IO.pure(Left(error))
          case Right(pages) if pages.forall(_.isBlank) =>
            pass.run(PassContent.ScannedPdf(file), known)
          case Right(pages) => passes(plan(pages, known), known)
        }

  /** The chunks a PDF with these pages is read in. */
  def plan(pages: List[PdfPage], known: List[Leistung]): List[TextChunk] =
    Chunking.chunks(pages, budget.documentChars(pass.promptText(known)), budget.maxPagesPerPass)

  private def passes(
      chunks: List[TextChunk],
      known: List[Leistung]
  ): IO[Either[ExtractionError, RawOffer]] =
    chunks
      .parTraverse(c => pass.run(PassContent.Text(c), known).map(_.left.map(inChunk(c, _))))
      .map(_.sequence.map(parts => RawOfferMerge.merge(chunks.zip(parts))))

  /** A failed pass fails the whole offer (no page is dropped); multi-pass errors name the part. */
  private def inChunk(chunk: TextChunk, error: ExtractionError): ExtractionError =
    error match
      case ExtractionError.UnexpectedResponse(m) if chunk.total > 1 =>
        ExtractionError.UnexpectedResponse(s"$m in ${chunk.label}")
      case other => other

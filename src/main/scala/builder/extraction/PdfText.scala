package builder.extraction

import org.apache.pdfbox.Loader
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.text.PDFTextStripper

import scala.util.Try
import scala.util.Using

/** Text of one PDF page; `number` is 1-based, as printed in the page markers sent to Claude. */
final case class PdfPage(number: Int, text: String):
  /** True when the page has no meaningful text (scan / empty). */
  def isBlank: Boolean = text.trim.isEmpty

/** Reads the text layer of a PDF with Apache PDFBox, page by page. The Java boundary is wrapped in
  * `Try` and surfaced as [[ExtractionError.UnreadablePdf]].
  */
object PdfText:
  /** Extracts per-page text via PDFBox, or [[ExtractionError.UnreadablePdf]]. */
  def pages(bytes: Array[Byte]): Either[ExtractionError, List[PdfPage]] =
    Try(Using.resource(Loader.loadPDF(bytes))(readAll)).toEither.left.map(e =>
      ExtractionError.UnreadablePdf(e.getClass.getSimpleName)
    )

  /** Reads every page of an open document into [[PdfPage]] values. */
  private def readAll(doc: PDDocument): List[PdfPage] =
    val stripper = PDFTextStripper()
    (1 to doc.getNumberOfPages).toList.map(n => PdfPage(n, pageText(stripper, doc, n)))

  /** Text of a single 1-based page using the shared stripper. */
  private def pageText(stripper: PDFTextStripper, doc: PDDocument, n: Int): String =
    stripper.setStartPage(n)
    stripper.setEndPage(n)
    stripper.getText(doc)

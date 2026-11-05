package builder.extraction

import org.apache.pdfbox.Loader
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.text.PDFTextStripper

import scala.util.Try
import scala.util.Using

/** Text of one PDF page; `number` is 1-based, as printed in the page markers sent to Claude. */
final case class PdfPage(number: Int, text: String):
  def isBlank: Boolean = text.trim.isEmpty

/** Reads the text layer of a PDF with Apache PDFBox, page by page. The Java boundary is wrapped in
  * `Try` and surfaced as [[ExtractionError.UnreadablePdf]].
  */
object PdfText:
  def pages(bytes: Array[Byte]): Either[ExtractionError, List[PdfPage]] =
    Try(Using.resource(Loader.loadPDF(bytes))(readAll)).toEither.left.map(e =>
      ExtractionError.UnreadablePdf(e.getClass.getSimpleName)
    )

  private def readAll(doc: PDDocument): List[PdfPage] =
    val stripper = PDFTextStripper()
    (1 to doc.getNumberOfPages).toList.map(n => PdfPage(n, pageText(stripper, doc, n)))

  private def pageText(stripper: PDFTextStripper, doc: PDDocument, n: Int): String =
    stripper.setStartPage(n)
    stripper.setEndPage(n)
    stripper.getText(doc)

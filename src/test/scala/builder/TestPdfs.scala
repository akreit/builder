package builder

import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.pdmodel.PDPage
import org.apache.pdfbox.pdmodel.PDPageContentStream
import org.apache.pdfbox.pdmodel.font.PDType1Font
import org.apache.pdfbox.pdmodel.font.Standard14Fonts

import java.io.ByteArrayOutputStream
import scala.util.Using

/** Generates offer-like German PDFs with PDFBox for extraction tests (no fixtures on disk). */
object TestPdfs:
  /** One line per position, `linesPerPage` positions per page; page n mentions "Seite n". */
  def offer(pages: Int, linesPerPage: Int = 30): Array[Byte] =
    write((1 to pages).toList.map(n => pageLines(n, linesPerPage)))

  /** A PDF whose pages have no text layer (like a scan). */
  def blank(pages: Int): Array[Byte] = write(List.fill(pages)(Nil))

  private def pageLines(n: Int, lines: Int): List[String] =
    s"Angebot Dachdeckerei Muster GmbH – Seite $n" ::
      (1 to lines).toList.map(i =>
        s"Pos. $n.$i Dachlatten 30x50 mm imprägniert, Menge ${i * 3} m, Einzelpreis 4,${i % 10}0 €, Gesamtpreis ${i * 12},00 €"
      )

  private def write(pages: List[List[String]]): Array[Byte] =
    Using
      .resource(PDDocument()) { doc =>
        val font = PDType1Font(Standard14Fonts.FontName.HELVETICA)
        pages.foreach { lines =>
          val page = PDPage()
          doc.addPage(page)
          Using.resource(PDPageContentStream(doc, page)) { cs =>
            cs.beginText()
            cs.setFont(font, 7)
            cs.setLeading(9)
            cs.newLineAtOffset(30, 760)
            lines.foreach { l =>
              cs.showText(l)
              cs.newLine()
            }
            cs.endText()
          }
        }
        val out = ByteArrayOutputStream()
        doc.save(out)
        out.toByteArray
      }

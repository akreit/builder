package builder.pdf

import builder.domain.*
import builder.domain.OfferLabel
import builder.extraction.RawLineItem
import builder.repo.*
import builder.ui.Format
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.pdmodel.PDPage
import org.apache.pdfbox.pdmodel.PDPageContentStream
import org.apache.pdfbox.pdmodel.common.PDRectangle
import org.apache.pdfbox.pdmodel.font.PDType1Font
import org.apache.pdfbox.pdmodel.font.Standard14Fonts

import java.io.ByteArrayOutputStream
import scala.util.Try
import scala.util.Using

/** Landscape comparison, at most three offers per horizontal section. Every continuation repeats
  * the category and offer headings; no content is truncated.
  */
object ComparisonPdf:
  private def offerHeading(col: MatrixColumn): String =
    val label = OfferLabel.display(col.raw)
    if label.nonEmpty then label else col.upload.upload.fileName

  private def coverage(c: Coverage): String = c match
    case Coverage.Enthalten      => "enthalten"
    case Coverage.NichtEnthalten => "NICHT ENTHALTEN"
    case Coverage.Ausgeschlossen => "AUSGESCHLOSSEN"
    case Coverage.Unklar         => "UNKLAR"

  private def position(li: RawLineItem): String =
    s"${li.description.text.getOrElse("Beschreibung fehlt")}\nMenge: ${li.quantity.text.getOrElse("?")} ${li.unit.text.getOrElse("")}\nEP: ${li.unitPrice.text.getOrElse("?")}\nGesamt: ${li.total.text.getOrElse("?")}"

  private def cellText(m: Matrix, row: MatrixRow, i: Int): String =
    row.cells
      .lift(i)
      .fold("?")(c =>
        coverage(c.coverage) + (if c.flagged then " (prüfen)" else "") +
          s" | Summe: ${c.price.map(Format.money).getOrElse("?")}" +
          m.columns
            .lift(i)
            .toList
            .flatMap(col => c.items.flatMap(col.raw.lineItems.lift).map(position))
            .mkString("\n", "\n", "")
      )

  private def unassignedRow(m: Matrix, indexes: List[Int]): List[String] =
    "Nicht zugeordnet" :: indexes.map(i =>
      m.columns
        .lift(i)
        .toList
        .flatMap(col =>
          m.unassigned
            .filter(_.offer == col.upload.upload.id)
            .flatMap(u => col.raw.lineItems.lift(u.item).map(position))
        )
        .mkString("\n")
    )

  private def totalsRow(m: Matrix, indexes: List[Int]): List[String] =
    "Summe Zellen + nicht zugeordnet" :: indexes.map(i =>
      m.totals
        .lift(i)
        .fold("?")(t =>
          t.sum.map(Format.money).getOrElse("nicht lesbar") +
            s"\nNetto laut Angebot: ${t.net.map(Format.money).getOrElse("nicht lesbar")}" +
            (if t.mismatch then "\nWeicht vom Netto ab – Zuordnung prüfen" else "")
        )
    )

  private def exclusionsRow(m: Matrix, indexes: List[Int]): List[String] =
    "AUSSCHLÜSSE" :: indexes.map(i =>
      m.columns
        .lift(i)
        .fold("?")(col =>
          col.raw.exclusions.flatMap(_.text) match
            case Nil => "Keine erkannt (nicht bestätigt)"
            case xs  => xs.mkString("\n")
        )
    )

  private def offerStatusRow(m: Matrix, indexes: List[Int]): List[String] =
    "Angebot / Status" :: indexes.map(i =>
      m.columns
        .lift(i)
        .fold("?")(col =>
          offerHeading(col) + (col.upload.status match
            case UploadStatus.Accepted(_, _)      => " (validiert)"
            case UploadStatus.Rejected(_, _)      => " (NICHT VALIDIERT – prüfen)"
            case UploadStatus.ExtractionFailed(_) => " (nicht lesbar)")
        )
    )

  /** Table body for one horizontal batch of offer columns (at most three). */
  private def rows(m: Matrix, indexes: List[Int]): List[List[String]] =
    val scope = m.rows.map(r => r.leistung.name.value :: indexes.map(i => cellText(m, r, i)))
    offerStatusRow(m, indexes) :: (scope ++ List(
      unassignedRow(m, indexes),
      totalsRow(m, indexes),
      exclusionsRow(m, indexes)
    ))

  private def safeText(font: PDType1Font, s: String): String =
    s.map(c => if Try(font.encode(c.toString)).isSuccess then c else '?')

  private def wrapped(font: PDType1Font, s: String, width: Float): List[String] =
    s.split("\n", -1).toList.flatMap { line =>
      val (done, last) =
        safeText(font, line).foldLeft((List.empty[String], "")) { case ((done, current), c) =>
          val next = current + c
          if font.getStringWidth(next) * 9 / 1000 > width && current.nonEmpty then
            (current :: done, c.toString)
          else (done, next)
        }
      (last :: done).reverse
    }

  private def writePage(
      doc: PDDocument,
      font: PDType1Font,
      category: Category,
      currency: Currency,
      headings: List[String],
      body: List[List[String]],
      pageIndex: Int
  ): Unit =
    val width = 780f / headings.size
    val textWidth = width - 12
    val page = PDPage(PDRectangle(842, 595))
    doc.addPage(page)
    Using.resource(PDPageContentStream(doc, page)) { cs =>
      def text(s: String, x: Float, y: Float, size: Float): Unit =
        cs.beginText()
        cs.setFont(font, size)
        cs.newLineAtOffset(x, y)
        cs.showText(safeText(font, s))
        cs.endText()
      text(s"Leistungsvergleich – ${category.name.value} – Teil ${pageIndex + 1}", 30, 565, 13)
      text(
        "Beträge in " + currency.code + "; ? = fehlt / nicht lesbar. Extrahierte Angaben prüfen.",
        30,
        547,
        9
      )
      headings.zipWithIndex.foreach((h, i) =>
        wrapped(font, h, textWidth)
          .take(2)
          .zipWithIndex
          .foreach((l, n) => text(l, 30 + i * width, 525f - n.toFloat * 12, 9))
      )
      body.zipWithIndex.foreach((row, n) =>
        row.zipWithIndex.foreach((s, i) => text(s, 30 + i * width, 490f - n.toFloat * 13, 8))
      )
    }

  private def renderCategory(
      doc: PDDocument,
      font: PDType1Font,
      view: ProjectView,
      category: Category
  ): Unit =
    val m = Matrix.build(view, category.id)
    val batches = m.columns.indices.toList.grouped(3).toList match
      case Nil => List(Nil)
      case xs  => xs
    batches.foreach { indexes =>
      val headings =
        "Leistung / Umfang" :: indexes.flatMap(i => m.columns.lift(i).map(col => offerHeading(col)))
      val textWidth = 780f / headings.size - 12
      val lines = rows(m, indexes).flatMap { row =>
        val cells = row.map(wrapped(font, _, textWidth))
        val height = cells.map(_.size).maxOption.getOrElse(1)
        (0 until height).toList.map(n => cells.map(_.lift(n).getOrElse(""))) :+
          List.fill(headings.size)("----------------")
      }
      val pages = if lines.isEmpty then List(List(List("Noch keine Angebote")))
      else lines.grouped(33).toList
      pages.zipWithIndex.foreach { (body, pageIndex) =>
        writePage(doc, font, category, view.project.currency, headings, body, pageIndex)
      }
    }

  /** Renders the full comparison PDF. `Try`/`Using` stay at the PDFBox Java boundary (checked
    * exception / AutoCloseable); no domain logic uses throw/catch.
    */
  def render(view: ProjectView): Either[String, Array[Byte]] =
    Try(Using.resource(PDDocument()) { doc =>
      val font = PDType1Font(Standard14Fonts.FontName.HELVETICA)
      view.project.categories.foreach(c => renderCategory(doc, font, view, c))
      val out = ByteArrayOutputStream()
      doc.save(out)
      out.toByteArray
    }).toEither.left.map(_.getClass.getSimpleName)

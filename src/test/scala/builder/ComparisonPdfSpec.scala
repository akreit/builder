package builder

import builder.domain.*
import builder.extraction.OfferFileType
import builder.extraction.PdfText
import builder.pdf.ComparisonPdf
import builder.repo.*
import org.scalatest.EitherValues
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ComparisonPdfSpec extends AnyFreeSpec with Matchers with EitherValues with OptionValues:
  private def emptyView = ProjectView(
    SampleData.project.value,
    Nil,
    Nil,
    SampleData.leistungen.value.map(_._2),
    Nil,
    Nil,
    Map.empty,
    Weights.default
  )

  "an empty comparison exports every category and missing values without inventing offers" in {
    val pages = PdfText.pages(ComparisonPdf.render(emptyView).value).value
    pages.size shouldBe 6
    val text = pages.map(_.text).mkString
    List("Dach", "Fenster", "Fassadendämmung", "Elektrik", "Heizung", "Innenausbau").foreach(c =>
      text should include(c)
    )
    text should include("AUSSCHLÜSSE")
  }

  "offers stay side by side and long content and exclusions survive pagination" in {
    val base = emptyView
    val cat = base.project.categories.headOption.value
    val long = "Leistungsumfang " * 180
    val raw = RawOffers.dachfenster.copy(
      lineItems = List(RawOffers.item(long + " ENDE", "3", "Stk", "1250.00", "3750.00")),
      exclusions = List(RawOffers.f("Innenfutter ausgeschlossen"))
    )
    val uploads = (1 to 4).toList.map { n =>
      val id = Fixtures.offerId(s"pdf-$n")
      val offer = Fixtures.offer(s"pdf-$n", Fixtures.Roofing, 100)
      val r = raw.copy(contractor = RawOffers.f(s"Firma $n"))
      UploadView(
        Upload(
          id,
          cat.id,
          s"angebot-$n.pdf",
          OfferFileType.Pdf,
          java.time.Instant.EPOCH,
          UploadOutcome.Read(r)
        ),
        UploadStatus.Accepted(r, offer)
      )
    }
    val pages = PdfText.pages(ComparisonPdf.render(base.copy(uploads = uploads)).value).value
    val text = pages.map(_.text).mkString
    pages.size should be > 6
    List("Firma 1", "Firma 2", "Firma 3").foreach(n =>
      pages.headOption.value.text should include(n)
    )
    List("Firma 4", "ENDE", "EP: 1250.00", "3750.00", "Innenfutter ausgeschlossen", "5961.90")
      .foreach(s => text should include(s))
  }

  "manual exclusions and missing prices are shown without treating missing amounts as zero" in {
    val base = emptyView
    val l = base.leistungen.headOption.value
    val id = Fixtures.offerId("pdf-coverage")
    val raw = RawOffers.dachfenster.copy(lineItems =
      List(RawOffers.item("Dacharbeiten", "2", "m²", "", ""))
    )
    val u = UploadView(
      Upload(
        id,
        l.category,
        "unvollstaendig.pdf",
        OfferFileType.Pdf,
        java.time.Instant.EPOCH,
        UploadOutcome.Read(raw)
      ),
      UploadStatus.Accepted(raw, Fixtures.offer("pdf-coverage", Fixtures.Roofing, 100))
    )
    val view = base.copy(
      uploads = List(u),
      assignments = List(ItemAssignment(id, 0, Some(l.id), true)),
      coverage = List(CoverageEntry(id, l.id, Coverage.Ausgeschlossen, CoverageSource.Manual))
    )
    val text = PdfText.pages(ComparisonPdf.render(view).value).value.map(_.text).mkString
    text should include("AUSGESCHLOSSEN")
    text should include("Dacharbeiten")
    text should include("EP: ?")
    text should include("Gesamt: ?")
  }

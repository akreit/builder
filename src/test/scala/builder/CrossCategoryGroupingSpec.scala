package builder

import builder.RawOffers.*
import builder.domain.*
import builder.extraction.*
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import fs2.Stream
import org.http4s.*
import org.http4s.multipart.Multiparts
import org.http4s.multipart.Part
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

/** Owner regressions for PR26: cross-category line grouping, category creation inside the
  * comparison view, and summary-only Leistungsverzeichnis (no line-item dump).
  */
class CrossCategoryGroupingSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers:
  private val pdf: Array[Byte] = "%PDF-1.7\nfiktives Angebot".getBytes("UTF-8")

  private def run(app: TestApp, req: Request[IO]): IO[(Status, String)] =
    app.http.run(req).flatMap(r => r.bodyText.compile.string.map(r.status -> _))

  private def htmx(req: Request[IO]): Request[IO] =
    req.putHeaders(Header.Raw(org.typelevel.ci.CIString("HX-Request"), "true"))

  private def upload(app: TestApp, category: String): IO[(Status, String)] =
    Multiparts
      .forSync[IO]
      .flatMap(
        _.multipart(
          Vector(Part.fileData[IO]("datei", "angebot.pdf", Stream.emits(pdf.toIndexedSeq)))
        )
      )
      .flatMap(mp =>
        run(
          app,
          htmx(
            Request[IO](
              Method.POST,
              Uri.unsafeFromString(s"/ui/projects/demo/categories/$category/angebote")
            ).withEntity(mp).putHeaders(mp.headers)
          )
        )
      )

  private def edit(app: TestApp, ansicht: String, fields: (String, String)*): IO[(Status, String)] =
    run(
      app,
      htmx(
        Request[IO](Method.POST, Uri.unsafeFromString("/ui/projects/demo/aenderungen"))
          .withEntity(UrlForm((("ansicht" -> ansicht) +: fields)*))
      )
    )

  private def page(app: TestApp, category: String): IO[String] =
    run(app, Request[IO](Method.GET, Uri.unsafeFromString(s"/projekte/demo/kategorien/$category")))
      .map(_._2)

  private def idOf(html: String, name: String): String =
    s"""data-leistung="([^"]+)">${java.util.regex.Pattern.quote(name)}<""".r
      .findFirstMatchIn(html)
      .fold("")(_.group(1))

  private def offerIn(html: String, category: String): String =
    s"""data-zelle="$category-[^|"]+\\|([^"]+)"""".r
      .findFirstMatchIn(html)
      .fold("")(_.group(1))

  private def verzeichnis(html: String): String =
    """id="leistungsverzeichnis"[\s\S]*?(?=<section id="summe"|$)""".r
      .findFirstIn(html)
      .getOrElse("")

  private def vergleichSection(html: String): String =
    """class="leistungen"[^>]*data-kategorie="[^"]+"[\s\S]*?</section>""".r
      .findFirstIn(html)
      .getOrElse {
        // Fallback: from Leistungsvergleich heading to next top-level section-ish marker
        """<h3>Leistungsvergleich</h3>[\s\S]*?(?=<section class="summe"|<form[^>]*kategorie-neu|id="leistungsverzeichnis"|$)""".r
          .findFirstIn(html)
          .getOrElse("")
      }

  "manual cross-category assign groups a fenster position onto a dach Leistung" in {
    val extractor = new SwitchingExtractor(gauben)
    TestApp.fresh(extractor).use { app =>
      for
        (_, g) <- upload(app, "dach")
        blech = idOf(g, "Gaubenblech")
        _ <- extractor.set(
          dachfenster.copy(
            lineItems = dachfenster.lineItems.map(_.copy(leistung = Some(blech))),
            abdeckung = Some(List(RawAbdeckung(blech, "enthalten", 2, Confidence.High)))
          )
        )
        (_, html) <- upload(app, "fenster")
        offer = offerIn(html, "fenster")
        (st, after) <- edit(
          app,
          "fenster",
          "aktion" -> "position-zuordnen",
          "angebot" -> offer,
          "position" -> "1",
          "ziel" -> blech
        )
        dach <- page(app, "dach")
      yield
        blech should not be empty
        offer should not be empty
        // Must succeed — not the old same-category Conflict.
        st shouldBe Status.Ok
        after should not include "anderen Kategorie"
        after should include("""label="Dach"""")
        after should include("""label="Fenster"""")
        dach should include(offer)
        dach should include(blech)
    }
  }

  "Leistungsverzeichnis is summary-only: categories and aggregates, no position lines" in {
    TestApp.fresh(RecordingExtractor(List(dachfensterMitLeistungen))).use { app =>
      for
        _ <- upload(app, "fenster")
        html <- page(app, "fenster")
        v = verzeichnis(html)
      yield
        html should include("""id="leistungsverzeichnis"""")
        v should include("Leistungsverzeichnis")
        v should include("""data-verzeichnis-kategorie="fenster"""")
        v should include("""data-verzeichnis-kategorie="dach"""")
        v should include("Kategorie")
        v should include("Leistungen")
        v should include("Angebote")
        // No individual line items re-listed in the summary view.
        v should not include "Pos. 1:"
        v should not include "Dachfenster liefern"
        v should not include "Positionen zuordnen"
    }
  }

  "comparison view offers a new-category control; new category then receives a cross-group assign" in {
    TestApp.fresh(RecordingExtractor(List(dachfensterMitLeistungen))).use { app =>
      for
        (_, uploaded) <- upload(app, "fenster")
        offer = offerIn(uploaded, "fenster")
        vergleich = vergleichSection(uploaded)
        // Category creation control must live inside the comparison (Leistungen) section.
        _ = vergleich should include("aktion")
        _ = vergleich should include("kategorie-neu")
        _ = vergleich should include("Kategorie hinzufügen")
        (addSt, added) <- edit(
          app,
          "fenster",
          "aktion" -> "kategorie-neu",
          "name" -> "Gruppenfenster"
        )
        newCat =
          """data-kategorie="(k-[^"]+)"""".r
            .findFirstMatchIn(added)
            .fold("")(_.group(1))
        // Create the Leistung while viewing the new category so it appears in that matrix.
        (leistSt, _) <- edit(
          app,
          newCat,
          "aktion" -> "leistung-neu",
          "kategorie" -> newCat,
          "name" -> "Fenstergruppe"
        )
        withLeistung <- page(app, newCat)
        lid = idOf(withLeistung, "Fenstergruppe")
        // Assign from the offer's home category (fenster) onto the new category's Leistung.
        (assignSt, assigned) <- edit(
          app,
          "fenster",
          "aktion" -> "position-zuordnen",
          "angebot" -> offer,
          "position" -> "1",
          "ziel" -> lid
        )
        targetPage <- page(app, newCat)
        // Persistence: full page reload of the new CATEGORY (not a Leistung id).
        reopened <- page(app, newCat)
        fensterReload <- page(app, "fenster")
      yield
        offer should not be empty
        addSt shouldBe Status.Ok
        // Domain: CATEGORY id is k-…, never a Leistung id (l-…).
        newCat should startWith("k-")
        newCat should not startWith ("l-")
        added should include("Gruppenfenster")
        // Appears as a real category: sidebar/tab + Verzeichnis row.
        added should include(s"""data-kategorie="$newCat"""")
        added should include(s"""href="/projekte/demo/kategorien/$newCat"""")
        added should include(s"""data-verzeichnis-kategorie="$newCat"""")
        leistSt shouldBe Status.Ok
        lid should not be empty
        lid should startWith("l-") // scaffolding Leistung under the new CATEGORY
        assignSt shouldBe Status.Ok
        assigned should not include "anderen Kategorie"
        targetPage should include(offer)
        targetPage should include("Fenstergruppe")
        targetPage should include(s"""data-kategorie="$newCat"""")
        reopened should include("Gruppenfenster")
        reopened should include(offer)
        reopened should include(lid)
        // Home category still offers the cross-category Ziel after reload.
        fensterReload should include(lid)
        fensterReload should include("""label="Gruppenfenster"""")
    }
  }

  /** Same helper as LeistungenReviewSpec: mutable extractor for two-step uploads. */
  final class SwitchingExtractor(initial: RawOffer) extends OfferExtractor[IO]:
    private val ref = cats.effect.Ref.unsafe[IO, RawOffer](initial)
    def set(raw: RawOffer): IO[Unit] = ref.set(raw)
    def extract(
        file: Array[Byte],
        fileType: OfferFileType,
        known: List[Leistung]
    ): IO[Either[ExtractionError, RawOffer]] = ref.get.map(Right(_))

  private val gauben: RawOffer = dachfenster.copy(
    contractor = f("Gaubenbau Erfunden KG"),
    lineItems = dachfenster.lineItems.map(_.copy(leistung = Some("neu-g"))),
    neueLeistungen = Some(List(RawNeueLeistung("neu-g", "Gaubenblech")))
  )

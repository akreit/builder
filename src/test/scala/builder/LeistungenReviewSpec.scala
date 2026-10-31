package builder

import builder.domain.*
import builder.extraction.*
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import fs2.Stream
import org.http4s.*
import org.http4s.multipart.Multiparts
import org.http4s.multipart.Part
import org.scalatest.EitherValues
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

/** Review follow-ups of the Leistungen feature: limits, status codes, check sums, upgrades. */
class LeistungenReviewSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers with EitherValues:
  import RawOffers.*

  private val pdf: Array[Byte] = "%PDF-1.7\nfiktives Angebot".getBytes("UTF-8")
  private val fenster = CategoryId.from("dachfenster").value
  private def lid(s: String) = LeistungId.from(s).value
  private val seeded = SampleData.leistungen.value.map(_._2)

  /** A Gauben offer that proposes the Leistung "Gaubenblech" for both items. */
  private val gauben: RawOffer = dachfenster.copy(
    contractor = f("Gaubenbau Erfunden KG"),
    lineItems = dachfenster.lineItems.map(_.copy(leistung = Some("neu-g"))),
    neueLeistungen = Some(List(RawNeueLeistung("neu-g", "Gaubenblech")))
  )

  // ---- pure ----------------------------------------------------------------------------------

  "an extraction adds at most MaxNew Leistungen and reports how many it dropped" in {
    val many = (1 to 12).toList.map(i => RawNeueLeistung(s"neu-$i", s"Leistung $i"))
    val raw = dachfenster.copy(neueLeistungen = Some(many))
    val plan = LeistungPlan.plan(raw, fenster, seeded, LazyList.from(1).map(i => lid(s"l-$i")))
    plan.added.map(_.name.value) shouldBe (1 to 10).toList.map(i => s"Leistung $i")
    LeistungPlan.dropped(raw) shouldBe 2
    LeistungPlan.dropped(dachfensterMitLeistungen) shouldBe 0
  }

  "a reference to a Leistung of another category stays unmapped" in {
    val other = Leistung(
      lid("gauben-blech"),
      CategoryId.from("gauben").value,
      LeistungName.from("Blech").value
    )
    val raw = dachfenster.copy(
      lineItems = dachfenster.lineItems.map(_.copy(leistung = Some("gauben-blech"))),
      abdeckung = Some(List(RawAbdeckung("gauben-blech", "enthalten", 2, Confidence.High)))
    )
    // Only the category's own Leistungen are known to the plan, never `other`.
    val plan = LeistungPlan.plan(raw, fenster, seeded, LazyList.empty)
    plan.items shouldBe List(None, None)
    plan.coverage shouldBe Nil
    seeded should not contain other
  }

  /** Returns the offer last [[set]]; lets a test refer to ids created by earlier uploads. */
  final class SwitchingExtractor(initial: RawOffer) extends OfferExtractor[IO]:
    private val ref = cats.effect.Ref.unsafe[IO, RawOffer](initial)
    def set(raw: RawOffer): IO[Unit] = ref.set(raw)
    def extract(
        file: Array[Byte],
        fileType: OfferFileType,
        known: List[Leistung]
    ): IO[Either[ExtractionError, RawOffer]] = ref.get.map(Right(_))

  // ---- through the app -----------------------------------------------------------------------

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

  private def edit(app: TestApp, fields: (String, String)*): IO[(Status, String)] =
    run(
      app,
      htmx(
        Request[IO](Method.POST, Uri.unsafeFromString("/ui/projects/demo/aenderungen"))
          .withEntity(UrlForm(fields*))
      )
    )

  private def idOf(html: String, name: String): String =
    s"""data-leistung="([^"]+)">${java.util.regex.Pattern.quote(name)}<""".r
      .findFirstMatchIn(html)
      .fold("")(_.group(1))

  private def offerIn(html: String, category: String): String =
    s"""data-zelle="$category-[^|"]+\\|([^"]+)"""".r
      .findFirstMatchIn(html)
      .fold("")(_.group(1))

  /** The `data-abweichung` flags of the check-sum row, in column order. */
  private def mismatches(html: String): List[String] =
    """data-abweichung="(true|false)"""".r.findAllMatchIn(html).map(_.group(1)).toList

  "Leistungen of another category are never assigned, neither by the extractor nor by hand" in {
    val extractor = SwitchingExtractor(gauben)
    TestApp.fresh(extractor).use { app =>
      for
        (_, g) <- upload(app, "gauben")
        blech = idOf(g, "Gaubenblech")
        // The extractor (wrongly) points the Dachfenster items at the Gauben Leistung.
        _ <- extractor.set(
          dachfenster.copy(
            lineItems = dachfenster.lineItems.map(_.copy(leistung = Some(blech))),
            abdeckung = Some(List(RawAbdeckung(blech, "enthalten", 2, Confidence.High)))
          )
        )
        (_, html) <- upload(app, "dachfenster")
        offer = offerIn(html, "dachfenster")
        (assignStatus, assign) <- edit(
          app,
          "aktion" -> "position-zuordnen",
          "angebot" -> offer,
          "position" -> "1",
          "ziel" -> blech
        )
        (coverageStatus, coverage) <- edit(
          app,
          "aktion" -> "status-setzen",
          "angebot" -> offer,
          "leistung" -> blech,
          "status" -> "enthalten"
        )
      yield
        blech should not be empty
        html should include("Pos. 1: Dachfenster liefern")
        html should include("Pos. 2: Einbau inkl. Eindeckrahmen")
        assignStatus shouldBe Status.Conflict
        assign should include("anderen Kategorie")
        coverageStatus shouldBe Status.Conflict
        coverage should include("anderen Kategorie")
    }
  }

  "unknown Leistungen are 404, unknown positions 422, oversized forms 413" in {
    TestApp.fresh(RecordingExtractor(List(dachfensterMitLeistungen))).use { app =>
      for
        (_, html) <- upload(app, "dachfenster")
        offer = offerIn(html, "dachfenster")
        (missing, _) <- edit(
          app,
          "aktion" -> "leistung-umbenennen",
          "leistung" -> "nix",
          "name" -> "Y"
        )
        (badItem, text) <- edit(
          app,
          "aktion" -> "position-zuordnen",
          "angebot" -> offer,
          "position" -> "9",
          "ziel" -> ""
        )
        (tooBig, _) <- edit(app, "aktion" -> "kategorie-neu", "name" -> ("x" * (20 * 1024)))
        (used, _) <- edit(app, "aktion" -> "kategorie-loeschen", "kategorie" -> "dachfenster")
      yield
        missing shouldBe Status.NotFound
        badItem shouldBe Status.UnprocessableContent
        text should include("Position 9 gibt es nicht")
        tooBig shouldBe Status.PayloadTooLarge
        used shouldBe Status.Conflict
    }
  }

  "each column sums cells and unassigned items and is flagged when that differs from net" in {
    val off = dachfensterMitLeistungen.copy(
      contractor = f("Abweichend GmbH"),
      net = f("6000.00", 3)
    )
    TestApp.fresh(RecordingExtractor(List(dachfensterMitLeistungen, off))).use { app =>
      for
        (_, one) <- upload(app, "dachfenster")
        (_, two) <- upload(app, "dachfenster")
      yield
        mismatches(one) shouldBe List("false")
        one should include("5.010,00")
        mismatches(two) shouldBe List("false", "true")
        two should include("Weicht vom Netto ab")
    }
  }

  "the UI warns when proposals beyond the cap were dropped" in {
    val many = (1 to 13).toList.map(i => RawNeueLeistung(s"neu-$i", s"Leistung $i"))
    TestApp.fresh(RecordingExtractor(List(dachfenster.copy(neueLeistungen = Some(many))))).use {
      app =>
        upload(app, "dachfenster").map { (_, html) =>
          html should include("3 weitere Leistungsvorschläge verworfen")
          idOf(html, "Leistung 10") should not be empty
          idOf(html, "Leistung 11") shouldBe empty
        }
    }
  }

  // ---- upgrade -------------------------------------------------------------------------------

  "after a restart offers, uploads and favorites are still there and all foreign keys hold" in {
    TestApp.tempDir.use { dir =>
      val extractor = RecordingExtractor(List(dachfenster))
      for
        (_, first) <- TestApp.resource(dir, extractor).use(app => upload(app, "dachfenster"))
        _ <- TestDb.sql(
          dir,
          "INSERT INTO favorites SELECT project_id, category_id, id FROM uploads"
        )
        (_, html) <- TestApp
          .resource(dir, extractor)
          .use(app => run(app, Request[IO](Method.GET, Uri.unsafeFromString("/"))))
        counts <- List("uploads", "offers o JOIN uploads u USING (project_id, id)", "favorites")
          .flatTraverse(t => TestDb.ints(dir, s"SELECT count(*)::int FROM $t"))
        orphanFavorite <- TestDb
          .sql(dir, "INSERT INTO favorites VALUES ('demo', 'gauben', 'gibtsnicht')")
          .attempt
      yield
        first should include("Zimmerei Erfunden GmbH")
        counts shouldBe List(1, 1, 1)
        orphanFavorite.isLeft shouldBe true
        html should include("Zimmerei Erfunden GmbH")
        html should include("gezählt (Favorit)")
        html should include("Pos. 1: Dachfenster liefern")
    }
  }

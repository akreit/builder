package builder

import builder.domain.*
import builder.extraction.*
import builder.repo.*
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import fs2.Stream
import org.http4s.*
import org.http4s.multipart.Multiparts
import org.http4s.multipart.Part
import org.scalatest.EitherValues
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class LeistungenSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers with EitherValues:
  import RawOffers.*

  private val pdf: Array[Byte] = "%PDF-1.7\nfiktives Angebot".getBytes("UTF-8")
  private val fenster = CategoryId.from("dachfenster").value
  private def lid(s: String) = LeistungId.from(s).value
  private def leistung(id: String, name: String) =
    Leistung(lid(id), fenster, LeistungName.from(name).value)
  private val seeded = SampleData.leistungen.value.map(_._2)

  // ---- pure: resolving the extractor's answer ------------------------------------------------

  "a plan maps items onto known Leistungen and adds proposed ones with fresh ids" in {
    val plan = LeistungPlan.plan(dachfensterMitLeistungen, fenster, seeded, LazyList(lid("l-neu")))
    plan.added shouldBe List(leistung("l-neu", "Gerüst"))
    plan.items shouldBe List(Some(lid("dachfenster-fenster")), Some(lid("l-neu")))
    plan.coverage.map(c => c.leistung.value -> c.coverage) should contain theSameElementsAs List(
      "dachfenster-fenster" -> Coverage.Enthalten,
      "dachfenster-einbau" -> Coverage.Unklar,
      "dachfenster-innenausbau" -> Coverage.Ausgeschlossen,
      "l-neu" -> Coverage.Enthalten
    )
  }

  "a proposed Leistung named like an existing one is mapped onto it, not added twice" in {
    val raw = dachfensterMitLeistungen.copy(neueLeistungen =
      Some(List(RawNeueLeistung("neu-1", " einbau ")))
    )
    val plan = LeistungPlan.plan(raw, fenster, seeded, LazyList(lid("l-neu")))
    plan.added shouldBe Nil
    plan.items shouldBe List(Some(lid("dachfenster-fenster")), Some(lid("dachfenster-einbau")))
  }

  "unknown references stay unmapped and unknown statuses become 'unklar'" in {
    val raw = dachfenster.copy(
      lineItems = dachfenster.lineItems.map(_.copy(leistung = Some("gibt-es-nicht"))),
      abdeckung = Some(List(RawAbdeckung("dachfenster-einbau", "vielleicht", 0, Confidence.High)))
    )
    val plan = LeistungPlan.plan(raw, fenster, seeded, LazyList.empty)
    plan.items shouldBe List(None, None)
    plan.coverage.map(c => (c.coverage, c.page)) shouldBe List((Coverage.Unklar, None))
  }

  // ---- pure: matrix cells --------------------------------------------------------------------

  private val o = Fixtures.offerId("a")
  private def read(c: Coverage, conf: Confidence = Confidence.High) =
    Some(CoverageEntry(o, lid("x"), c, CoverageSource.Read(PageNumber.from(2).toOption, conf)))

  "a cell sums the prices of its items" in {
    val cell = Matrix.cell(
      read(Coverage.Enthalten),
      List(0, 1),
      List(Some(Fixtures.eur(100)), Some(Fixtures.eur(250))),
      Currency.EUR
    )
    (cell.coverage, cell.price, cell.flagged) shouldBe (
      Coverage.Enthalten,
      Some(Fixtures.eur(350)),
      false
    )
  }

  "unclear, low-confidence and contradicting cells are flagged, never guessed" in {
    Matrix.cell(read(Coverage.Unklar), Nil, Nil, Currency.EUR).flagged shouldBe true
    Matrix
      .cell(read(Coverage.Enthalten, Confidence.Low), Nil, Nil, Currency.EUR)
      .flagged shouldBe true
    Matrix
      .cell(read(Coverage.NichtEnthalten), List(0), List(Some(Fixtures.eur(1))), Currency.EUR)
      .flagged shouldBe true
    val nothing = Matrix.cell(None, Nil, Nil, Currency.EUR)
    (nothing.coverage, nothing.flagged) shouldBe (Coverage.Unklar, true)
  }

  "without a stated status, mapped items mean 'enthalten'; a manual status wins and is not flagged" in {
    Matrix
      .cell(None, List(0), List(Some(Fixtures.eur(5))), Currency.EUR)
      .coverage shouldBe Coverage.Enthalten
    val manual = Some(CoverageEntry(o, lid("x"), Coverage.NichtEnthalten, CoverageSource.Manual))
    val cell = Matrix.cell(manual, List(0), List(Some(Fixtures.eur(5))), Currency.EUR)
    (cell.coverage, cell.source, cell.flagged) shouldBe (
      Coverage.NichtEnthalten,
      CoverageSource.Manual,
      false
    )
  }

  "a manual item assignment and coverage win over extracted ones" in {
    val l1 = lid("l1")
    val l2 = lid("l2")
    Matrix.effective(
      List(
        ItemAssignment(o, 0, Some(l1), manual = false),
        ItemAssignment(o, 0, Some(l2), manual = true)
      )
    ) shouldBe Map((o, 0) -> Some(l2))
    Matrix
      .effectiveCoverage(
        List(
          CoverageEntry(o, l1, Coverage.Enthalten, CoverageSource.Manual),
          CoverageEntry(o, l1, Coverage.Unklar, CoverageSource.Read(None, Confidence.High))
        )
      )
      .get((o, l1))
      .map(_.coverage) shouldBe Some(Coverage.Enthalten)
  }

  // ---- through the app -----------------------------------------------------------------------

  private def run(app: TestApp, req: Request[IO]): IO[(Status, String)] =
    app.http.run(req).flatMap(r => r.bodyText.compile.string.map(r.status -> _))

  private def upload(app: TestApp): IO[(Status, String)] =
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
          Request[IO](
            Method.POST,
            Uri.unsafeFromString("/ui/projects/demo/categories/dachfenster/angebote")
          )
            .withEntity(mp)
            .putHeaders(mp.headers)
            .putHeaders(Header.Raw(org.typelevel.ci.CIString("HX-Request"), "true"))
        )
      )

  private def edit(app: TestApp, fields: (String, String)*): IO[(Status, String)] =
    run(
      app,
      Request[IO](Method.POST, Uri.unsafeFromString("/ui/projects/demo/aenderungen"))
        .withEntity(UrlForm(fields*))
        .putHeaders(Header.Raw(org.typelevel.ci.CIString("HX-Request"), "true"))
    )

  private def page(app: TestApp): IO[String] =
    run(app, Request[IO](Method.GET, Uri.unsafeFromString("/"))).map(_._2)

  /** Leistung names of the Dachfenster matrix, in order. */
  private def leistungen(html: String): List[(String, String)] =
    """data-leistung="([^"]+)">([^<]+)<""".r
      .findAllMatchIn(html)
      .map(m => m.group(1) -> m.group(2))
      .toList

  private def offers(html: String): List[String] =
    """data-zelle="[^|"]+\|([^"]+)"""".r
      .findAllMatchIn(html)
      .map(_.group(1))
      .toList
      .distinct

  private def status(html: String, leistung: String, offer: String): Option[String] =
    s"""data-zelle="${java.util.regex.Pattern.quote(
        s"$leistung|$offer"
      )}" data-status="([a-z_]+)"""".r
      .findFirstMatchIn(html)
      .map(_.group(1))

  private def idOf(html: String, name: String): String =
    leistungen(html).find(_._2 == name).fold("")(_._1)

  "an upload shows the category's Leistungen as a matrix and adds proposed ones" in {
    val extractor = RecordingExtractor(List(dachfensterMitLeistungen))
    TestApp.fresh(extractor).use { app =>
      for
        (st, html) <- upload(app)
        _ <- upload(app)
        known <- extractor.knownPerCall
      yield
        st shouldBe Status.Ok
        leistungen(html).map(_._2) shouldBe
          List("Fenster inkl. Zubehör", "Einbau", "Innenausbau", "Entsorgung", "Gerüst")
        val offer = offers(html).headOption.getOrElse("")
        status(html, "dachfenster-fenster", offer) shouldBe Some("enthalten")
        status(html, "dachfenster-innenausbau", offer) shouldBe Some("ausgeschlossen")
        status(html, "dachfenster-entsorgung", offer) shouldBe Some("unklar")
        html should include("3.750,00")
        // The second offer of the category is read against the grown list, without a second "Gerüst".
        known shouldBe List(
          List("Fenster inkl. Zubehör", "Einbau", "Innenausbau", "Entsorgung"),
          List("Fenster inkl. Zubehör", "Einbau", "Innenausbau", "Entsorgung", "Gerüst")
        )
    }
  }

  "a manual status survives later extractions and a restart; 'auto' brings the read one back" in {
    TestApp.tempDir.use { dir =>
      val extractor = RecordingExtractor(List(dachfensterMitLeistungen))
      for
        (_, first) <- TestApp.resource(dir, extractor).use(upload)
        offer = offers(first).headOption.getOrElse("")
        _ <- TestApp
          .resource(dir, extractor)
          .use(app =>
            edit(
              app,
              "aktion" -> "status-setzen",
              "angebot" -> offer,
              "leistung" -> "dachfenster-entsorgung",
              "status" -> "enthalten"
            ) >>
              upload(app)
          )
        restarted <- TestApp.resource(dir, extractor).use(page)
        (_, reset) <- TestApp
          .resource(dir, extractor)
          .use(app =>
            edit(
              app,
              "aktion" -> "status-setzen",
              "angebot" -> offer,
              "leistung" -> "dachfenster-entsorgung",
              "status" -> "auto"
            )
          )
      yield
        status(restarted, "dachfenster-entsorgung", offer) shouldBe Some("enthalten")
        status(reset, "dachfenster-entsorgung", offer) shouldBe Some("unklar")
    }
  }

  "Leistungen can be renamed, merged and deleted; items can be reassigned" in {
    TestApp.fresh(RecordingExtractor(List(dachfensterMitLeistungen))).use { app =>
      for
        (_, html) <- upload(app)
        offer = offers(html).headOption.getOrElse("")
        geruest = idOf(html, "Gerüst")
        (_, renamed) <- edit(
          app,
          "aktion" -> "leistung-umbenennen",
          "leistung" -> geruest,
          "name" -> "Montage"
        )
        (_, merged) <- edit(
          app,
          "aktion" -> "leistung-zusammenfuehren",
          "leistung" -> geruest,
          "ziel" -> "dachfenster-einbau"
        )
        (_, reassigned) <- edit(
          app,
          "aktion" -> "position-zuordnen",
          "angebot" -> offer,
          "position" -> "1",
          "ziel" -> ""
        )
        (_, deleted) <- edit(
          app,
          "aktion" -> "leistung-loeschen",
          "leistung" -> "dachfenster-innenausbau"
        )
      yield
        leistungen(renamed).map(_._2) should contain("Montage")
        leistungen(merged).map(_._2) should not contain "Montage"
        // The merged item now counts for "Einbau", which turns from "unklar" to "enthalten".
        status(merged, "dachfenster-einbau", offer) shouldBe Some("enthalten")
        reassigned should include("Nicht zugeordnet")
        reassigned should include("Pos. 1: Dachfenster liefern")
        leistungen(deleted).map(_._2) shouldBe List("Fenster inkl. Zubehör", "Einbau", "Entsorgung")
    }
  }

  "categories can be added, renamed and deleted, but not while they hold offers" in {
    TestApp.fresh(RecordingExtractor(List(dachfensterMitLeistungen))).use { app =>
      for
        (_, added) <- edit(app, "aktion" -> "kategorie-neu", "name" -> "Gerüstbau")
        newId = """data-kategorie="(k-[^"]+)"""".r.findFirstMatchIn(added).fold("")(_.group(1))
        (_, renamed) <- edit(
          app,
          "aktion" -> "kategorie-umbenennen",
          "kategorie" -> newId,
          "name" -> "Gerüst"
        )
        (deletedStatus, deleted) <- edit(
          app,
          "aktion" -> "kategorie-loeschen",
          "kategorie" -> newId
        )
        _ <- upload(app)
        (usedStatus, used) <- edit(
          app,
          "aktion" -> "kategorie-loeschen",
          "kategorie" -> "dachfenster"
        )
        (seedStatus, _) <- edit(app, "aktion" -> "kategorie-loeschen", "kategorie" -> "gauben")
      yield
        added should include("Gerüstbau")
        renamed should include("<h2>Gerüst<")
        deletedStatus shouldBe Status.Ok
        deleted should not include newId
        usedStatus shouldBe Status.Conflict
        used should include("hochgeladene Angebote")
        seedStatus shouldBe Status.Conflict
    }
  }

  "edits need an htmx request and valid fields" in {
    TestApp.fresh(RecordingExtractor(Nil)).use { app =>
      for
        (plain, _) <- run(
          app,
          Request[IO](Method.POST, Uri.unsafeFromString("/ui/projects/demo/aenderungen"))
            .withEntity(UrlForm("aktion" -> "kategorie-neu", "name" -> "X"))
        )
        (unknown, text) <- edit(app, "aktion" -> "zaubern")
        (badStatus, _) <- edit(
          app,
          "aktion" -> "status-setzen",
          "angebot" -> "upl-x",
          "leistung" -> "dachfenster-einbau",
          "status" -> "vielleicht"
        )
        (missing, _) <- edit(
          app,
          "aktion" -> "leistung-umbenennen",
          "leistung" -> "nix",
          "name" -> "Y"
        )
      yield
        plain shouldBe Status.Forbidden
        unknown shouldBe Status.BadRequest
        text should include("Unbekannte Aktion")
        badStatus shouldBe Status.BadRequest
        missing shouldBe Status.NotFound
    }
  }

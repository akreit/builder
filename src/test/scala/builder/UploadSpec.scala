package builder

import builder.RawOffers.CountingExtractor
import builder.extraction.*
import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import fs2.Stream
import io.circe.parser.parse
import org.http4s.*
import org.http4s.implicits.*
import org.http4s.multipart.Multiparts
import org.http4s.multipart.Part
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

class UploadSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers:
  import RawOffers.*

  private val pdf: Array[Byte] = "%PDF-1.7\nfiktives Angebot".getBytes("UTF-8")
  private val png: Array[Byte] =
    Array(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0).map(_.toByte)

  private def upload(
      app: TestApp,
      bytes: Array[Byte],
      name: String = "angebot.pdf",
      category: String = "fenster",
      headers: List[(String, String)] = List("HX-Request" -> "true")
  ): IO[(Status, String)] =
    Multiparts
      .forSync[IO]
      .flatMap(
        _.multipart(Vector(Part.fileData[IO]("datei", name, Stream.emits(bytes.toIndexedSeq))))
      )
      .flatMap { mp =>
        val req = Request[IO](
          Method.POST,
          Uri.unsafeFromString(s"/ui/projects/demo/categories/$category/angebote")
        ).withEntity(mp)
          .putHeaders(mp.headers)
          .putHeaders(headers.map(Header.ToRaw.keyValuesToRaw)*)
        app.http.run(req).flatMap(r => r.bodyText.compile.string.map(r.status -> _))
      }

  private def net(app: TestApp): IO[String] =
    app.http
      .run(Request[IO](Method.GET, uri"/projects/demo/totals"))
      .flatMap(_.bodyText.compile.string)
      .map(b => parse(b).flatMap(_.hcursor.downField("net").as[String]).getOrElse(""))

  private def withApp[A](result: Either[ExtractionError, RawOffer])(f: TestApp => IO[A]): IO[A] =
    TestApp.fresh(FakeExtractor(result)).use(f)

  "uploading a consistent offer PDF shows it with page refs and flags and updates the total" in {
    withApp(Right(dachfenster)) { app =>
      for
        before <- net(app)
        (status, body) <- upload(app, pdf)
        after <- net(app)
      yield
        before shouldBe "21118.50"
        status shouldBe Status.Ok
        body should include("Zimmerei Erfunden GmbH")
        body should include("Ausgelesen und geprüft")
        body should include("S. 2")
        body should include("⚠ prüfen")
        body should include("Position 1: Modell")
        body should include("5.961,90 €")
        // 21.118,50 € + 5.010,00 € (only offer in Dachfenster, so it counts)
        body should include("26.128,50 €")
        after shouldBe "26128.50"
    }
  }

  "the uploaded file is stored in the configured directory" in {
    withApp(Right(dachfenster)) { app =>
      upload(app, pdf) >> IO.blocking(
        Files.list(app.dir.resolve("angebote")).toArray.toList.map(_.toString)
      )
    }.asserting(files => files.exists(_.endsWith(".pdf")) shouldBe true)
  }

  "an offer whose amounts do not add up is shown with the mismatch and not counted" in {
    val wrong = dachfenster.copy(gross = f("5999.00", 3))
    withApp(Right(wrong)) { app =>
      for
        (status, body) <- upload(app, png, "foto.png")
        after <- net(app)
      yield
        status shouldBe Status.Ok
        body should include("Abweichungen gefunden")
        body should include("Brutto 5.999,00 € ≠ Netto 5.010,00 € + MwSt. 951,90 €")
        body should include("Zimmerei Erfunden GmbH")
        after shouldBe "21118.50"
    }
  }

  "a failed extraction is shown and nothing is counted" in {
    withApp(Left(ExtractionError.HttpStatus(529, Some("overloaded_error")))) { app =>
      for
        (status, body) <- upload(app, pdf)
        after <- net(app)
      yield
        status shouldBe Status.Ok
        body should include("Auslesen fehlgeschlagen")
        body should include("Status 529 (overloaded_error)")
        after shouldBe "21118.50"
    }
  }

  "files that are neither PDF nor JPEG/PNG are rejected with 415" in {
    withApp(Right(dachfenster)) { app =>
      upload(app, "hallo".getBytes("UTF-8"), "angebot.txt").map(_._1)
    }.asserting(_ shouldBe Status.UnsupportedMediaType)
  }

  "uploading into an unknown category is 404" in {
    withApp(Right(dachfenster))(app => upload(app, pdf, category = "gibtsnicht").map(_._1))
      .asserting(_ shouldBe Status.NotFound)
  }

  "uploads and favorites survive a restart" in {
    def favorite(app: TestApp) = app.http.run(
      Request[IO](
        Method.POST,
        uri"/ui/projects/demo/categories/fassadendaemmung/favorite/fassade-b"
      )
        .putHeaders("HX-Request" -> "true")
    )
    val second = dachfenster.copy(contractor = f("Dachbau Phantasie OHG"))
    TestApp.tempDir.use(dir =>
      for
        _ <- TestApp.resource(dir, FakeExtractor(Right(dachfenster))).use(app => upload(app, pdf))
        _ <- TestApp
          .resource(dir, FakeExtractor(Right(second)))
          .use(app => upload(app, pdf) >> favorite(app))
        reopened <- TestApp.resource(dir, FakeExtractor(Right(dachfenster))).use { app =>
          app.http
            .run(Request[IO](Method.GET, uri"/projekte/demo/kategorien/fenster"))
            .flatMap(_.bodyText.compile.string)
            .product(net(app))
        }
      yield
        val (page, total) = reopened
        page should include("Zimmerei Erfunden GmbH")
        page should include("Dachbau Phantasie OHG")
        // Two Dachfenster offers, no favorite: undecided. Dämmung favorite fassade-b counts.
        total shouldBe "31906.50"
    )
  }

  "uploading the same firm again in the same category overwrites the existing offer" in {
    val updated = dachfenster.copy(
      net = f("6000.00", 3),
      vat = f("1140.00", 3),
      gross = f("7140.00", 3),
      lineItems = List(item("Dachfenster liefern", "3", "Stk", "2000.00", "6000.00"))
    )
    TestApp.fresh(RecordingExtractor(List(dachfenster, updated))).use { app =>
      for
        (st1, body1) <- upload(app, pdf, "alt.pdf")
        id1 <- IO.pure {
          val m = """data-upload="([^"]+)"""".r.findFirstMatchIn(body1).map(_.group(1))
          m.getOrElse("")
        }
        (st2, body2) <- upload(app, pdf, "neu.pdf")
        after <- net(app)
        files <- storedFiles(app)
      yield
        st1 shouldBe Status.Ok
        st2 shouldBe Status.Ok
        body1 should include("Zimmerei Erfunden GmbH")
        body2 should include("Zimmerei Erfunden GmbH")
        body2 should include("neu.pdf")
        body2 should include("6.000,00")
        // Still a single upload of that firm; the old file name is gone.
        body2 should not include ("alt.pdf")
        """data-upload="""".r.findAllIn(body2).size shouldBe 1
        body2 should include(s"""data-upload="$id1"""")
        // Only offer in Fenster: counts the new net.
        after shouldBe "27118.50"
        files.count(_.endsWith(".pdf")) shouldBe 1
    }
  }

  "the same firm in a different category creates a new offer" in {
    def page(app: TestApp, category: String): IO[String] =
      app.http
        .run(Request[IO](Method.GET, Uri.unsafeFromString(s"/projekte/demo/kategorien/$category")))
        .flatMap(_.bodyText.compile.string)
    TestApp.fresh(FakeExtractor(Right(dachfenster))).use { app =>
      for
        (st1, body1) <- upload(app, pdf, category = "fenster")
        (st2, body2) <- upload(app, pdf, category = "dach")
        fensterPage <- page(app, "fenster")
        dachPage <- page(app, "dach")
        fensterIds = """data-upload="([^"]+)"""".r.findAllMatchIn(fensterPage).map(_.group(1)).toSet
        dachIds = """data-upload="([^"]+)"""".r.findAllMatchIn(dachPage).map(_.group(1)).toSet
      yield
        st1 shouldBe Status.Ok
        st2 shouldBe Status.Ok
        body1 should include("Zimmerei Erfunden GmbH")
        body2 should include("Zimmerei Erfunden GmbH")
        fensterIds.size shouldBe 1
        dachIds.size shouldBe 1
        fensterIds.intersect(dachIds) shouldBe empty
    }
  }

  "a different firm in the same category creates a new offer" in {
    val other = dachfenster.copy(contractor = f("Dachbau Phantasie OHG"))
    TestApp.fresh(RecordingExtractor(List(dachfenster, other))).use { app =>
      for
        (_, body1) <- upload(app, pdf)
        (_, body2) <- upload(app, pdf)
      yield
        body2 should include("Zimmerei Erfunden GmbH")
        body2 should include("Dachbau Phantasie OHG")
        """data-upload="""".r.findAllIn(body2).size shouldBe 2
        body1 should include("Zimmerei Erfunden GmbH")
    }
  }

  "same firm ignoring case and spaces overwrites; favorite keeps the offer id" in {
    val sameFirm = dachfenster.copy(
      contractor = f("  zimmerei   ERFUNDEN gmbh "),
      net = f("5500.00", 3),
      vat = f("1045.00", 3),
      gross = f("6545.00", 3),
      lineItems = List(item("Dachfenster liefern", "2", "Stk", "2750.00", "5500.00"))
    )
    def favorite(app: TestApp, offer: String) = app.http.run(
      Request[IO](
        Method.POST,
        Uri.unsafeFromString(s"/ui/projects/demo/categories/fenster/favorite/$offer")
      ).putHeaders("HX-Request" -> "true")
    )
    TestApp.fresh(RecordingExtractor(List(dachfenster, sameFirm))).use { app =>
      for
        (_, body1) <- upload(app, pdf)
        offerId <- IO.pure(
          """data-upload="([^"]+)"""".r.findFirstMatchIn(body1).map(_.group(1)).getOrElse("")
        )
        _ <- favorite(app, offerId)
        (_, body2) <- upload(app, pdf, "ersetzt.pdf")
        after <- net(app)
      yield
        body2 should include(s"""data-upload="$offerId"""")
        """data-upload="""".r.findAllIn(body2).size shouldBe 1
        body2 should include("Favorit")
        body2 should include("ersetzt.pdf")
        after shouldBe "26618.50"
    }
  }

  "manual coverage and item assignment survive a same-firm overwrite" in {
    val updated = dachfensterMitLeistungen.copy(
      net = f("6000.00", 3),
      vat = f("1140.00", 3),
      gross = f("7140.00", 3),
      lineItems = List(
        dachfensterMitLeistungen.lineItems(0).copy(leistung = Some("fenster-fenster")),
        dachfensterMitLeistungen.lineItems(1).copy(leistung = Some("fenster-einbau"))
      ),
      neueLeistungen = None,
      abdeckung = dachfensterMitLeistungen.abdeckung.map(
        _.map(a =>
          if a.leistung == "fenster-entsorgung" then a.copy(status = "nicht_enthalten")
          else if a.leistung == "fenster-einbau" then a.copy(status = "enthalten")
          else a
        )
      )
    )
    TestApp
      .fresh(RecordingExtractor(List(dachfensterMitLeistungen, updated)))
      .use { app =>
        for
          (_, body1) <- upload(app, pdf, "alt.pdf")
          offer = uploadId(body1)
          _ <- edit(
            app,
            "aktion" -> "status-setzen",
            "angebot" -> offer,
            "leistung" -> "fenster-entsorgung",
            "status" -> "enthalten"
          )
          _ <- edit(
            app,
            "aktion" -> "position-zuordnen",
            "angebot" -> offer,
            "position" -> "1",
            "ziel" -> ""
          )
          (_, body2) <- upload(app, pdf, "neu.pdf")
          afterId = uploadId(body2)
        yield
          afterId shouldBe offer
          """data-upload="""".r.findAllIn(body2).size shouldBe 1
          body2 should include("neu.pdf")
          coverageStatus(body2, "fenster-entsorgung", offer) shouldBe Some("enthalten")
          body2 should include("manuell")
          body2 should include("Nicht zugeordnet")
          body2 should include("Pos. 1: Dachfenster liefern")
          // Overwrite did apply: second line item now counts under Einbau.
          body2 should include("6.000,00")
      }
  }

  "two concurrent same-firm uploads keep one offer id and one matching file" in {
    val pdfA = "%PDF-1.7\nAAA-same-firm".getBytes("UTF-8")
    val pdfB = "%PDF-1.7\nBBB-same-firm".getBytes("UTF-8")
    for
      ready <- Ref[IO].of(0)
      go <- Deferred[IO, Unit]
      extractor = new OfferExtractor[IO]:
        def extract(
            file: Array[Byte],
            fileType: OfferFileType,
            known: List[builder.domain.Leistung]
        ) =
          ready.modify(n => (n + 1, n + 1)).flatMap { n =>
            val open = if n >= 2 then go.complete(()).void else IO.unit
            open >> go.get.as(Right(dachfenster): Either[ExtractionError, RawOffer])
          }
      result <- TestApp.fresh(extractor).use { app =>
        (upload(app, pdfA, "a.pdf"), upload(app, pdfB, "b.pdf")).parTupled.flatMap {
          case ((st1, body1), (st2, body2)) =>
            for
              files <- storedFiles(app)
              pdfs = files.filter(_.endsWith(".pdf"))
              bytes <- pdfs.traverse(p => IO.blocking(Files.readAllBytes(java.nio.file.Path.of(p))))
              ids =
                ("""data-upload="([^"]+)"""".r.findAllMatchIn(body1 ++ body2).map(_.group(1)).toSet)
            yield (st1, st2, ids, pdfs.size, bytes)
        }
      }
    yield
      val (st1, st2, ids, pdfCount, contents) = result
      st1 shouldBe Status.Ok
      st2 shouldBe Status.Ok
      ids.size shouldBe 1
      pdfCount shouldBe 1
      contents.size shouldBe 1
      val kept = contents.head
      (kept.sameElements(pdfA) || kept.sameElements(pdfB)) shouldBe true
  }

  private def edit(app: TestApp, fields: (String, String)*): IO[(Status, String)] =
    app.http
      .run(
        Request[IO](Method.POST, Uri.unsafeFromString("/ui/projects/demo/aenderungen"))
          .withEntity(UrlForm((("ansicht" -> "fenster") +: fields)*))
          .putHeaders("HX-Request" -> "true")
      )
      .flatMap(r => r.bodyText.compile.string.map(r.status -> _))

  private def coverageStatus(html: String, leistung: String, offer: String): Option[String] =
    s"""data-zelle="${java.util.regex.Pattern.quote(
        s"$leistung|$offer"
      )}" data-status="([a-z_]+)"""".r
      .findFirstMatchIn(html)
      .map(_.group(1))

  private def uploadId(body: String): String =
    """data-upload="([^"]+)"""".r.findFirstMatchIn(body).fold("")(_.group(1))

  private def storedFiles(app: TestApp): IO[List[String]] =
    IO.blocking {
      val dir = app.dir.resolve("angebote")
      if Files.exists(dir) then
        scala.util.Using.resource(Files.list(dir))(_.toArray.toList.map(_.toString))
      else Nil
    }

  "an upload without HX-Request or from a foreign origin is 403 and nothing is read or stored" in {
    val counting = CountingExtractor(Right(dachfenster))
    TestApp.fresh(counting).use { app =>
      for
        (noHx, _) <- upload(app, pdf, headers = Nil)
        (foreign, body) <- upload(
          app,
          pdf,
          headers = List("HX-Request" -> "true", "Origin" -> "https://evil.example")
        )
        calls <- counting.calls
        files <- storedFiles(app)
      yield
        noHx shouldBe Status.Forbidden
        foreign shouldBe Status.Forbidden
        body should include("fremde Herkunft")
        calls shouldBe 0
        files shouldBe Nil
    }
  }

  "a request body over 20 MB is 413 and the extractor is not called" in {
    val counting = CountingExtractor(Right(dachfenster))
    val big = pdf ++ Array.fill(21 * 1024 * 1024)('x'.toByte)
    TestApp.fresh(counting).use { app =>
      for
        (status, body) <- upload(app, big)
        calls <- counting.calls
        files <- storedFiles(app)
      yield
        status shouldBe Status.PayloadTooLarge
        body should include("Datei zu groß")
        calls shouldBe 0
        files shouldBe Nil
    }
  }

  "an image over 5 MB is 413 before the extractor is called" in {
    val counting = CountingExtractor(Right(dachfenster))
    val bigPng = png ++ Array.fill(6 * 1024 * 1024)(0.toByte)
    TestApp.fresh(counting).use { app =>
      for
        (status, body) <- upload(app, bigPng, "foto.png")
        calls <- counting.calls
      yield
        status shouldBe Status.PayloadTooLarge
        body should include("PNG höchstens 5 MB")
        calls shouldBe 0
    }
  }

  "when reading fails with an error, the saved file is removed again" in {
    val failing = new OfferExtractor[IO]:
      def extract(
          file: Array[Byte],
          fileType: OfferFileType,
          known: List[builder.domain.Leistung]
      ) =
        IO.raiseError(IllegalStateException("kaputt"))
    TestApp.fresh(failing).use { app =>
      for
        (status, _) <- upload(app, pdf)
        files <- storedFiles(app)
      yield
        status shouldBe Status.InternalServerError
        files shouldBe Nil
    }
  }

  private def sql(dir: java.nio.file.Path, statements: String*): IO[Unit] =
    TestDb.sql(dir, statements*)

  private def page(app: TestApp): IO[String] =
    app.http
      .run(Request[IO](Method.GET, uri"/projekte/demo/kategorien/fenster"))
      .flatMap(_.bodyText.compile.string)

  "an unreadable stored extraction is shown as failed and its favorite is reported as ignored" in {
    TestApp.tempDir.use(dir =>
      for
        _ <- TestApp.resource(dir, FakeExtractor(Right(dachfenster))).use(app => upload(app, pdf))
        _ <- sql(
          dir,
          "UPDATE uploads SET extraction = '{kaputt'",
          "INSERT INTO favorites SELECT project_id, category_id, id FROM uploads"
        )
        result <- TestApp
          .resource(dir, FakeExtractor(Right(dachfenster)))
          .use(app => page(app).product(net(app)))
      yield
        val (html, total) = result
        html should include("Auslesen fehlgeschlagen")
        html should include("gespeicherte Daten nicht lesbar")
        html should include("Gespeicherter Favorit")
        html should include("wird ignoriert")
        total shouldBe "21118.50"
    )
  }

  "a restart applies no migration twice and keeps all rows" in {
    val other = dachfenster.copy(contractor = f("Andere Fensterbau KG"))
    TestApp.tempDir.use(dir =>
      for
        _ <- TestApp.resource(dir, FakeExtractor(Right(dachfenster))).use(app => upload(app, pdf))
        _ <- TestApp.resource(dir, FakeExtractor(Right(other))).use(app => upload(app, pdf))
        versions <- TestDb.ints(dir, "SELECT version FROM schema_migrations ORDER BY version")
        uploads <- uploadRows(dir)
      yield
        versions shouldBe versions.distinct
        versions should not be empty
        uploads shouldBe 2
    )
  }

  "two starts migrating an empty database in parallel apply each migration once" in {
    List
      .range(0, 5)
      .traverse(_ =>
        TestApp.tempDir.use(dir =>
          val start = TestApp.resource(dir, FakeExtractor(Right(dachfenster))).use(page)
          for
            pages <- (start, start).parTupled
            versions <- TestDb.ints(dir, "SELECT version FROM schema_migrations ORDER BY version")
          yield (
            pages._1.contains("Datenbankfehler"),
            pages._2.contains("Datenbankfehler"),
            versions
          )
        )
      )
      .map(_ shouldBe List.fill(5)((false, false, List(1, 2))))
  }

  "a lost database connection is shown as a storage error, not a crash" in {
    TestApp.tempDir.use(dir =>
      TestApp.resource(dir, FakeExtractor(Right(dachfenster))).use { app =>
        for
          before <- page(app)
          _ <- TestDb.terminateConnections(dir)
          response <- app.http.run(Request[IO](Method.GET, uri"/projekte/demo/kategorien/fenster"))
          body <- response.bodyText.compile.string
        yield
          before should not include "Datenbankfehler"
          response.status shouldBe Status.InternalServerError
          body should include("Datenbankfehler (Code 08000)")
      }
    )
  }

  private def uploadRows(dir: java.nio.file.Path): IO[Int] =
    TestDb.ints(dir, "SELECT count(*)::int FROM uploads").map(_.sum)

  "cancelling an upload while it is being read leaves neither file nor row" in {
    for
      started <- cats.effect.Deferred[IO, Unit]
      hanging = new OfferExtractor[IO]:
        def extract(
            file: Array[Byte],
            fileType: OfferFileType,
            known: List[builder.domain.Leistung]
        ) =
          started.complete(()) >> IO.never
      result <- TestApp.tempDir.use(dir =>
        TestApp.resource(dir, hanging).use { app =>
          for
            fiber <- upload(app, pdf).start
            _ <- started.get
            savedWhileReading <- storedFiles(app)
            _ <- fiber.cancel
            files <- storedFiles(app)
            rows <- uploadRows(dir)
          yield (savedWhileReading.size, files, rows)
        }
      )
    // Reading runs before the file is stored, so a cancel mid-read never leaves a file.
    yield result shouldBe (0, Nil, 0)
  }

  "when the row cannot be inserted, the saved file is deleted" in {
    TestApp.tempDir.use(dir =>
      for
        _ <- TestApp.resource(dir, FakeExtractor(Right(dachfenster))).use(_ => IO.unit)
        _ <- sql(
          dir,
          """CREATE FUNCTION nein() RETURNS trigger LANGUAGE plpgsql AS
            |$$ BEGIN RAISE EXCEPTION 'nein'; END $$""".stripMargin,
          "CREATE TRIGGER kein_upload BEFORE INSERT ON uploads FOR EACH ROW EXECUTE FUNCTION nein()"
        )
        result <- TestApp.resource(dir, FakeExtractor(Right(dachfenster))).use { app =>
          for
            (status, body) <- upload(app, pdf)
            files <- storedFiles(app)
          yield (status, body, files)
        }
        rows <- uploadRows(dir)
      yield
        val (status, body, files) = result
        status shouldBe Status.InternalServerError
        body should include("Datenbankfehler")
        files shouldBe Nil
        rows shouldBe 0
    )
  }

  "favorites reference offers by (project, id): another project's offer id is rejected" in {
    TestApp.tempDir.use(dir =>
      for
        _ <- TestApp.resource(dir, FakeExtractor(Right(dachfenster))).use(_ => IO.unit)
        // Same offer id, other project: no matching (project_id, id) row in offers.
        foreign <- sql(
          dir,
          "INSERT INTO favorites VALUES ('anderes', 'fassadendaemmung', 'fassade-b')"
        ).attempt
        own <- sql(dir, "INSERT INTO favorites VALUES ('demo', 'dach', 'fassade-b')").attempt
      yield
        foreign.left.map(_.getMessage.contains("foreign key")) shouldBe Left(true)
        own.isRight shouldBe true
    )
  }

  "an offer with a position missing price and quantity is accepted, counted and shown collapsed" in {
    val incomplete = dachfenster.copy(
      lineItems = List(
        item("Dachfenster liefern", "3", "Stk", "1250.00", "3750.00"),
        RawLineItem(
          f("Einbau ohne Preis/Menge", 2),
          absent,
          f("Stk", 2),
          absent,
          absent,
          absent,
          absent,
          absent
        )
      )
    )
    withApp(Right(incomplete)) { app =>
      for
        before <- net(app)
        (status, body) <- upload(app, pdf)
        after <- net(app)
      yield
        status shouldBe Status.Ok
        body should include("Ausgelesen und geprüft: Netto, MwSt. und Brutto stimmen überein.")
        body should not include ("Positionssumme nicht prüfbar")
        body should not include ("Positionen, Netto, MwSt. und Brutto stimmen überein")
        body should include("Einbau ohne Preis/Menge")
        body should include("Menge fehlt")
        body should include("Preis fehlt")
        body should include("""class="positionen"""")
        body should include("<details")
        body should include("<summary")
        // Position <details> stay collapsed (no open attr); the outer "Ausgelesene Angebotsdaten" may be open.
        body should include("""<details class="position">""")
        body should not include ("""<details class="position" open""")
        // Offer still counted (only offer in Dachfenster): +5010 net
        before shouldBe "21118.50"
        after shouldBe "26128.50"
        body should include("26.128,50 €")
    }
  }

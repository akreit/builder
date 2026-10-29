package builder

import builder.RawOffers.CountingExtractor
import builder.extraction.*
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
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
      category: String = "dachfenster",
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
      Request[IO](Method.POST, uri"/ui/projects/demo/categories/daemmung/favorite/daemmung-b")
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
            .run(Request[IO](Method.GET, uri"/"))
            .flatMap(_.bodyText.compile.string)
            .product(net(app))
        }
      yield
        val (page, total) = reopened
        page should include("Zimmerei Erfunden GmbH")
        page should include("Dachbau Phantasie OHG")
        // Two Dachfenster offers, no favorite: undecided. Dämmung favorite daemmung-b counts.
        total shouldBe "31906.50"
    )
  }

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
      def extract(file: Array[Byte], fileType: OfferFileType) =
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
    IO.blocking {
      val ds = org.sqlite.SQLiteDataSource()
      ds.setUrl(s"jdbc:sqlite:${dir.resolve("builder.db")}")
      scala.util.Using.resource(ds.getConnection())(c =>
        scala.util.Using.resource(c.createStatement())(st => statements.foreach(st.execute))
      )
    }

  private def page(app: TestApp): IO[String] =
    app.http.run(Request[IO](Method.GET, uri"/")).flatMap(_.bodyText.compile.string)

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

  "a database of the first version (no schema version, unversioned JSON) is migrated" in {
    import io.circe.syntax.*
    val json = dachfenster.asJson.noSpaces.replace("'", "''")
    TestApp.tempDir.use(dir =>
      for
        _ <- sql(
          dir,
          """CREATE TABLE uploads(id TEXT PRIMARY KEY, project_id TEXT NOT NULL,
            |  category_id TEXT NOT NULL, file_name TEXT NOT NULL, file_type TEXT NOT NULL,
            |  created_at TEXT NOT NULL, extraction TEXT, failure TEXT)""".stripMargin,
          """CREATE TABLE favorites(project_id TEXT NOT NULL, category_id TEXT NOT NULL,
            |  offer_id TEXT NOT NULL, PRIMARY KEY (project_id, category_id))""".stripMargin,
          s"""INSERT INTO uploads VALUES ('upl-alt', 'demo', 'dachfenster', 'alt.pdf', 'pdf',
             |  '2026-10-01T10:00:00Z', '$json', NULL)""".stripMargin,
          "INSERT INTO favorites VALUES ('demo', 'daemmung', 'daemmung-b')"
        )
        result <- TestApp
          .resource(dir, FakeExtractor(Right(dachfenster)))
          .use(app => page(app).product(net(app)))
      yield
        val (html, total) = result
        html should include("Zimmerei Erfunden GmbH")
        html should not include "wird ignoriert"
        // 21.118,50 € + Dämmung favorite (10.788,00 €) + the migrated Dachfenster offer (5.010,00 €)
        total shouldBe "36916.50"
    )
  }

package builder

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
      category: String = "dachfenster"
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
        ).withEntity(mp).putHeaders(mp.headers).putHeaders("HX-Request" -> "true")
        app.http.run(req).flatMap(r => r.bodyText.compile.string.map(r.status -> _))
      }

  private def net(app: TestApp): IO[String] =
    app.http
      .run(Request[IO](Method.GET, uri"/projects/demo/totals"))
      .flatMap(_.bodyText.compile.string)
      .map(b => parse(b).flatMap(_.hcursor.downField("net").as[String]).getOrElse(""))

  private def withApp[A](result: Either[ExtractionError, RawOffer])(f: TestApp => IO[A]): IO[A] =
    TestApp.tempDir.flatMap(dir => TestApp.resource(dir, FakeExtractor(result)).use(f))

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
    withApp(Left(ExtractionError.HttpStatus(529, "overloaded"))) { app =>
      for
        (status, body) <- upload(app, pdf)
        after <- net(app)
      yield
        status shouldBe Status.Ok
        body should include("Auslesen fehlgeschlagen")
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
    for
      dir <- TestApp.tempDir
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
  }

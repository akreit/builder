package builder

import builder.RawOffers.*
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import fs2.Stream
import org.http4s.*
import org.http4s.implicits.*
import org.http4s.multipart.Multiparts
import org.http4s.multipart.Part
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

class DeleteOfferSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers:
  private val pdf = "%PDF-1.7\nfiktiv".getBytes("UTF-8")

  private def upload(app: TestApp): IO[(Status, String)] =
    Multiparts
      .forSync[IO]
      .flatMap(
        _.multipart(Vector(Part.fileData[IO]("datei", "a.pdf", Stream.emits(pdf.toIndexedSeq))))
      )
      .flatMap { mp =>
        app.http
          .run(
            Request[IO](Method.POST, uri"/ui/projects/demo/categories/fenster/angebote")
              .withEntity(mp)
              .putHeaders(mp.headers)
              .putHeaders("HX-Request" -> "true")
          )
          .flatMap(r => r.bodyText.compile.string.map(r.status -> _))
      }

  private def delete(
      app: TestApp,
      offer: String,
      category: String = "fenster"
  ): IO[(Status, String)] =
    app.http
      .run(
        Request[IO](
          Method.POST,
          Uri.unsafeFromString(
            s"/ui/projects/demo/categories/$category/angebote/$offer/loeschen"
          )
        ).putHeaders("HX-Request" -> "true")
      )
      .flatMap(r => r.bodyText.compile.string.map(r.status -> _))

  "deleting an uploaded offer removes it, keeps others, and drops the file" in {
    val other = dachfenster.copy(contractor = f("Andere Fensterbau KG"))
    TestApp.fresh(RecordingExtractor(List(dachfenster, other))).use { app =>
      for
        (_, body1) <- upload(app)
        id1 = """data-upload="([^"]+)"""".r.findFirstMatchIn(body1).map(_.group(1)).get
        (_, body2) <- upload(app)
        id2 = """data-upload="([^"]+)"""".r
          .findAllMatchIn(body2)
          .map(_.group(1))
          .toList
          .find(_ != id1)
          .get
        file1 = app.dir.resolve(s"angebote/$id1.pdf")
        _ <- IO.blocking(Files.exists(file1) shouldBe true)
        (st, after) <- delete(app, id1)
        gone = Files.exists(file1)
      yield
        st shouldBe Status.Ok
        after should not include id1
        after should include(id2)
        after should include("Andere Fensterbau KG")
        after should include("Löschen")
        gone shouldBe false
    }
  }

  "delete for unknown offer or wrong project is 404" in {
    TestApp.fresh(FakeExtractor(Right(dachfenster))).use { app =>
      for
        (_, body) <- upload(app)
        id = """data-upload="([^"]+)"""".r.findFirstMatchIn(body).map(_.group(1)).get
        (missing, _) <- delete(app, "gibtsnicht")
        (wrongCat, _) <- delete(app, id, "dach")
        wrongProj <- app.http
          .run(
            Request[IO](
              Method.POST,
              Uri.unsafeFromString(s"/ui/projects/nope/categories/fenster/angebote/$id/loeschen")
            ).putHeaders("HX-Request" -> "true")
          )
          .map(_.status)
      yield
        missing shouldBe Status.NotFound
        wrongCat shouldBe Status.NotFound
        wrongProj shouldBe Status.NotFound
    }
  }

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

class RecipientUploadSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers:
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

  "same firm with different recipients creates two offers" in {
    val andreas = dachfenster.copy(recipient = Some(f("Andreas")))
    val nachbar = dachfenster.copy(recipient = Some(f("Nachbar")))
    TestApp.fresh(RecordingExtractor(List(andreas, nachbar))).use { app =>
      for
        (_, b1) <- upload(app)
        (_, b2) <- upload(app)
      yield
        """data-upload="""".r.findAllIn(b2).size shouldBe 2
        b2 should include("ZimmereiErfundenGmbH_Andreas")
        b2 should include("ZimmereiErfundenGmbH_Nachbar")
        b1 should include("ZimmereiErfundenGmbH_Andreas")
    }
  }

  "same firm and same recipient overwrites" in {
    val first = dachfenster.copy(recipient = Some(f("Andreas")))
    val second = first.copy(net = f("5500.00", 3), vat = f("1045.00", 3), gross = f("6545.00", 3))
    TestApp.fresh(RecordingExtractor(List(first, second))).use { app =>
      for
        (_, b1) <- upload(app)
        id = """data-upload="([^"]+)"""".r.findFirstMatchIn(b1).map(_.group(1)).get
        (_, b2) <- upload(app)
      yield
        """data-upload="""".r.findAllIn(b2).size shouldBe 1
        b2 should include(s"""data-upload="$id"""")
        b2 should include("ZimmereiErfundenGmbH_Andreas")
    }
  }

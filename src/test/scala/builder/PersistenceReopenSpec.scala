package builder

import builder.RawOffers.*
import builder.http.Api
import builder.repo.PostgresProjectRepository
import builder.storage.FileStore
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

/** Upload/extract once, release the repository, reopen on the same DB + FileStore directory: bytes
  * and extraction must load without another extractor call (Compose volumes builder-pg /
  * builder-data provide the same guarantee across container restarts).
  */
class PersistenceReopenSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers:
  private val pdf = "%PDF-1.7\npersist".getBytes("UTF-8")
  private val sample = SampleData.project.toOption.get

  "reopening the store reloads file and extraction without calling the extractor" in {
    TestApp.tempDir.use { dir =>
      val counter = CountingExtractor(Right(dachfenster.copy(recipient = Some(f("Andreas")))))
      def open =
        TestDb
          .config(dir)
          .flatMap(cfg =>
            PostgresProjectRepository.resource(cfg, List(sample)).use { repo =>
              val http =
                Api(repo, sample.id, counter, FileStore(dir.resolve("angebote"))).routes.orNotFound
              Multiparts
                .forSync[IO]
                .flatMap(
                  _.multipart(
                    Vector(Part.fileData[IO]("datei", "p.pdf", Stream.emits(pdf.toIndexedSeq)))
                  )
                )
                .flatMap { mp =>
                  http
                    .run(
                      Request[IO](Method.POST, uri"/ui/projects/demo/categories/fenster/angebote")
                        .withEntity(mp)
                        .putHeaders(mp.headers)
                        .putHeaders("HX-Request" -> "true")
                    )
                    .flatMap(r => r.bodyText.compile.string.map(r.status -> _))
                }
            }
          )
      def reopen =
        TestDb
          .config(dir)
          .flatMap(cfg =>
            PostgresProjectRepository.resource(cfg, List(sample)).use { repo =>
              val http =
                Api(repo, sample.id, counter, FileStore(dir.resolve("angebote"))).routes.orNotFound
              http
                .run(Request[IO](Method.GET, uri"/projekte/demo/kategorien/fenster"))
                .flatMap(r => r.bodyText.compile.string.map(r.status -> _))
            }
          )
      for
        (st1, body1) <- open
        id = """data-upload="([^"]+)"""".r.findFirstMatchIn(body1).map(_.group(1)).get
        callsAfterUpload <- counter.calls
        bytes = Files.readAllBytes(dir.resolve(s"angebote/$id.pdf"))
        (st2, body2) <- reopen
        callsAfterReopen <- counter.calls
      yield
        st1 shouldBe Status.Ok
        st2 shouldBe Status.Ok
        callsAfterUpload shouldBe 1
        callsAfterReopen shouldBe 1
        bytes shouldBe pdf
        body2 should include("ZimmereiErfundenGmbH_Andreas")
        body2 should include("S. 2")
        body2 should include(id)
    }
  }

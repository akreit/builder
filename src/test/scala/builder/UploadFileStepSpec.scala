package builder

import builder.domain.*
import builder.extraction.*
import builder.repo.*
import builder.storage.FileStore
import cats.effect.Deferred
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import org.scalatest.EitherValues
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files
import java.nio.file.Path
import java.time.Instant
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** The file side of `addUpload` is installed before the DB commit and rolled back when the
  * transaction fails or is canceled, so DB row and live file always belong to the same upload.
  */
class UploadFileStepSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers with EitherValues:
  import RawOffers.*

  private val sample = SampleData.project.value
  private val fenster = CategoryId.from("fenster").value
  private val oldBytes = "%PDF-1.7\nALT".getBytes("UTF-8")
  private val newBytes = "%PDF-1.7\nNEU".getBytes("UTF-8")

  private def draft(id: String, name: String): Upload =
    Upload(
      OfferId.from(id).value,
      fenster,
      name,
      OfferFileType.Pdf,
      Instant.EPOCH,
      UploadOutcome.Read(dachfenster)
    )

  private def prepareWith(files: FileStore, staged: Path): UploadStored => IO[FileStep] = {
    case UploadStored(keep, obsolete) =>
      files
        .install(staged, keep, OfferFileType.Pdf)
        .map(i =>
          FileStep(files.rollback(i), files.finish(i) >> obsolete.traverse_(files.deleteAll))
        )
  }

  /** A repo + store with one committed `alt.pdf` upload; `f` gets the kept offer id. */
  private def withStored[A](f: (Path, ProjectRepository, FileStore, OfferId) => IO[A]): IO[A] =
    TestApp.tempDir.use(dir =>
      TestDb
        .config(dir)
        .toResource
        .flatMap(PostgresProjectRepository.resource(_, List(sample), SampleData.leistungen.value))
        .use { repo =>
          val files = FileStore(dir.resolve("angebote"))
          files
            .stage(oldBytes)
            .flatMap(staged =>
              repo
                .addUpload(sample.id, draft("u-alt", "alt.pdf"), prepareWith(files, staged))
                .flatMap(stored => f(dir, repo, files, stored.value.keep))
            )
        }
    )

  private def dbNames(repo: ProjectRepository): IO[List[String]] =
    repo.find(sample.id).map(_.value.uploads.map(_.upload.fileName))

  private def liveFiles(dir: Path): IO[List[Path]] =
    IO.blocking(
      scala.util.Using.resource(Files.list(dir.resolve("angebote")))(_.iterator.asScala.toList)
    )

  "a failing transaction after the file was installed restores the old file" in {
    withStored { (dir, repo, files, keep) =>
      for
        staged <- files.stage(newBytes)
        prepare = (s: UploadStored) =>
          prepareWith(files, staged)(s).flatTap(_ => TestDb.terminateConnections(dir))
        result <- repo.addUpload(sample.id, draft("u-neu", "neu.pdf"), prepare)
        names <- dbNames(repo)
        live <- liveFiles(dir)
        bytes <- IO.blocking(
          Files.readAllBytes(dir.resolve("angebote").resolve(s"${keep.value}.pdf"))
        )
      yield
        result.isLeft shouldBe true
        names shouldBe List("alt.pdf")
        live.map(_.getFileName.toString) shouldBe List(s"${keep.value}.pdf")
        bytes shouldBe oldBytes
    }
  }

  "cancelling while the transaction runs restores the old file and keeps the old row" in {
    withStored { (dir, repo, files, keep) =>
      for
        staged <- files.stage(newBytes)
        installed <- Deferred[IO, Unit]
        gate <- Deferred[IO, Unit]
        prepare = (s: UploadStored) =>
          prepareWith(files, staged)(s).flatTap(_ => installed.complete(()) >> gate.get)
        fiber <- repo.addUpload(sample.id, draft("u-neu", "neu.pdf"), prepare).start
        _ <- installed.get
        canceling <- fiber.cancel.start
        _ <- IO.sleep(200.millis)
        _ <- gate.complete(())
        _ <- canceling.join
        outcome <- fiber.join
        names <- dbNames(repo)
        live <- liveFiles(dir)
        bytes <- IO.blocking(
          Files.readAllBytes(dir.resolve("angebote").resolve(s"${keep.value}.pdf"))
        )
      yield
        outcome.isCanceled shouldBe true
        names shouldBe List("alt.pdf")
        live.map(_.getFileName.toString) shouldBe List(s"${keep.value}.pdf")
        bytes shouldBe oldBytes
    }
  }

  "a failing cleanup after the commit still leaves DB row and live file in agreement" in {
    withStored { (dir, repo, files, keep) =>
      for
        staged <- files.stage(newBytes)
        prepare = (s: UploadStored) =>
          prepareWith(files, staged)(s)
            .map(_.copy(finish = IO.raiseError(IllegalStateException("aufräumen kaputt"))))
        result <- repo.addUpload(sample.id, draft("u-neu", "neu.pdf"), prepare)
        names <- dbNames(repo)
        bytes <- IO.blocking(
          Files.readAllBytes(dir.resolve("angebote").resolve(s"${keep.value}.pdf"))
        )
      yield
        result.value.keep shouldBe keep
        names shouldBe List("neu.pdf")
        bytes shouldBe newBytes
    }
  }

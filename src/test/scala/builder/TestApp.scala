package builder

import builder.extraction.OfferExtractor
import builder.http.Api
import builder.repo.SqliteProjectRepository
import builder.storage.FileStore
import cats.effect.IO
import cats.effect.Resource
import org.http4s.HttpApp
import org.scalatest.EitherValues

import java.nio.file.Files
import java.nio.file.Path

/** The app over a SQLite database in `dir`, as `Main` wires it. */
final case class TestApp(http: HttpApp[IO], dir: Path)

object TestApp extends EitherValues:
  private val sample = SampleData.project.value

  def resource(dir: Path, extractor: OfferExtractor[IO]): Resource[IO, TestApp] =
    SqliteProjectRepository
      .resource(dir.resolve("builder.db"), List(sample))
      .map(repo =>
        TestApp(
          Api(repo, sample.id, extractor, FileStore(dir.resolve("angebote"))).routes.orNotFound,
          dir
        )
      )

  def tempDir: IO[Path] = IO.blocking(Files.createTempDirectory("builder-test"))

  /** A fresh app on a fresh database; the connection lives until the test JVM exits. */
  def allocated(extractor: OfferExtractor[IO]): IO[TestApp] =
    tempDir.flatMap(resource(_, extractor).allocated.map(_._1))

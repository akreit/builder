package builder

import builder.extraction.OfferExtractor
import builder.http.Api
import builder.repo.PostgresProjectRepository
import builder.storage.FileStore
import cats.effect.IO
import cats.effect.Resource
import org.http4s.HttpApp
import org.scalatest.EitherValues

import java.nio.file.Files
import java.nio.file.Path
import java.util.Comparator

/** The app over the Postgres database of `dir` (see [[TestDb]]), as `Main` wires it. */
final case class TestApp(http: HttpApp[IO], dir: Path)

object TestApp extends EitherValues:
  private val sample = SampleData.project.value

  def resource(dir: Path, extractor: OfferExtractor[IO]): Resource[IO, TestApp] =
    Resource
      .eval(TestDb.config(dir))
      .flatMap(PostgresProjectRepository.resource(_, List(sample), SampleData.leistungen.value))
      .map(repo =>
        TestApp(
          Api(repo, sample.id, extractor, FileStore(dir.resolve("angebote"))).routes.orNotFound,
          dir
        )
      )

  /** A temporary directory with its own empty database; both are deleted on release. */
  val tempDir: Resource[IO, Path] =
    directory.flatMap(dir => TestDb.database(dir).map(_ => dir))

  private lazy val directory: Resource[IO, Path] =
    Resource.make(IO.blocking(Files.createTempDirectory("builder-test")))(dir =>
      IO.blocking(
        scala.util.Using.resource(Files.walk(dir))(
          _.sorted(Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)
        )
      )
    )

  /** A fresh app on a fresh database in a temporary directory. */
  def fresh(extractor: OfferExtractor[IO]): Resource[IO, TestApp] =
    tempDir.flatMap(resource(_, extractor))

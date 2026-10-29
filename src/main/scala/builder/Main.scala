package builder

import builder.extraction.ApiKey
import builder.extraction.ClaudeOfferExtractor
import builder.extraction.OfferExtractor
import builder.http.Api
import builder.repo.SqliteProjectRepository
import builder.storage.FileStore
import cats.effect.IO
import cats.effect.IOApp
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder

import java.nio.file.Path
import scala.concurrent.duration.*

/** Configuration from the environment: `PORT`, `HOST`, `BUILDER_DATA_DIR` (SQLite database and
  * uploaded files, default `./data`), `ANTHROPIC_API_KEY`, `ANTHROPIC_MODEL`.
  */
object Main extends IOApp.Simple:
  private val port: Port =
    sys.env.lift("PORT").flatMap(Port.fromString).getOrElse(port"8080")

  private val host: Host =
    sys.env.lift("HOST").flatMap(Host.fromString).getOrElse(host"127.0.0.1")

  private val dataDir: Path = Path.of(sys.env.getOrElse("BUILDER_DATA_DIR", "data"))

  private val model: String =
    sys.env.lift("ANTHROPIC_MODEL").filter(_.nonEmpty).getOrElse(ClaudeOfferExtractor.DefaultModel)

  def run: IO[Unit] =
    val app = for
      project <- cats.effect.Resource.eval(
        IO.fromEither(
          SampleData.project.left.map(e => IllegalStateException(s"Invalid sample data: $e"))
        )
      )
      repo <- SqliteProjectRepository.resource(dataDir.resolve("builder.db"), List(project))
      claude <- ClaudeOfferExtractor.resource(ApiKey.fromEnv, model)
      extractor <- cats.effect.Resource.eval(OfferExtractor.bounded(claude, permits = 2))
      server <- EmberServerBuilder
        .default[IO]
        .withHost(host)
        .withPort(port)
        .withIdleTimeout(5.minutes)
        .withRequestHeaderReceiveTimeout(10.seconds)
        .withHttpApp(
          Api(repo, project.id, extractor, FileStore(dataDir.resolve("angebote"))).routes.orNotFound
        )
        .build
    yield server
    app
      .evalTap(s =>
        IO.println(
          s"builder listening on ${s.address} (Daten: ${dataDir.toAbsolutePath}, Modell: $model, " +
            s"API-Schlüssel ${if ApiKey.fromEnv.isDefined then "gesetzt" else "fehlt"})"
        )
      )
      .useForever

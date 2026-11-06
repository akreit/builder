package builder

import builder.extraction.ApiKey
import builder.extraction.ClaudeOfferExtractor
import builder.extraction.MultiPassOfferExtractor
import builder.extraction.OfferPass
import builder.http.Api
import builder.repo.DbConfig
import builder.repo.PostgresProjectRepository
import builder.storage.FileStore
import cats.effect.IO
import cats.effect.IOApp
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder

import java.nio.file.Path
import scala.concurrent.duration.*

/** Configuration from the environment: `PORT`, `HOST`, `BUILDER_DATA_DIR` (uploaded files, default
  * `./data`), `BUILDER_PROJECT_NAME` (home project display name), the Postgres connection
  * (`DATABASE_URL` or `DATABASE_HOST`, `DATABASE_PORT`, `DATABASE_USER`, `DATABASE_PASSWORD`,
  * `DATABASE_NAME`), `ANTHROPIC_API_KEY`, `ANTHROPIC_MODEL`.
  */
object Main extends IOApp.Simple:
  private val port: Port =
    sys.env.lift("PORT").flatMap(Port.fromString).getOrElse(port"8080")

  private val host: Host =
    sys.env.lift("HOST").flatMap(Host.fromString).getOrElse(host"127.0.0.1")

  private val dataDir: Path = Path.of(sys.env.getOrElse("BUILDER_DATA_DIR", "data"))

  private val model: String =
    sys.env.lift("ANTHROPIC_MODEL").filter(_.nonEmpty).getOrElse(ClaudeOfferExtractor.DefaultModel)

  /** The home project shell (`id=default`): structural categories, no offers or favorites. Name
    * comes from [[DefaultProject.NameEnv]].
    */
  private[builder] def defaultProject(
      env: Map[String, String] = sys.env
  ): Either[builder.domain.DomainError, builder.domain.Project] =
    DefaultProject.shell(env)

  /** IOApp entry: migrate the database, wire extractor and HTTP routes, serve until cancelled. */
  def run: IO[Unit] =
    val app = for
      shell <- cats.effect.Resource.eval(
        IO.fromEither(
          defaultProject().left.map(e => IllegalStateException(s"Invalid home project: $e"))
        )
      )
      home = shell.id
      db <- cats.effect.Resource.eval(
        IO.fromEither(
          DbConfig
            .fromEnv(sys.env)
            .left
            .map(e => IllegalStateException(s"Datenbank-Konfiguration ungültig: $e"))
        )
      )
      repo <- PostgresProjectRepository.resource(db, List(shell))
      claude <- ClaudeOfferExtractor.resource(ApiKey.fromEnv, model)
      passes <- cats.effect.Resource.eval(OfferPass.bounded(claude, permits = 2))
      extractor = MultiPassOfferExtractor(passes)
      server <- EmberServerBuilder
        .default[IO]
        .withHost(host)
        .withPort(port)
        .withIdleTimeout(5.minutes)
        .withRequestHeaderReceiveTimeout(10.seconds)
        .withHttpApp(
          Api(repo, home, extractor, FileStore(dataDir.resolve("angebote"))).routes.orNotFound
        )
        .build
    yield (server, db)
    app
      .evalTap((s, db) =>
        IO.println(
          s"builder listening on ${s.address} (Datenbank: $db, Dateien: ${dataDir.toAbsolutePath}, Modell: $model, " +
            s"API-Schlüssel ${if ApiKey.fromEnv.isDefined then "gesetzt" else "fehlt"})"
        )
      )
      .useForever

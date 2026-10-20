package builder

import builder.http.Api
import builder.repo.InMemoryProjectRepository
import cats.effect.IO
import cats.effect.IOApp
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder

object Main extends IOApp.Simple:
  private val port: Port =
    sys.env.lift("PORT").flatMap(Port.fromString).getOrElse(port"8080")

  private val host: Host =
    sys.env.lift("HOST").flatMap(Host.fromString).getOrElse(host"127.0.0.1")

  def run: IO[Unit] =
    for
      project <- IO.fromEither(
        SampleData.project.left.map(e => IllegalStateException(s"Invalid sample data: $e"))
      )
      repo <- InMemoryProjectRepository.of(List(project))
      _ <- EmberServerBuilder
        .default[IO]
        .withHost(host)
        .withPort(port)
        .withHttpApp(Api(repo, project.id).routes.orNotFound)
        .build
        .evalTap(s => IO.println(s"builder listening on ${s.address}"))
        .useForever
    yield ()

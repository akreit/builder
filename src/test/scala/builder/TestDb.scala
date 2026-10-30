package builder

import builder.repo.DbConfig
import builder.repo.Postgres
import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import com.dimafeng.testcontainers.GenericContainer
import org.testcontainers.containers.wait.strategy.Wait
import skunk.*
import skunk.codec.all.int4
import skunk.util.Origin

import java.nio.file.Path

/** A real Postgres for the tests. With `BUILDER_TEST_DATABASE_URL` set (a superuser connection,
  * e.g. `postgres://postgres:test@localhost:5432/postgres`) that server is used; otherwise one
  * `postgres:17-alpine` container is started via Testcontainers for the whole test run (needs
  * Docker). Every test directory gets its own database, dropped on release.
  */
object TestDb:
  import Postgres.given

  private lazy val started: Either[String, DbConfig] =
    sys.env.get("BUILDER_TEST_DATABASE_URL").filter(_.nonEmpty) match
      case Some(url) =>
        DbConfig.fromEnv(Map("DATABASE_URL" -> url)).left.map(e => s"BUILDER_TEST_DATABASE_URL: $e")
      case None =>
        val c = GenericContainer(
          "postgres:17-alpine",
          exposedPorts = Seq(5432),
          env = Map("POSTGRES_PASSWORD" -> "test"),
          waitStrategy = Wait.forLogMessage(".*ready to accept connections.*", 2)
        )
        c.start()
        Right(DbConfig(c.host, c.mappedPort(5432), "postgres", Some("test"), "postgres"))

  private val server: IO[DbConfig] =
    IO.blocking(started).flatMap(e => IO.fromEither(e.left.map(IllegalStateException(_))))

  /** The database belonging to the test directory `dir`. */
  def config(dir: Path): IO[DbConfig] =
    server.map(_.copy(database = nameOf(dir)))

  private def nameOf(dir: Path): String =
    "t_" + dir.getFileName.toString.toLowerCase.filter(_.isLetterOrDigit)

  private def session(config: DbConfig): Resource[IO, Session[IO]] =
    Session
      .Builder[IO]
      .withHost(config.host)
      .withPort(config.port)
      .withDatabase(config.database)
      .withCredentials(Session.Credentials(config.user, config.password))
      .single

  private def admin(statement: String): IO[Unit] =
    server.flatMap(s =>
      session(s).use(_.execute(Command(statement, Origin.unknown, Void.codec)).void)
    )

  /** Creates the database of `dir`; drops it (with open connections) on release. */
  def database(dir: Path): Resource[IO, Unit] =
    Resource.make(admin(s"CREATE DATABASE ${nameOf(dir)}"))(_ =>
      admin(s"DROP DATABASE IF EXISTS ${nameOf(dir)} WITH (FORCE)")
    )

  /** Runs `statements` in order, each on its own, against the database of `dir`. */
  def sql(dir: Path, statements: String*): IO[Unit] =
    config(dir).flatMap(c =>
      session(c).use(s =>
        statements.toList.traverse_(st => s.execute(Command(st, Origin.unknown, Void.codec)))
      )
    )

  /** The single `int` column of `query` against the database of `dir`. */
  def ints(dir: Path, query: String): IO[List[Int]] =
    config(dir).flatMap(c =>
      session(c).use(_.execute(Query(query, Origin.unknown, Void.codec, int4)))
    )

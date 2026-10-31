package builder.repo

import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.Console
import cats.syntax.all.*
import org.typelevel.otel4s.metrics.Meter
import org.typelevel.otel4s.trace.Tracer
import skunk.Command
import skunk.Query
import skunk.Session
import skunk.Void
import skunk.exception.PostgresErrorException
import skunk.exception.SkunkException

import java.net.URI

/** A Postgres SQLSTATE (five characters, e.g. `23503` for a foreign key violation). */
opaque type SqlState = String

object SqlState:
  def apply(code: String): SqlState = code
  extension (s: SqlState) def value: String = s

/** A typed command and its arguments, so statements with different parameter types can run in one
  * transaction.
  */
final case class Stmt[A](command: Command[A], args: A):
  private[repo] def run(s: Session[IO]): IO[Unit] = s.prepare(command).flatMap(_.execute(args)).void

/** Why a database configuration could not be read from the environment. */
enum DbConfigError:
  case Missing(variable: String)
  case Invalid(variable: String, reason: String)

/** Connection settings. The password is never part of `toString`. */
final case class DbConfig(
    host: String,
    port: Int,
    user: String,
    password: Option[String],
    database: String
):
  override def toString: String = s"postgres://$user@$host:$port/$database"

object DbConfig:
  /** `DATABASE_URL` (`postgres://user:pass@host:port/db`) or `DATABASE_HOST`, `DATABASE_PORT`
    * (default 5432), `DATABASE_USER`, `DATABASE_PASSWORD`, `DATABASE_NAME`.
    */
  def fromEnv(env: Map[String, String]): Either[DbConfigError, DbConfig] =
    env.get("DATABASE_URL").filter(_.nonEmpty) match
      case Some(url) => fromUrl(url)
      case None      =>
        def required(k: String) =
          env.get(k).filter(_.nonEmpty).toRight(DbConfigError.Missing(k))
        for
          host <- required("DATABASE_HOST")
          port <- env
            .get("DATABASE_PORT")
            .filter(_.nonEmpty)
            .fold(Right(5432))(p =>
              p.toIntOption.toRight(DbConfigError.Invalid("DATABASE_PORT", "keine Zahl"))
            )
          user <- required("DATABASE_USER")
          name <- required("DATABASE_NAME")
        yield DbConfig(host, port, user, env.get("DATABASE_PASSWORD").filter(_.nonEmpty), name)

  private def fromUrl(url: String): Either[DbConfigError, DbConfig] =
    val invalid =
      DbConfigError.Invalid("DATABASE_URL", "erwartet postgres://user:pass@host:port/db")
    Either
      .catchNonFatal(URI(url))
      .left
      .map(_ => invalid)
      .flatMap(uri =>
        for
          _ <- Either.cond(Set("postgres", "postgresql").contains(uri.getScheme), (), invalid)
          host <- Option(uri.getHost).filter(_.nonEmpty).toRight(invalid)
          info <- Option(uri.getUserInfo).toRight(invalid)
          db <- Option(uri.getPath).map(_.stripPrefix("/")).filter(_.nonEmpty).toRight(invalid)
        yield
          val user = info.takeWhile(_ != ':')
          val password = Some(info.dropWhile(_ != ':').drop(1)).filter(_.nonEmpty)
          DbConfig(host, if uri.getPort > 0 then uri.getPort else 5432, user, password, db)
      )

/** A pool of Skunk sessions. Postgres errors become [[RepoError.Storage]] with their SQLSTATE; lost
  * or refused connections and other Skunk protocol failures become `08000` (connection_exception),
  * so callers show a storage error instead of crashing.
  */
final class Postgres private (pool: Resource[IO, Session[IO]]):
  private def run[A](io: IO[A]): IO[Either[RepoError, A]] =
    io.attempt.flatMap {
      case Right(a)                        => IO.pure(Right(a))
      case Left(e: PostgresErrorException) => IO.pure(Left(RepoError.Storage(SqlState(e.code))))
      case Left(_: SkunkException)         => IO.pure(Left(Postgres.ConnectionLost))
      case Left(_: java.io.IOException)    => IO.pure(Left(Postgres.ConnectionLost))
      case Left(e)                         => IO.raiseError(e)
    }

  /** Runs `q` as a prepared statement with `args`. */
  def query[A, B](q: Query[A, B], args: A): IO[Either[RepoError, List[B]]] =
    run(pool.use(_.prepare(q).flatMap(_.stream(args, 256).compile.toList)))

  /** Runs all statements in one transaction: committed if all succeed, rolled back otherwise. */
  def transact(statements: List[Stmt[?]]): IO[Either[RepoError, Unit]] =
    run(pool.use(s => s.transaction.use(_ => statements.traverse_(_.run(s)))))

  /** Applies migration `version` unless `schema_migrations` already records it. The advisory lock
    * `lock` is taken first in the same transaction, before `schema_migrations` is created, so
    * concurrent starts on an empty database create the table and apply each migration once.
    */
  def migration(
      lock: Long,
      version: Int,
      statements: List[Command[Void]]
  ): IO[Either[RepoError, Unit]] =
    run(
      pool.use(s =>
        s.transaction.use(_ =>
          s.execute(Sql.lockMigrations)(lock) >>
            s.execute(Sql.createMigrationsTable) >>
            s.execute(Sql.appliedVersion)(version)
              .flatMap(applied =>
                if applied.nonEmpty then IO.unit
                else
                  statements.traverse_(c => s.execute(c)) >>
                    s.execute(Sql.recordVersion)(version).void
              )
        )
      )
    )

object Postgres:
  given Tracer[IO] = Tracer.Implicits.noop
  given Meter[IO] = Meter.Implicits.noop
  given Console[IO] = Console.make[IO]

  /** SQLSTATE class 08 (connection_exception). */
  val ConnectionLost: RepoError = RepoError.Storage(SqlState("08000"))

  /** A session pool of at most `max` connections. */
  def resource(config: DbConfig, max: Int = 10): Resource[IO, Postgres] =
    Session
      .Builder[IO]
      .withHost(config.host)
      .withPort(config.port)
      .withDatabase(config.database)
      .withCredentials(Session.Credentials(config.user, config.password))
      .pooled(max)
      .map(Postgres(_))

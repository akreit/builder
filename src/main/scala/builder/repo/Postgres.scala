package builder.repo

import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.Console
import cats.syntax.all.*
import org.typelevel.otel4s.metrics.Meter
import org.typelevel.otel4s.trace.Tracer
import skunk.*
import skunk.codec.all.*
import skunk.exception.PostgresErrorException
import skunk.implicits.*
import skunk.util.Origin

import java.net.URI
import java.time.Instant
import java.time.ZoneOffset

/** A Postgres SQLSTATE (five characters, e.g. `23503` for a foreign key violation). */
opaque type SqlState = String

object SqlState:
  def apply(code: String): SqlState = code
  extension (s: SqlState) def value: String = s

/** A statement parameter; `?` placeholders in SQL are bound to these in order. */
enum Param:
  case Text(value: Option[String])
  case Int4(value: Option[Int])
  case Bool(value: Boolean)
  case Timestamp(value: Instant)

object Param:
  def text(s: String): Param = Text(Some(s))
  def int(i: Int): Param = Int4(Some(i))

  private def encoder(p: Param): Encoder[Void] = p match
    case Text(v)      => skunk.codec.all.text.opt.contramap[Void](_ => v)
    case Int4(v)      => int4.opt.contramap[Void](_ => v)
    case Bool(v)      => bool.contramap[Void](_ => v)
    case Timestamp(v) => timestamptz.contramap[Void](_ => v.atOffset(ZoneOffset.UTC))

  private[repo] def encoderOf(params: List[Param]): Encoder[Void] =
    params
      .map(encoder)
      .foldLeft(Void.codec: Encoder[Void])((acc, e) => acc.product(e).contramap[Void](v => (v, v)))

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

/** A pool of Skunk sessions. Postgres errors become [[RepoError.Storage]] with their SQLSTATE. */
final class Postgres private (pool: Resource[IO, Session[IO]]):
  private def run[A](io: IO[A]): IO[Either[RepoError, A]] =
    io.attemptNarrow[PostgresErrorException]
      .map(_.left.map(e => RepoError.Storage(SqlState(e.code))))

  /** Replaces the `?` placeholders with `$1, $2, ...`. */
  private def numbered(sql: String): String =
    sql
      .split("\\?", -1)
      .toList
      .zipWithIndex
      .map((part, i) => if i == 0 then part else s"$$$i$part")
      .mkString

  private def exec(s: Session[IO], c: Command[Void]): IO[Unit] =
    s.prepare(c).flatMap(_.execute(Void)).void

  private def command(sql: String, params: List[Param]): Command[Void] =
    Command(numbered(sql), Origin.unknown, Param.encoderOf(params))

  def query[A](sql: String, params: List[Param])(
      decoder: Decoder[A]
  ): IO[Either[RepoError, List[A]]] =
    run(
      pool.use(
        // Prepared (extended protocol), so the parameters are bound; Void-typed `execute` would
        // use the simple protocol, which has no parameters.
        _.prepare(Query(numbered(sql), Origin.unknown, Param.encoderOf(params), decoder))
          .flatMap(_.stream(Void, 256).compile.toList)
      )
    )

  /** Runs all statements in one transaction: committed if all succeed, rolled back otherwise. */
  def transact(statements: List[(String, List[Param])]): IO[Either[RepoError, Unit]] =
    run(
      pool.use(s =>
        s.transaction.use(_ => statements.traverse_((sql, params) => exec(s, command(sql, params))))
      )
    )

  /** Applies migration `version` unless `schema_migrations` already records it. Holds the advisory
    * lock `lock` for the transaction, so concurrent starts apply each migration once.
    */
  def migration(lock: Long, version: Int, statements: List[String]): IO[Either[RepoError, Unit]] =
    run(
      pool.use(s =>
        s.transaction.use(_ =>
          s.execute(sql"SELECT 1 FROM (SELECT pg_advisory_xact_lock($int8)) AS l".query(int4))(
            lock
          ) >>
            s.execute(sql"SELECT version FROM schema_migrations WHERE version = $int4".query(int4))(
              version
            ).flatMap(applied =>
              if applied.nonEmpty then IO.unit
              else
                statements.traverse_(st => exec(s, command(st, Nil))) >>
                  s.execute(sql"INSERT INTO schema_migrations(version) VALUES ($int4)".command)(
                    version
                  ).void
            )
        )
      )
    )

object Postgres:
  given Tracer[IO] = Tracer.Implicits.noop
  given Meter[IO] = Meter.Implicits.noop
  given Console[IO] = Console.make[IO]

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

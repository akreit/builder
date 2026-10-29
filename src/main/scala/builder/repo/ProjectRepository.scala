package builder.repo

import builder.domain.*
import builder.extraction.*
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Outcome
import cats.effect.Resource
import cats.effect.std.Mutex
import cats.syntax.all.*
import io.circe.parser.decode
import io.circe.syntax.*

import java.sql.Connection
import java.sql.ResultSet
import java.sql.SQLException
import java.time.Instant
import scala.util.Try
import scala.util.Using

enum RepoError:
  case ProjectNotFound(id: ProjectId)
  case CategoryNotFound(id: CategoryId)
  case OfferNotFound(id: OfferId)
  case Invalid(error: DomainError)

  /** The database failed; `code` is the SQLite error code. */
  case Storage(code: Int)

/** What came out of reading an uploaded file. */
enum UploadOutcome:
  case Failed(message: String)
  case Read(raw: RawOffer)

/** An uploaded offer document and what the extractor read from it. Its id becomes the offer id. */
final case class Upload(
    id: OfferId,
    category: CategoryId,
    fileName: String,
    fileType: OfferFileType,
    createdAt: Instant,
    outcome: UploadOutcome
)

enum UploadStatus:
  case ExtractionFailed(message: String)

  /** Read, but missing, unreadable or inconsistent values: shown, never counted. */
  case Rejected(raw: RawOffer, issues: NonEmptyList[Issue])

  /** Validated and part of the project (and its running total). */
  case Accepted(raw: RawOffer, offer: Offer)

final case class UploadView(upload: Upload, status: UploadStatus)

/** A project including all accepted uploaded offers, plus every upload with its status, and
  * warnings about stored data that could not be used (unreadable rows, ignored favorites).
  */
final case class ProjectView(project: Project, uploads: List[UploadView], warnings: List[String])

trait ProjectRepository:
  def find(id: ProjectId): IO[Either[RepoError, ProjectView]]

  /** Marks `offer` as the favorite of `category` and returns the updated project. */
  def setFavorite(
      id: ProjectId,
      category: CategoryId,
      offer: OfferId
  ): IO[Either[RepoError, ProjectView]]

  /** Stores an uploaded offer of `id`; it is validated (and counted if valid) on every read. */
  def addUpload(id: ProjectId, upload: Upload): IO[Either[RepoError, ProjectView]]

/** One `uploads` row as stored; decoded into an [[Upload]] by [[SqliteProjectRepository]]. */
private final case class UploadRow(
    id: String,
    category: String,
    fileName: String,
    fileType: String,
    createdAt: String,
    extraction: Option[String],
    extractionVersion: Option[Int],
    failure: Option[String]
)

/** SQLite-backed repository. Projects (categories, work packages, seed offers) come from `seeds`;
  * uploads and favorites are stored in the database, so they survive restarts. Every read
  * re-validates stored extractions with the domain smart constructors.
  */
final class SqliteProjectRepository private (
    db: Sqlite,
    lock: Mutex[IO],
    seeds: Map[ProjectId, Project]
) extends ProjectRepository:
  import SqliteProjectRepository.*

  private def loadRows(id: ProjectId): IO[Either[RepoError, List[UploadRow]]] =
    db.query(
      "SELECT * FROM uploads WHERE project_id = ? ORDER BY created_at, id",
      List(Some(id.value))
    )(rs =>
      UploadRow(
        rs.getString("id"),
        rs.getString("category_id"),
        rs.getString("file_name"),
        rs.getString("file_type"),
        rs.getString("created_at"),
        Option(rs.getString("extraction")),
        Option(rs.getObject("extraction_version")).flatMap(_.toString.toIntOption),
        Option(rs.getString("failure"))
      )
    )

  private def loadFavorites(id: ProjectId): IO[Either[RepoError, List[(String, String)]]] =
    db.query(
      "SELECT category_id, offer_id FROM favorites WHERE project_id = ? ORDER BY category_id",
      List(Some(id.value))
    )(rs => rs.getString("category_id") -> rs.getString("offer_id"))

  private def status(project: Project, u: Upload): UploadStatus = u.outcome match
    case UploadOutcome.Failed(m) => UploadStatus.ExtractionFailed(m)
    case UploadOutcome.Read(raw) =>
      OfferDraft
        .validate(raw, u.id, u.category, project.currency)
        .flatMap(o =>
          project
            .withOffer(o)
            .left
            .map(e => NonEmptyList.one(Issue.Invalid("Projekt", e)))
            .map(_ => o)
        ) match
        case Right(o)     => UploadStatus.Accepted(raw, o)
        case Left(issues) => UploadStatus.Rejected(raw, issues)

  private def build(
      seed: Project,
      rows: List[UploadRow],
      favorites: List[(String, String)]
  ): ProjectView =
    val decoded = rows.map(r =>
      toUpload(r).left.map(_ => s"Gespeicherter Upload ${r.id} ist nicht lesbar und wird ignoriert")
    )
    val (project, views) =
      decoded.collect { case Right(u) => u }.foldLeft((seed, List.empty[UploadView])) {
        case ((p, acc), u) =>
          val s = status(p, u)
          val next = s match
            case UploadStatus.Accepted(_, o) => p.withOffer(o).getOrElse(p)
            case UploadStatus.Rejected(_, _) | UploadStatus.ExtractionFailed(_) => p
          (next, acc :+ UploadView(u, s))
      }
    val (withFavs, ignored) = favorites.foldLeft((project, List.empty[String])) {
      case ((p, warnings), (c, o)) =>
        (for
          cat <- CategoryId.from(c)
          offer <- OfferId.from(o)
          next <- p.withFavorite(cat, offer)
        yield next) match
          case Right(next) => (next, warnings)
          case Left(_)     =>
            (
              p,
              warnings :+ s"Gespeicherter Favorit $o für Kategorie $c ist nicht (mehr) gültig und wird ignoriert"
            )
    }
    ProjectView(withFavs, views, decoded.collect { case Left(w) => w } ++ ignored)

  private def load(id: ProjectId): IO[Either[RepoError, ProjectView]] =
    seeds.lift(id) match
      case None       => IO.pure(Left(RepoError.ProjectNotFound(id)))
      case Some(seed) =>
        for
          rows <- loadRows(id)
          favorites <- loadFavorites(id)
        yield
          for
            r <- rows
            f <- favorites
          yield build(seed, r, f)

  def find(id: ProjectId): IO[Either[RepoError, ProjectView]] = lock.lock.surround(load(id))

  def setFavorite(
      id: ProjectId,
      category: CategoryId,
      offer: OfferId
  ): IO[Either[RepoError, ProjectView]] =
    lock.lock.surround(
      load(id).flatMap {
        case Left(e)  => IO.pure(Left(e))
        case Right(v) =>
          val p = v.project
          if !p.categories.exists(_.id == category) then
            IO.pure(Left(RepoError.CategoryNotFound(category)))
          else if !p.offers.exists(_.id == offer) then IO.pure(Left(RepoError.OfferNotFound(offer)))
          else
            p.withFavorite(category, offer) match
              case Left(e)  => IO.pure(Left(RepoError.Invalid(e)))
              case Right(_) =>
                db.transact(
                  List(
                    "INSERT OR REPLACE INTO favorites(project_id, category_id, offer_id) VALUES (?, ?, ?)" ->
                      List(Some(id.value), Some(category.value), Some(offer.value))
                  )
                ).flatMap(_.fold(e => IO.pure(Left(e)), _ => load(id)))
      }
    )

  def addUpload(id: ProjectId, upload: Upload): IO[Either[RepoError, ProjectView]] =
    lock.lock.surround(
      seeds.lift(id) match
        case None => IO.pure(Left(RepoError.ProjectNotFound(id)))
        case Some(p) if !p.categories.exists(_.id == upload.category) =>
          IO.pure(Left(RepoError.CategoryNotFound(upload.category)))
        case Some(_) =>
          val (extraction, version, failure) = upload.outcome match
            case UploadOutcome.Read(raw) =>
              (Some(raw.asJson.noSpaces), Some(ExtractionVersion.toString), None)
            case UploadOutcome.Failed(m) => (None, None, Some(m))
          db.transact(
            List(
              "INSERT INTO offers(id, project_id) VALUES (?, ?)" ->
                List(Some(upload.id.value), Some(id.value)),
              """INSERT INTO uploads(id, project_id, category_id, file_name, file_type, created_at,
                |  extraction, extraction_version, failure)
                |VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)""".stripMargin -> List(
                Some(upload.id.value),
                Some(id.value),
                Some(upload.category.value),
                Some(upload.fileName),
                Some(upload.fileType.extension),
                Some(upload.createdAt.toString),
                extraction,
                version,
                failure
              )
            )
          ).flatMap(_.fold(e => IO.pure(Left(e)), _ => load(id)))
    )

object SqliteProjectRepository:
  /** Version of the [[RawOffer]] JSON in `uploads.extraction`; other versions are not readable. */
  val ExtractionVersion: Int = 1

  val Unreadable: String = "gespeicherte Daten nicht lesbar"

  /** A row with a usable id, category, file type and time becomes an upload; a stored extraction
    * that does not decode (or has an unknown version) becomes a failed one, so it stays visible.
    */
  private[repo] def toUpload(r: UploadRow): Either[String, Upload] =
    val outcome = r.extraction match
      case Some(json) =>
        r.extractionVersion
          .filter(_ == ExtractionVersion)
          .flatMap(_ => decode[RawOffer](json).toOption)
          .fold(UploadOutcome.Failed(Unreadable))(UploadOutcome.Read(_))
      case None => UploadOutcome.Failed(r.failure.getOrElse(Unreadable))
    for
      id <- OfferId.from(r.id).left.map(_.toString)
      cat <- CategoryId.from(r.category).left.map(_.toString)
      ft <- OfferFileType.fromExtension(r.fileType).toRight(s"file type ${r.fileType}")
      created <- Try(Instant.parse(r.createdAt)).toEither.left.map(_.getClass.getSimpleName)
    yield Upload(id, cat, r.fileName, ft, created, outcome)

  /** Schema migrations; migration n (1-based) brings `PRAGMA user_version` from n-1 to n. */
  private val migrations: List[List[String]] = List(
    List(
      """CREATE TABLE IF NOT EXISTS uploads(
        |  id TEXT PRIMARY KEY, project_id TEXT NOT NULL, category_id TEXT NOT NULL,
        |  file_name TEXT NOT NULL, file_type TEXT NOT NULL, created_at TEXT NOT NULL,
        |  extraction TEXT, failure TEXT)""".stripMargin,
      """CREATE TABLE IF NOT EXISTS favorites(
        |  project_id TEXT NOT NULL, category_id TEXT NOT NULL, offer_id TEXT NOT NULL,
        |  PRIMARY KEY (project_id, category_id))""".stripMargin
    ),
    // Every offer (seed or upload) gets a row in `offers`, so favorites can reference it.
    List(
      "CREATE TABLE offers(id TEXT PRIMARY KEY, project_id TEXT NOT NULL)",
      "INSERT OR IGNORE INTO offers(id, project_id) SELECT id, project_id FROM uploads",
      """CREATE TABLE favorites_new(
        |  project_id TEXT NOT NULL, category_id TEXT NOT NULL,
        |  offer_id TEXT NOT NULL REFERENCES offers(id) ON DELETE CASCADE,
        |  PRIMARY KEY (project_id, category_id))""".stripMargin,
      "INSERT INTO favorites_new SELECT project_id, category_id, offer_id FROM favorites",
      "DROP TABLE favorites",
      "ALTER TABLE favorites_new RENAME TO favorites"
    ),
    // Extractions written before versioning are RawOffer JSON version 1.
    List(
      "ALTER TABLE uploads ADD COLUMN extraction_version INTEGER",
      "UPDATE uploads SET extraction_version = 1 WHERE extraction IS NOT NULL"
    )
  )

  private def migrate(db: Sqlite, seeds: List[Project]): IO[Either[RepoError, Unit]] =
    val seedOffers = seeds.flatMap(p => p.offers.map(o => (o.id.value, p.id.value)))
    // Foreign keys stay off while tables are rebuilt; see https://sqlite.org/lang_altertable.html
    val steps = for
      _ <- EitherT(db.execute("PRAGMA foreign_keys = OFF"))
      current <- EitherT(db.query("PRAGMA user_version", Nil)(_.getInt(1)))
      _ <- migrations.zipWithIndex
        .drop(current.headOption.getOrElse(0))
        .traverse_((statements, i) =>
          EitherT(db.transact((statements :+ s"PRAGMA user_version = ${i + 1}").map(_ -> Nil)))
        )
      _ <- EitherT(
        db.transact(
          seedOffers.map((o, p) =>
            "INSERT OR IGNORE INTO offers(id, project_id) VALUES (?, ?)" -> List(Some(o), Some(p))
          )
        )
      )
      _ <- EitherT(db.execute("PRAGMA foreign_keys = ON"))
    yield ()
    steps.value

  /** Opens (and if needed creates or migrates) the database at `path`. */
  def resource(
      path: java.nio.file.Path,
      seeds: List[Project]
  ): Resource[IO, SqliteProjectRepository] =
    for
      db <- Sqlite.resource(path)
      _ <- Resource.eval(
        migrate(db, seeds).flatMap(
          _.fold(
            e => IO.raiseError(IllegalStateException(s"Datenbank-Migration fehlgeschlagen: $e")),
            IO.pure
          )
        )
      )
      lock <- Resource.eval(Mutex[IO])
    yield SqliteProjectRepository(db, lock, seeds.map(p => p.id -> p).toMap)

/** A SQLite connection. Statements and result sets are closed via `Using`; SQL errors become
  * [[RepoError.Storage]].
  */
final class Sqlite private (conn: Connection):
  private def storage(e: SQLException): RepoError = RepoError.Storage(e.getErrorCode)

  private def run[A](io: IO[A]): IO[Either[RepoError, A]] =
    io.attemptNarrow[SQLException].map(_.left.map(storage))

  private def bind(st: java.sql.PreparedStatement, params: List[Option[String]]): Unit =
    params.zipWithIndex.foreach((v, i) =>
      v.fold(st.setNull(i + 1, java.sql.Types.VARCHAR))(st.setString(i + 1, _))
    )

  def query[A](sql: String, params: List[Option[String]])(
      read: ResultSet => A
  ): IO[Either[RepoError, List[A]]] =
    run(IO.blocking(Using.resource(conn.prepareStatement(sql)) { st =>
      bind(st, params)
      Using.resource(st.executeQuery())(rs =>
        Iterator.continually(rs.next()).takeWhile(identity).map(_ => read(rs)).toList
      )
    }))

  /** A statement outside a transaction (PRAGMAs). */
  def execute(sql: String): IO[Either[RepoError, Unit]] =
    run(IO.blocking(Using.resource(conn.createStatement())(st => st.execute(sql))).void)

  /** Runs all statements in one transaction: committed if all succeed, rolled back otherwise. */
  def transact(statements: List[(String, List[Option[String]])]): IO[Either[RepoError, Unit]] =
    run(
      IO.blocking(conn.setAutoCommit(false))
        .bracketCase(_ =>
          IO.blocking(
            statements.foreach((sql, params) =>
              Using.resource(conn.prepareStatement(sql)) { st =>
                bind(st, params)
                st.execute()
              }
            )
          )
        )((_, outcome) =>
          (outcome match
            case Outcome.Succeeded(_)                    => IO.blocking(conn.commit())
            case Outcome.Errored(_) | Outcome.Canceled() => IO.blocking(conn.rollback())
          ) >> IO.blocking(conn.setAutoCommit(true))
        )
    )

object Sqlite:
  /** Connection in WAL mode that waits up to 5 s for a lock instead of failing at once. */
  def resource(path: java.nio.file.Path): Resource[IO, Sqlite] =
    Resource
      .make(IO.blocking {
        Option(path.toAbsolutePath.getParent).foreach(java.nio.file.Files.createDirectories(_))
        val ds = org.sqlite.SQLiteDataSource()
        ds.setUrl(s"jdbc:sqlite:${path.toAbsolutePath}")
        ds.setJournalMode("WAL")
        ds.setBusyTimeout(5000)
        ds.getConnection()
      })(c => IO.blocking(c.close()))
      .map(Sqlite(_))

package builder.repo

import builder.domain.*
import builder.extraction.*
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.Mutex
import io.circe.parser.decode
import io.circe.syntax.*

import java.sql.Connection
import java.sql.ResultSet
import java.time.Instant
import scala.util.Try

enum RepoError:
  case ProjectNotFound(id: ProjectId)
  case CategoryNotFound(id: CategoryId)
  case OfferNotFound(id: OfferId)
  case Invalid(error: DomainError)

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

/** A project including all accepted uploaded offers, plus every upload with its status. */
final case class ProjectView(project: Project, uploads: List[UploadView])

trait ProjectRepository:
  def find(id: ProjectId): IO[Option[ProjectView]]

  /** Marks `offer` as the favorite of `category` and returns the updated project. */
  def setFavorite(
      id: ProjectId,
      category: CategoryId,
      offer: OfferId
  ): IO[Either[RepoError, ProjectView]]

  /** Stores an uploaded offer of `id`; it is validated (and counted if valid) on every read. */
  def addUpload(id: ProjectId, upload: Upload): IO[Either[RepoError, ProjectView]]

/** SQLite-backed repository. Projects (categories, work packages, seed offers) come from `seeds`;
  * uploads and favorites are stored in the database, so they survive restarts. Every read
  * re-validates stored extractions with the domain smart constructors.
  */
final class SqliteProjectRepository private (
    conn: Connection,
    lock: Mutex[IO],
    seeds: Map[ProjectId, Project]
) extends ProjectRepository:

  private def rows[A](rs: ResultSet)(read: ResultSet => Option[A]): List[A] =
    Iterator.continually(rs.next()).takeWhile(identity).flatMap(_ => read(rs)).toList

  private def readUpload(rs: ResultSet): Option[Upload] =
    val outcome = Option(rs.getString("extraction"))
      .flatMap(decode[RawOffer](_).toOption)
      .map(UploadOutcome.Read(_))
      .orElse(Option(rs.getString("failure")).map(UploadOutcome.Failed(_)))
    for
      id <- OfferId.from(rs.getString("id")).toOption
      cat <- CategoryId.from(rs.getString("category_id")).toOption
      ft <- OfferFileType.fromExtension(rs.getString("file_type"))
      created <- Try(Instant.parse(rs.getString("created_at"))).toOption
      o <- outcome
    yield Upload(id, cat, rs.getString("file_name"), ft, created, o)

  private def loadUploads(id: ProjectId): IO[List[Upload]] = IO.blocking {
    val st = conn.prepareStatement(
      "SELECT * FROM uploads WHERE project_id = ? ORDER BY created_at, id"
    )
    st.setString(1, id.value)
    val result = rows(st.executeQuery())(readUpload)
    st.close()
    result
  }

  private def loadFavorites(id: ProjectId): IO[List[(CategoryId, OfferId)]] = IO.blocking {
    val st =
      conn.prepareStatement("SELECT category_id, offer_id FROM favorites WHERE project_id = ?")
    st.setString(1, id.value)
    val result = rows(st.executeQuery())(rs =>
      (for
        c <- CategoryId.from(rs.getString("category_id"))
        o <- OfferId.from(rs.getString("offer_id"))
      yield c -> o).toOption
    )
    st.close()
    result
  }

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

  private def load(id: ProjectId): IO[Option[ProjectView]] =
    seeds.lift(id) match
      case None       => IO.pure(None)
      case Some(seed) =>
        for
          uploads <- loadUploads(id)
          favorites <- loadFavorites(id)
        yield
          val (project, views) = uploads.foldLeft((seed, List.empty[UploadView])) {
            case ((p, acc), u) =>
              val s = status(p, u)
              val next = s match
                case UploadStatus.Accepted(_, o) => p.withOffer(o).getOrElse(p)
                case UploadStatus.Rejected(_, _) | UploadStatus.ExtractionFailed(_) => p
              (next, acc :+ UploadView(u, s))
          }
          // Favorites were validated when set; one whose offer vanished is ignored.
          val withFavs =
            favorites.foldLeft(project)((p, cf) => p.withFavorite(cf._1, cf._2).getOrElse(p))
          Some(ProjectView(withFavs, views))

  def find(id: ProjectId): IO[Option[ProjectView]] = lock.lock.surround(load(id))

  def setFavorite(
      id: ProjectId,
      category: CategoryId,
      offer: OfferId
  ): IO[Either[RepoError, ProjectView]] =
    lock.lock.surround(
      load(id).flatMap {
        case None    => IO.pure(Left(RepoError.ProjectNotFound(id)))
        case Some(v) =>
          val p = v.project
          if !p.categories.exists(_.id == category) then
            IO.pure(Left(RepoError.CategoryNotFound(category)))
          else if !p.offers.exists(_.id == offer) then IO.pure(Left(RepoError.OfferNotFound(offer)))
          else
            p.withFavorite(category, offer) match
              case Left(e)  => IO.pure(Left(RepoError.Invalid(e)))
              case Right(_) =>
                IO.blocking {
                  val st = conn.prepareStatement(
                    "INSERT OR REPLACE INTO favorites(project_id, category_id, offer_id) VALUES (?, ?, ?)"
                  )
                  st.setString(1, id.value)
                  st.setString(2, category.value)
                  st.setString(3, offer.value)
                  st.executeUpdate()
                  st.close()
                } >> load(id).map(_.toRight(RepoError.ProjectNotFound(id)))
      }
    )

  def addUpload(id: ProjectId, upload: Upload): IO[Either[RepoError, ProjectView]] =
    lock.lock.surround(
      seeds.lift(id) match
        case None => IO.pure(Left(RepoError.ProjectNotFound(id)))
        case Some(p) if !p.categories.exists(_.id == upload.category) =>
          IO.pure(Left(RepoError.CategoryNotFound(upload.category)))
        case Some(_) =>
          val (extraction, failure) = upload.outcome match
            case UploadOutcome.Read(raw) => (Some(raw.asJson.noSpaces), None)
            case UploadOutcome.Failed(m) => (None, Some(m))
          IO.blocking {
            val st = conn.prepareStatement(
              """INSERT INTO uploads(id, project_id, category_id, file_name, file_type, created_at, extraction, failure)
                |VALUES (?, ?, ?, ?, ?, ?, ?, ?)""".stripMargin
            )
            st.setString(1, upload.id.value)
            st.setString(2, id.value)
            st.setString(3, upload.category.value)
            st.setString(4, upload.fileName)
            st.setString(5, upload.fileType.extension)
            st.setString(6, upload.createdAt.toString)
            extraction.fold(st.setNull(7, java.sql.Types.VARCHAR))(st.setString(7, _))
            failure.fold(st.setNull(8, java.sql.Types.VARCHAR))(st.setString(8, _))
            st.executeUpdate()
            st.close()
          } >> load(id).map(_.toRight(RepoError.ProjectNotFound(id)))
    )

object SqliteProjectRepository:
  private val schema = List(
    """CREATE TABLE IF NOT EXISTS uploads(
      |  id TEXT PRIMARY KEY, project_id TEXT NOT NULL, category_id TEXT NOT NULL,
      |  file_name TEXT NOT NULL, file_type TEXT NOT NULL, created_at TEXT NOT NULL,
      |  extraction TEXT, failure TEXT)""".stripMargin,
    """CREATE TABLE IF NOT EXISTS favorites(
      |  project_id TEXT NOT NULL, category_id TEXT NOT NULL, offer_id TEXT NOT NULL,
      |  PRIMARY KEY (project_id, category_id))""".stripMargin
  )

  /** Opens (and if needed creates) the database at `path`. */
  def resource(
      path: java.nio.file.Path,
      seeds: List[Project]
  ): Resource[IO, SqliteProjectRepository] =
    for
      conn <- Resource.make(IO.blocking {
        Option(path.toAbsolutePath.getParent).foreach(java.nio.file.Files.createDirectories(_))
        val ds = org.sqlite.SQLiteDataSource()
        ds.setUrl(s"jdbc:sqlite:${path.toAbsolutePath}")
        ds.getConnection()
      })(c => IO.blocking(c.close()))
      _ <- Resource.eval(IO.blocking {
        val st = conn.createStatement()
        schema.foreach(st.executeUpdate)
        st.close()
      })
      lock <- Resource.eval(Mutex[IO])
    yield SqliteProjectRepository(conn, lock, seeds.map(p => p.id -> p).toMap)

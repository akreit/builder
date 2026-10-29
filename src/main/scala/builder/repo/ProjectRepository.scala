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

  /** A consistent request the current state does not allow (e.g. deleting a used category). */
  case Rejected(reason: String)

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
final case class ProjectView(
    project: Project,
    uploads: List[UploadView],
    warnings: List[String],
    leistungen: List[Leistung],
    assignments: List[ItemAssignment],
    coverage: List[CoverageEntry]
)

/** A manual change to categories, Leistungen, item assignments or coverage. Manual changes are
  * stored separately from extracted data, so later extractions never overwrite them.
  */
enum Edit:
  case AddCategory(name: CategoryName)
  case RenameCategory(category: CategoryId, name: CategoryName)

  /** Only for categories without work packages, offers or uploads. */
  case DeleteCategory(category: CategoryId)
  case AddLeistung(category: CategoryId, name: LeistungName)
  case RenameLeistung(leistung: LeistungId, name: LeistungName)

  /** Moves items and coverage of `from` onto `into` (same category) and removes `from`. */
  case MergeLeistung(from: LeistungId, into: LeistungId)

  /** Removes the Leistung; its items become unassigned. */
  case DeleteLeistung(leistung: LeistungId)

  /** `None` leaves the item without Leistung. */
  case AssignItem(offer: OfferId, item: Int, leistung: Option[LeistungId])

  /** `None` removes the manual status, so the extracted one applies again. */
  case SetCoverage(offer: OfferId, leistung: LeistungId, coverage: Option[Coverage])

trait ProjectRepository:
  def find(id: ProjectId): IO[Either[RepoError, ProjectView]]

  /** Marks `offer` as the favorite of `category` and returns the updated project. */
  def setFavorite(
      id: ProjectId,
      category: CategoryId,
      offer: OfferId
  ): IO[Either[RepoError, ProjectView]]

  /** Stores an uploaded offer of `id`; it is validated (and counted if valid) on every read.
    * `Right` means the row is committed.
    */
  def addUpload(id: ProjectId, upload: Upload): IO[Either[RepoError, Unit]]

  /** Applies a manual change and returns the updated project. */
  def edit(id: ProjectId, e: Edit): IO[Either[RepoError, ProjectView]]

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

private final case class CategoryRow(id: String, name: String, position: Int, deleted: Boolean)
private final case class LeistungRow(id: String, category: String, name: String)
private final case class AssignmentRow(
    offer: String,
    item: Int,
    leistung: Option[String],
    manual: Boolean
)
private final case class CoverageRow(
    offer: String,
    leistung: String,
    status: String,
    page: Option[Int],
    confidence: Option[String],
    manual: Boolean
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

  private def loadCategories(id: ProjectId): IO[Either[RepoError, List[CategoryRow]]] =
    db.query(
      "SELECT id, name, position, deleted FROM categories WHERE project_id = ? ORDER BY position, id",
      List(Some(id.value))
    )(rs =>
      CategoryRow(
        rs.getString("id"),
        rs.getString("name"),
        rs.getInt("position"),
        rs.getInt("deleted") == 1
      )
    )

  private def loadLeistungen(id: ProjectId): IO[Either[RepoError, List[LeistungRow]]] =
    db.query(
      """SELECT id, category_id, name FROM leistungen
        |WHERE project_id = ? AND deleted = 0 ORDER BY position, id""".stripMargin,
      List(Some(id.value))
    )(rs => LeistungRow(rs.getString("id"), rs.getString("category_id"), rs.getString("name")))

  private def loadAssignments(id: ProjectId): IO[Either[RepoError, List[AssignmentRow]]] =
    db.query(
      "SELECT offer_id, item, leistung_id, manual FROM item_leistung WHERE project_id = ?",
      List(Some(id.value))
    )(rs =>
      AssignmentRow(
        rs.getString("offer_id"),
        rs.getInt("item"),
        Option(rs.getString("leistung_id")),
        rs.getInt("manual") == 1
      )
    )

  private def loadCoverage(id: ProjectId): IO[Either[RepoError, List[CoverageRow]]] =
    db.query(
      "SELECT offer_id, leistung_id, status, page, confidence, manual FROM abdeckung WHERE project_id = ?",
      List(Some(id.value))
    )(rs =>
      CoverageRow(
        rs.getString("offer_id"),
        rs.getString("leistung_id"),
        rs.getString("status"),
        Option(rs.getObject("page")).flatMap(_.toString.toIntOption),
        Option(rs.getString("confidence")),
        rs.getInt("manual") == 1
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
    ProjectView(withFavs, views, decoded.collect { case Left(w) => w } ++ ignored, Nil, Nil, Nil)

  private def load(id: ProjectId): IO[Either[RepoError, ProjectView]] =
    seeds.lift(id) match
      case None       => IO.pure(Left(RepoError.ProjectNotFound(id)))
      case Some(seed) =>
        (for
          rows <- EitherT(loadRows(id))
          favorites <- EitherT(loadFavorites(id))
          cats <- EitherT(loadCategories(id))
          ls <- EitherT(loadLeistungen(id))
          as <- EitherT(loadAssignments(id))
          cs <- EitherT(loadCoverage(id))
        yield
          val (base, catWarnings) = withCategories(seed, cats)
          val view = build(base, rows, favorites)
          val categories = base.categories.map(_.id).toSet
          val leistungen = ls.flatMap(r =>
            (LeistungId.from(r.id), CategoryId.from(r.category), LeistungName.from(r.name))
              .mapN(Leistung.apply)
              .toOption
              .filter(l => categories.contains(l.category))
          )
          view.copy(
            warnings = catWarnings ++ view.warnings,
            leistungen = leistungen,
            assignments = as.flatMap(toAssignment),
            coverage = cs.flatMap(toCoverage)
          )
        ).value

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

  def addUpload(id: ProjectId, upload: Upload): IO[Either[RepoError, Unit]] =
    lock.lock.surround(
      load(id).flatMap {
        case Left(e)                                                           => IO.pure(Left(e))
        case Right(v) if !v.project.categories.exists(_.id == upload.category) =>
          IO.pure(Left(RepoError.CategoryNotFound(upload.category)))
        case Right(v) =>
          val (extraction, version, failure) = upload.outcome match
            case UploadOutcome.Read(raw) =>
              (Some(raw.asJson.noSpaces), Some(ExtractionVersion.toString), None)
            case UploadOutcome.Failed(m) => (None, None, Some(m))
          val proposed = upload.outcome match
            case UploadOutcome.Read(raw) => raw.neueLeistungen.getOrElse(Nil).size
            case UploadOutcome.Failed(_) => 0
          freshIds("l", proposed).flatMap(ids =>
            db.transact(
              uploadStatements(id, upload, extraction, version, failure) ++
                (upload.outcome match
                  case UploadOutcome.Read(raw) =>
                    planStatements(
                      id,
                      upload.id,
                      LeistungPlan.plan(
                        raw,
                        upload.category,
                        v.leistungen.filter(_.category == upload.category),
                        LazyList.from(ids)
                      )
                    )
                  case UploadOutcome.Failed(_) => Nil)
            )
          )
      }
    )

  private def uploadStatements(
      id: ProjectId,
      upload: Upload,
      extraction: Option[String],
      version: Option[String],
      failure: Option[String]
  ): List[(String, List[Option[String]])] =
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

  private def freshIds(prefix: String, n: Int): IO[List[LeistungId]] =
    List
      .fill(n)(IO(java.util.UUID.randomUUID().toString.replace("-", "").take(10)))
      .sequence
      .map(_.flatMap(h => LeistungId.from(s"$prefix-$h").toOption))

  private def planStatements(
      id: ProjectId,
      offer: OfferId,
      plan: LeistungPlan
  ): List[(String, List[Option[String]])] =
    val p = Some(id.value)
    plan.added.map(l =>
      """INSERT INTO leistungen(project_id, id, category_id, name, position, deleted)
        |VALUES (?, ?, ?, ?, (SELECT COALESCE(MAX(position), 0) + 1 FROM leistungen WHERE project_id = ?), 0)""".stripMargin ->
        List(p, Some(l.id.value), Some(l.category.value), Some(l.name.value), p)
    ) ++ plan.items.zipWithIndex.map((l, i) =>
      "INSERT INTO item_leistung(project_id, offer_id, item, leistung_id, manual) VALUES (?, ?, ?, ?, 0)" ->
        List(p, Some(offer.value), Some(i.toString), l.map(_.value))
    ) ++ plan.coverage.map(c =>
      """INSERT INTO abdeckung(project_id, offer_id, leistung_id, status, page, confidence, manual)
        |VALUES (?, ?, ?, ?, ?, ?, 0)""".stripMargin ->
        List(
          p,
          Some(offer.value),
          Some(c.leistung.value),
          Some(Coverage.code(c.coverage)),
          c.page.map(_.value.toString),
          Some(c.confidence.toString.toLowerCase)
        )
    )

  def edit(id: ProjectId, e: Edit): IO[Either[RepoError, ProjectView]] =
    lock.lock.surround(
      (for
        view <- EitherT(load(id))
        fresh <- EitherT.liftF(
          IO(java.util.UUID.randomUUID().toString.replace("-", "").take(10))
        )
        statements <- EitherT.fromEither[IO](editStatements(id, view, e, fresh))
        _ <- EitherT(db.transact(statements))
        updated <- EitherT(load(id))
      yield updated).value
    )

  private def editStatements(
      id: ProjectId,
      view: ProjectView,
      e: Edit,
      fresh: String
  ): Either[RepoError, List[(String, List[Option[String]])]] =
    val p = Some(id.value)
    val project = view.project
    def category(c: CategoryId): Either[RepoError, Category] =
      project.categories.find(_.id == c).toRight(RepoError.CategoryNotFound(c))
    def leistung(l: LeistungId): Either[RepoError, Leistung] =
      view.leistungen
        .find(_.id == l)
        .toRight(RepoError.Rejected(s"Leistung ${l.value} gibt es nicht"))
    def upload(o: OfferId): Either[RepoError, (Upload, RawOffer)] =
      view.uploads
        .find(_.upload.id == o)
        .flatMap(u =>
          u.status match
            case UploadStatus.Accepted(raw, _)    => Some(u.upload -> raw)
            case UploadStatus.Rejected(raw, _)    => Some(u.upload -> raw)
            case UploadStatus.ExtractionFailed(_) => None
        )
        .toRight(RepoError.OfferNotFound(o))
    def sameCategory(u: Upload, l: Leistung): Either[RepoError, Unit] =
      Either.cond(
        u.category == l.category,
        (),
        RepoError.Rejected("Leistung gehört zu einer anderen Kategorie")
      )
    val upsertCategory =
      """INSERT INTO categories(project_id, id, name, position, deleted)
        |VALUES (?, ?, ?, (SELECT COALESCE(MAX(position), 0) + 1 FROM categories WHERE project_id = ?), ?)
        |ON CONFLICT(project_id, id) DO UPDATE SET name = excluded.name, deleted = excluded.deleted""".stripMargin
    e match
      case Edit.AddCategory(name) =>
        CategoryId
          .from(s"k-$fresh")
          .left
          .map(RepoError.Invalid(_))
          .map(c => List(upsertCategory -> List(p, Some(c.value), Some(name.value), p, Some("0"))))
      case Edit.RenameCategory(c, name) =>
        category(c).map(_ =>
          List(upsertCategory -> List(p, Some(c.value), Some(name.value), p, Some("0")))
        )
      case Edit.DeleteCategory(c) =>
        for
          cat <- category(c)
          _ <- Either.cond(
            !view.uploads.exists(_.upload.category == c),
            (),
            RepoError.Rejected("Kategorie enthält hochgeladene Angebote")
          )
          _ <- Project
            .from(
              project.id,
              project.name,
              project.currency,
              project.categories.filterNot(_.id == c),
              project.workPackages,
              project.offers,
              project.favorites
            )
            .left
            .map(_ =>
              RepoError
                .Rejected("Kategorie wird von Arbeitspaketen, Angeboten oder Favoriten verwendet")
            )
        yield List(
          upsertCategory -> List(p, Some(c.value), Some(cat.name.value), p, Some("1")),
          "UPDATE leistungen SET deleted = 1 WHERE project_id = ? AND category_id = ?" -> List(
            p,
            Some(c.value)
          )
        )
      case Edit.AddLeistung(c, name) =>
        for
          _ <- category(c)
          l <- LeistungId.from(s"l-$fresh").left.map(RepoError.Invalid(_))
        yield List(
          """INSERT INTO leistungen(project_id, id, category_id, name, position, deleted)
            |VALUES (?, ?, ?, ?, (SELECT COALESCE(MAX(position), 0) + 1 FROM leistungen WHERE project_id = ?), 0)""".stripMargin ->
            List(p, Some(l.value), Some(c.value), Some(name.value), p)
        )
      case Edit.RenameLeistung(l, name) =>
        leistung(l).map(_ =>
          List(
            "UPDATE leistungen SET name = ? WHERE project_id = ? AND id = ?" -> List(
              Some(name.value),
              p,
              Some(l.value)
            )
          )
        )
      case Edit.MergeLeistung(from, into) =>
        for
          f <- leistung(from)
          t <- leistung(into)
          _ <- Either.cond(
            f.id != t.id,
            (),
            RepoError.Rejected("Eine Leistung kann nicht mit sich selbst zusammengeführt werden")
          )
          _ <- Either.cond(
            f.category == t.category,
            (),
            RepoError.Rejected("Leistungen gehören zu verschiedenen Kategorien")
          )
        yield
          val ids = List(p, Some(from.value))
          List(
            "UPDATE item_leistung SET leistung_id = ? WHERE project_id = ? AND leistung_id = ?" -> (Some(
              into.value
            ) :: ids),
            // Read statuses of the target become "enthalten" where the merged one was.
            """UPDATE abdeckung SET status = 'enthalten'
              |WHERE project_id = ? AND leistung_id = ? AND manual = 0 AND offer_id IN
              |  (SELECT offer_id FROM abdeckung WHERE project_id = ? AND leistung_id = ? AND manual = 0
              |   AND status = 'enthalten')""".stripMargin -> List(
              p,
              Some(into.value),
              p,
              Some(from.value)
            ),
            "UPDATE OR IGNORE abdeckung SET leistung_id = ? WHERE project_id = ? AND leistung_id = ?" ->
              (Some(into.value) :: ids),
            "DELETE FROM abdeckung WHERE project_id = ? AND leistung_id = ?" -> ids,
            "UPDATE leistungen SET deleted = 1 WHERE project_id = ? AND id = ?" -> ids
          )
      case Edit.DeleteLeistung(l) =>
        leistung(l).map { _ =>
          val ids = List(p, Some(l.value))
          List(
            "UPDATE item_leistung SET leistung_id = NULL WHERE project_id = ? AND leistung_id = ?" -> ids,
            "DELETE FROM abdeckung WHERE project_id = ? AND leistung_id = ?" -> ids,
            "UPDATE leistungen SET deleted = 1 WHERE project_id = ? AND id = ?" -> ids
          )
        }
      case Edit.AssignItem(o, item, target) =>
        for
          (u, raw) <- upload(o)
          _ <- Either.cond(
            raw.lineItems.indices.contains(item),
            (),
            RepoError.Rejected(s"Position ${item + 1} gibt es nicht")
          )
          _ <- target.traverse(l => leistung(l).flatMap(sameCategory(u, _)))
        yield List(
          """INSERT OR REPLACE INTO item_leistung(project_id, offer_id, item, leistung_id, manual)
            |VALUES (?, ?, ?, ?, 1)""".stripMargin -> List(
            p,
            Some(o.value),
            Some(item.toString),
            target.map(_.value)
          )
        )
      case Edit.SetCoverage(o, l, coverage) =>
        for
          (u, _) <- upload(o)
          _ <- leistung(l).flatMap(sameCategory(u, _))
        yield coverage match
          case Some(c) =>
            List(
              """INSERT OR REPLACE INTO abdeckung(project_id, offer_id, leistung_id, status, page, confidence, manual)
                |VALUES (?, ?, ?, ?, NULL, NULL, 1)""".stripMargin ->
                List(p, Some(o.value), Some(l.value), Some(Coverage.code(c)))
            )
          case None =>
            List(
              "DELETE FROM abdeckung WHERE project_id = ? AND offer_id = ? AND leistung_id = ? AND manual = 1" ->
                List(p, Some(o.value), Some(l.value))
            )

object SqliteProjectRepository:
  /** Version of the [[RawOffer]] JSON written to `uploads.extraction`. Version 2 added the optional
    * Leistung fields, so version 1 still decodes; other versions are not readable.
    */
  val ExtractionVersion: Int = 2
  private val ReadableVersions: Set[Int] = Set(1, 2)

  /** Seed categories with stored renames and deletions applied, plus added categories. Stored
    * changes that would make the project invalid are ignored with a warning.
    */
  private[repo] def withCategories(
      seed: Project,
      rows: List[CategoryRow]
  ): (Project, List[String]) =
    val byId = rows.map(r => r.id -> r).toMap
    val kept = seed.categories.flatMap(c =>
      byId.get(c.id.value) match
        case Some(r) if r.deleted => None
        case Some(r) => Some(CategoryName.from(r.name).fold(_ => c, n => c.copy(name = n)))
        case None    => Some(c)
    )
    val seedIds = seed.categories.map(_.id.value).toSet
    val added = rows
      .filter(r => !r.deleted && !seedIds.contains(r.id))
      .flatMap(r =>
        (CategoryId.from(r.id), CategoryName.from(r.name)).mapN(Category.apply).toOption
      )
    Project.from(
      seed.id,
      seed.name,
      seed.currency,
      kept ++ added,
      seed.workPackages,
      seed.offers,
      seed.favorites
    ) match
      case Right(p) => (p, Nil)
      case Left(_)  =>
        (seed, List("Gespeicherte Änderungen an Kategorien sind ungültig und werden ignoriert"))

  private def toAssignment(r: AssignmentRow): Option[ItemAssignment] =
    for
      o <- OfferId.from(r.offer).toOption
      l <- r.leistung.traverse(LeistungId.from).toOption
    yield ItemAssignment(o, r.item, l, r.manual)

  private def toCoverage(r: CoverageRow): Option[CoverageEntry] =
    for
      o <- OfferId.from(r.offer).toOption
      l <- LeistungId.from(r.leistung).toOption
      c <- Coverage.fromCode(r.status)
    yield
      val source =
        if r.manual then CoverageSource.Manual
        else
          CoverageSource.Read(
            r.page.flatMap(PageNumber.from(_).toOption),
            r.confidence
              .flatMap(s => Confidence.values.find(_.toString.toLowerCase == s))
              .getOrElse(Confidence.Low)
          )
      CoverageEntry(o, l, c, source)

  val Unreadable: String = "gespeicherte Daten nicht lesbar"

  /** A row with a usable id, category, file type and time becomes an upload; a stored extraction
    * that does not decode (or has an unknown version) becomes a failed one, so it stays visible.
    */
  private[repo] def toUpload(r: UploadRow): Either[String, Upload] =
    val outcome = r.extraction match
      case Some(json) =>
        r.extractionVersion
          .filter(ReadableVersions.contains)
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
    ),
    // Offer ids are unique per project only: key (project_id, id), favorites reference both.
    List(
      """CREATE TABLE offers_new(project_id TEXT NOT NULL, id TEXT NOT NULL,
        |  PRIMARY KEY (project_id, id))""".stripMargin,
      "INSERT INTO offers_new(project_id, id) SELECT project_id, id FROM offers",
      """CREATE TABLE favorites_new(
        |  project_id TEXT NOT NULL, category_id TEXT NOT NULL, offer_id TEXT NOT NULL,
        |  PRIMARY KEY (project_id, category_id),
        |  FOREIGN KEY (project_id, offer_id) REFERENCES offers(project_id, id) ON DELETE CASCADE)""".stripMargin,
      "INSERT INTO favorites_new SELECT project_id, category_id, offer_id FROM favorites",
      "DROP TABLE favorites",
      "DROP TABLE offers",
      "ALTER TABLE offers_new RENAME TO offers",
      "ALTER TABLE favorites_new RENAME TO favorites"
    ),
    // Leistungen: category changes, Leistungen per category, item assignments and coverage.
    // Rows with manual = 1 are user edits and take precedence over extracted ones (manual = 0).
    List(
      """CREATE TABLE categories(project_id TEXT NOT NULL, id TEXT NOT NULL, name TEXT NOT NULL,
        |  position INTEGER NOT NULL, deleted INTEGER NOT NULL DEFAULT 0,
        |  PRIMARY KEY (project_id, id))""".stripMargin,
      """CREATE TABLE leistungen(project_id TEXT NOT NULL, id TEXT NOT NULL,
        |  category_id TEXT NOT NULL, name TEXT NOT NULL, position INTEGER NOT NULL,
        |  deleted INTEGER NOT NULL DEFAULT 0, PRIMARY KEY (project_id, id))""".stripMargin,
      """CREATE TABLE item_leistung(project_id TEXT NOT NULL, offer_id TEXT NOT NULL,
        |  item INTEGER NOT NULL, leistung_id TEXT, manual INTEGER NOT NULL,
        |  PRIMARY KEY (project_id, offer_id, item, manual),
        |  FOREIGN KEY (project_id, offer_id) REFERENCES offers(project_id, id) ON DELETE CASCADE,
        |  FOREIGN KEY (project_id, leistung_id) REFERENCES leistungen(project_id, id))""".stripMargin,
      """CREATE TABLE abdeckung(project_id TEXT NOT NULL, offer_id TEXT NOT NULL,
        |  leistung_id TEXT NOT NULL, status TEXT NOT NULL, page INTEGER, confidence TEXT,
        |  manual INTEGER NOT NULL, PRIMARY KEY (project_id, offer_id, leistung_id, manual),
        |  FOREIGN KEY (project_id, offer_id) REFERENCES offers(project_id, id) ON DELETE CASCADE,
        |  FOREIGN KEY (project_id, leistung_id) REFERENCES leistungen(project_id, id))""".stripMargin
    )
  )

  private def migrate(
      db: Sqlite,
      seeds: List[Project],
      leistungen: List[(ProjectId, Leistung)]
  ): IO[Either[RepoError, Unit]] =
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
          ) ++ leistungen.zipWithIndex.map { case ((p, l), i) =>
            // Seed Leistungen are inserted once; renames and deletions of them are kept.
            """INSERT OR IGNORE INTO leistungen(project_id, id, category_id, name, position, deleted)
              |VALUES (?, ?, ?, ?, ?, 0)""".stripMargin ->
              List(
                Some(p.value),
                Some(l.id.value),
                Some(l.category.value),
                Some(l.name.value),
                Some((i - 1000).toString)
              )
          }
        )
      )
      _ <- EitherT(db.execute("PRAGMA foreign_keys = ON"))
    yield ()
    steps.value

  /** Opens (and if needed creates or migrates) the database at `path`. */
  def resource(
      path: java.nio.file.Path,
      seeds: List[Project],
      leistungen: List[(ProjectId, Leistung)] = Nil
  ): Resource[IO, SqliteProjectRepository] =
    for
      db <- Sqlite.resource(path)
      _ <- Resource.eval(
        migrate(db, seeds, leistungen).flatMap(
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

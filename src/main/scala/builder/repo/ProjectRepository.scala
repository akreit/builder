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
import skunk.Command
import skunk.Void
import skunk.implicits.*

import java.time.Instant
import java.time.ZoneOffset

/** Failures from the project repository (not found, rejected edit, storage). */
enum RepoError:
  case ProjectNotFound(id: ProjectId)
  case CategoryNotFound(id: CategoryId)
  case OfferNotFound(id: OfferId)
  case LeistungNotFound(id: LeistungId)

  /** `item` (0-based) is not a line item of the offer. */
  case ItemNotFound(offer: OfferId, item: Int)
  case Invalid(error: DomainError)

  /** A consistent request the current state does not allow (e.g. deleting a used category). */
  case Rejected(reason: String)

  /** The database failed; `code` is the Postgres SQLSTATE. */
  case Storage(code: SqlState)

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

/** How an upload appears in the UI and whether it counts toward totals. */
enum UploadStatus:
  case ExtractionFailed(message: String)

  /** Read, but missing, unreadable or inconsistent values: shown, never counted. */
  case Rejected(raw: RawOffer, issues: NonEmptyList[Issue])

  /** Validated and part of the project (and its running total). */
  case Accepted(raw: RawOffer, offer: Offer)

/** An upload together with its derived [[UploadStatus]]. */
final case class UploadView(upload: Upload, status: UploadStatus)

/** Outcome of [[ProjectRepository.addUpload]]: which offer id the row landed on, and which same
  * firm+recipient uploads in the category were removed in the same transaction.
  */
final case class UploadStored(keep: OfferId, obsolete: List[OfferId])

/** The file side of an upload, already made durable before the DB transaction runs: `rollback`
  * undoes it when the transaction fails or is canceled; `finish` drops what is no longer needed
  * after a commit (backups, obsolete files). A failing `finish` only leaves orphan files — the DB
  * row and the live file already agree.
  */
final case class FileStep(rollback: IO[Unit], finish: IO[Unit])

/** A project including all accepted uploaded offers, plus every upload with its status, and
  * warnings about stored data that could not be used (unreadable rows, ignored favorites).
  */
final case class ProjectView(
    project: Project,
    uploads: List[UploadView],
    warnings: List[String],
    leistungen: List[Leistung],
    assignments: List[ItemAssignment],
    coverage: List[CoverageEntry],
    ratings: Map[ContractorKey, Rating],
    weights: Weights
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

  /** `None` removes the rating: the contractor is "nicht bewertet" again. */
  case RateContractor(contractor: ContractorKey, rating: Option[Rating])
  case SetWeights(weights: Weights)

/** Persistence API for projects: load, favorites, uploads, deletes and manual edits. */
trait ProjectRepository:

  /** Loads the full project view (trait contract). */
  def find(id: ProjectId): IO[Either[RepoError, ProjectView]]

  /** Marks `offer` as the favorite of `category` and returns the updated project. */
  def setFavorite(
      id: ProjectId,
      category: CategoryId,
      offer: OfferId
  ): IO[Either[RepoError, ProjectView]]

  /** Deletes the uploaded offer `offer` of `category` in `id` (cascades uploads, favorites,
    * assignments and coverage). After a successful transaction, `cleanup` runs still under the
    * repository lock (same pattern as upload `finish`) so a concurrent upload cannot interleave
    * with file removal. A hard kill mid-cleanup can still leave an orphan file — the lock does not
    * survive process death. Returns the updated project view.
    */
  def deleteOffer(
      id: ProjectId,
      category: CategoryId,
      offer: OfferId,
      cleanup: IO[Unit]
  ): IO[Either[RepoError, ProjectView]]

  /** Stores an uploaded offer of `id`; it is validated (and counted if valid) on every read. When
    * the project already has an upload of the same firm and recipient in the same category, that
    * row is replaced in place (same offer id) rather than creating a second offer. `Right` carries
    * the offer id that was kept and the obsolete same-identity ids whose files the caller should
    * delete — both decided under the repository lock from a fresh load, so the caller must not
    * guess them beforehand. Under the lock, `prepare` first puts the file durably in place
    * (returning how to undo it); only then is the transaction run. If it fails or is canceled the
    * file step is rolled back, so DB and file never disagree, and a concurrent replace cannot
    * interleave.
    */
  def addUpload(
      id: ProjectId,
      upload: Upload,
      prepare: UploadStored => IO[FileStep]
  ): IO[Either[RepoError, UploadStored]]

  /** Applies a manual change and returns the updated project. */
  def edit(id: ProjectId, e: Edit): IO[Either[RepoError, ProjectView]]

/** One `uploads` row as stored; decoded into an [[Upload]] by [[PostgresProjectRepository]]. */
private[repo] final case class UploadRow(
    id: String,
    category: String,
    fileName: String,
    fileType: String,
    createdAt: Instant,
    extraction: Option[String],
    extractionVersion: Option[Int],
    failure: Option[String]
)

/** One `categories` row as stored. */
private[repo] final case class CategoryRow(
    id: String,
    name: String,
    position: Int,
    deleted: Boolean
)

/** One `leistungen` row as stored. */
private[repo] final case class LeistungRow(id: String, category: String, name: String)

/** One `item_leistung` row as stored. */
private[repo] final case class AssignmentRow(
    offer: String,
    item: Int,
    leistung: Option[String],
    manual: Boolean
)

/** One `abdeckung` row as stored. */
private[repo] final case class CoverageRow(
    offer: String,
    leistung: String,
    status: String,
    page: Option[Int],
    confidence: Option[String],
    manual: Boolean
)

/** Postgres-backed repository. Project definitions come from the supplied `projects`; uploads and
  * favorites are stored in the database, so they survive restarts. Every read re-validates stored
  * extractions with the domain smart constructors.
  */
final class PostgresProjectRepository private (
    db: Postgres,
    lock: Mutex[IO],
    projects: Map[ProjectId, Project]
) extends ProjectRepository:
  import PostgresProjectRepository.*

  /** All upload rows for the project. */
  private def loadRows(id: ProjectId): IO[Either[RepoError, List[UploadRow]]] =
    db.query(Sql.uploads, id.value)

  /** Category rows (including soft-deleted) for the project. */
  private def loadCategories(id: ProjectId): IO[Either[RepoError, List[CategoryRow]]] =
    db.query(Sql.categories, id.value)

  /** Leistung rows for the project. */
  private def loadLeistungen(id: ProjectId): IO[Either[RepoError, List[LeistungRow]]] =
    db.query(Sql.leistungen, id.value)

  /** Item→Leistung assignment rows for the project. */
  private def loadAssignments(id: ProjectId): IO[Either[RepoError, List[AssignmentRow]]] =
    db.query(Sql.assignments, id.value)

  /** Coverage rows for the project. */
  private def loadCoverage(id: ProjectId): IO[Either[RepoError, List[CoverageRow]]] =
    db.query(Sql.coverage, id.value)

  /** Favorite (category, offer) pairs for the project. */
  private def loadFavorites(id: ProjectId): IO[Either[RepoError, List[(String, String)]]] =
    db.query(Sql.favorites, id.value)

  /** Derives [[UploadStatus]] by validating a stored extraction against the project. */
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

  /** Decodes upload rows into a [[ProjectView]] shell (Leistungen/ratings filled by [[load]]). */
  private def build(
      seed: Project,
      rows: List[UploadRow],
      favorites: List[(String, String)]
  ): ProjectView =
    val decoded = rows.map(r =>
      toUpload(r).left.map(_ => s"Gespeicherter Upload ${r.id} ist nicht lesbar und wird ignoriert")
    )
    val (project, views) = foldUploads(seed, decoded.collect { case Right(u) => u })
    val (withFavs, ignored) = applyFavorites(project, favorites)
    ProjectView(
      withFavs,
      views,
      decoded.collect { case Left(w) => w } ++ ignored,
      Nil,
      Nil,
      Nil,
      Map.empty,
      Weights.default
    )

  /** Folds decoded uploads into the seed project and upload views. */
  private def foldUploads(
      seed: Project,
      uploads: List[Upload]
  ): (Project, List[UploadView]) =
    uploads.foldLeft((seed, List.empty[UploadView])) { case ((p, acc), u) =>
      val s = status(p, u)
      val next = s match
        case UploadStatus.Accepted(_, o) => p.withOffer(o).getOrElse(p)
        case UploadStatus.Rejected(_, _) | UploadStatus.ExtractionFailed(_) => p
      (next, acc :+ UploadView(u, s))
    }

  /** Applies stored favorites; invalid ones become warnings and are skipped. */
  private def applyFavorites(
      project: Project,
      favorites: List[(String, String)]
  ): (Project, List[String]) =
    favorites.foldLeft((project, List.empty[String])) { case ((p, warnings), (c, o)) =>
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

  /** Loads and enriches the project view without taking the mutex (caller holds it). */
  private def load(id: ProjectId): IO[Either[RepoError, ProjectView]] =
    projects.lift(id) match
      case None       => IO.pure(Left(RepoError.ProjectNotFound(id)))
      case Some(seed) =>
        (for
          rows <- EitherT(loadRows(id))
          favorites <- EitherT(loadFavorites(id))
          cats <- EitherT(loadCategories(id))
          ls <- EitherT(loadLeistungen(id))
          as <- EitherT(loadAssignments(id))
          cs <- EitherT(loadCoverage(id))
          rs <- EitherT(db.query(Sql.ratings, id.value))
          ws <- EitherT(db.query(Sql.weights, id.value))
        yield enrichView(seed, rows, favorites, cats, ls, as, cs, rs, ws)).value

  /** Combines seed project, DB rows and ranking tables into a full [[ProjectView]]. */
  private def enrichView(
      seed: Project,
      rows: List[UploadRow],
      favorites: List[(String, String)],
      cats: List[CategoryRow],
      ls: List[LeistungRow],
      as: List[AssignmentRow],
      cs: List[CoverageRow],
      rs: List[(String, Int, String)],
      ws: List[(String, Int)]
  ): ProjectView =
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
      coverage = cs.flatMap(toCoverage),
      ratings = rs
        .flatMap((k, n, note) =>
          (ContractorKey.of(k), Stars.from(n).toOption)
            .mapN((key, stars) => key -> Rating(stars, note))
        )
        .toMap,
      // Stored weights complete the defaults, so criteria added later get their default.
      weights = Weights
        .from(
          Criterion.values.toList
            .map(c => c -> Weights.default.of(c))
            .toMap ++ ws.flatMap((c, w) => Criterion.fromCode(c).map(_ -> w))
        )
        .getOrElse(Weights.default)
    )

  /** Loads the project view under the repository mutex. */
  def find(id: ProjectId): IO[Either[RepoError, ProjectView]] = lock.lock.surround(load(id))

  /** Sets the category favorite and reloads the view under the lock. */
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
                  List(Stmt(Sql.upsertFavorite, (id.value, category.value, offer.value)))
                ).flatMap(_.fold(e => IO.pure(Left(e)), _ => load(id)))
      }
    )

  /** Deletes the offer row then runs `cleanup` still under the lock; reloads the view. */
  def deleteOffer(
      id: ProjectId,
      category: CategoryId,
      offer: OfferId,
      cleanup: IO[Unit]
  ): IO[Either[RepoError, ProjectView]] =
    lock.lock.surround(
      load(id).flatMap {
        case Left(e)  => IO.pure(Left(e))
        case Right(v) =>
          if !v.project.categories.exists(_.id == category) then
            IO.pure(Left(RepoError.CategoryNotFound(category)))
          else
            v.uploads.find(_.upload.id == offer) match
              case None => IO.pure(Left(RepoError.OfferNotFound(offer)))
              case Some(u) if u.upload.category != category =>
                IO.pure(Left(RepoError.OfferNotFound(offer)))
              case Some(_) =>
                db.transact(List(Stmt(Sql.deleteOffer, (id.value, offer.value)))).flatMap {
                  case Left(e)  => IO.pure(Left(e))
                  case Right(_) =>
                    // File removal under the lock after commit (upload finish pattern). Errors are
                    // swallowed like finish.attempt — DB row is already gone; a hard kill here can
                    // still orphan the file.
                    cleanup.attempt >> load(id)
                }
      }
    )

  /** Inserts/replaces an upload under the lock, running `prepare` for the file step. */
  def addUpload(
      id: ProjectId,
      upload: Upload,
      prepare: UploadStored => IO[FileStep]
  ): IO[Either[RepoError, UploadStored]] =
    lock.lock.surround(
      load(id).flatMap {
        case Left(e)                                                           => IO.pure(Left(e))
        case Right(v) if !v.project.categories.exists(_.id == upload.category) =>
          IO.pure(Left(RepoError.CategoryNotFound(upload.category)))
        case Right(v) =>
          // Keep/obsolete only under the lock from a fresh view — never from a stale snapshot.
          val keep = UploadReplace.kept(v, upload)
          val effective = upload.copy(id = keep)
          val obsolete = UploadReplace.obsolete(v, upload).map(_.id)
          val (extraction, version, failure) = effective.outcome match
            case UploadOutcome.Read(raw) =>
              (Some(raw.asJson.noSpaces), Some(ExtractionVersion), None)
            case UploadOutcome.Failed(m) => (None, None, Some(m))
          val proposed = effective.outcome match
            case UploadOutcome.Read(raw) => LeistungPlan.proposals(raw).size
            case UploadOutcome.Failed(_) => 0
          val replacing = v.uploads.exists(_.upload.id == keep)
          val stored = UploadStored(keep, obsolete)
          freshIds("l", proposed).flatMap(ids =>
            IO.uncancelable(poll =>
              prepare(stored).flatMap(step =>
                poll(
                  db.transact(
                    favoriteRetarget(id, v, upload.category, keep, obsolete) ++
                      obsolete.map(o => Stmt(Sql.deleteOffer, (id.value, o.value))) ++
                      uploadStatements(id, effective, extraction, version, failure, replacing) ++
                      (effective.outcome match
                        case UploadOutcome.Read(raw) =>
                          planStatements(
                            id,
                            keep,
                            LeistungPlan.plan(
                              raw,
                              upload.category,
                              v.leistungen.filter(_.category == upload.category),
                              LazyList.from(ids)
                            )
                          )
                        case UploadOutcome.Failed(_) => Nil)
                  )
                ).guaranteeCase {
                  case Outcome.Succeeded(_)                    => IO.unit
                  case Outcome.Errored(_) | Outcome.Canceled() => step.rollback
                }.flatMap {
                  case Left(e)  => step.rollback.as(Left(e))
                  case Right(_) => step.finish.attempt.as(Right(stored))
                }
              )
            )
          )
      }
    )

  /** When a favorite pointed at an offer we are about to delete, point it at the kept one. */
  private def favoriteRetarget(
      id: ProjectId,
      view: ProjectView,
      category: CategoryId,
      keep: OfferId,
      obsolete: List[OfferId]
  ): List[Stmt[?]] =
    view.project.favorites.get(category) match
      case Some(fav) if obsolete.contains(fav) =>
        List(Stmt(Sql.upsertFavorite, (id.value, category.value, keep.value)))
      case _ => Nil

  /** SQL statements that upsert the upload row and clear obsolete same-identity offers. */
  private def uploadStatements(
      id: ProjectId,
      upload: Upload,
      extraction: Option[String],
      version: Option[Int],
      failure: Option[String],
      replacing: Boolean
  ): List[Stmt[?]] =
    val row = (
      id.value,
      upload.id.value,
      upload.category.value,
      upload.fileName,
      upload.fileType.extension,
      upload.createdAt.atOffset(ZoneOffset.UTC),
      extraction,
      version,
      failure
    )
    if replacing then
      List(
        Stmt(Sql.deleteReadOfferItems, (id.value, upload.id.value)),
        Stmt(Sql.deleteReadOfferCoverage, (id.value, upload.id.value)),
        Stmt(Sql.updateUpload, row)
      )
    else
      List(
        Stmt(Sql.insertOffer, (id.value, upload.id.value)),
        Stmt(Sql.insertUpload, row)
      )

  /** Allocates `n` fresh Leistung ids with the given prefix. */
  private def freshIds(prefix: String, n: Int): IO[List[LeistungId]] =
    List
      .fill(n)(IO(java.util.UUID.randomUUID().toString.replace("-", "").take(10)))
      .sequence
      .map(_.flatMap(h => LeistungId.from(s"$prefix-$h").toOption))

  /** SQL statements that persist a [[LeistungPlan]] (new Leistungen, assignments, coverage). */
  private def planStatements(
      id: ProjectId,
      offer: OfferId,
      plan: LeistungPlan
  ): List[Stmt[?]] =
    val p = id.value
    plan.added.map(l =>
      Stmt(Sql.insertLeistung, (p, l.id.value, l.category.value, l.name.value))
    ) ++ plan.items.zipWithIndex.map((l, i) =>
      Stmt(Sql.insertReadItem, (p, offer.value, i, l.map(_.value)))
    ) ++ plan.coverage.map(c =>
      Stmt(
        Sql.insertReadCoverage,
        (
          p,
          offer.value,
          c.leistung.value,
          Coverage.code(c.coverage),
          c.page.map(_.value),
          c.confidence.toString.toLowerCase
        )
      )
    )

  /** Applies a manual [[Edit]] in a transaction under the lock and reloads. */
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

  /** SQL statements that apply one [[Edit]] inside a transaction. */
  private def editStatements(
      id: ProjectId,
      view: ProjectView,
      e: Edit,
      fresh: String
  ): Either[RepoError, List[Stmt[?]]] =
    val p = id.value
    e match
      case Edit.AddCategory(name)       => stmtsAddCategory(p, name, fresh)
      case Edit.RenameCategory(c, name) =>
        requireCategory(view, c).map(_ =>
          List(Stmt(Sql.upsertCategory, (p, c.value, name.value, false)))
        )
      case Edit.DeleteCategory(c)       => stmtsDeleteCategory(p, view, c)
      case Edit.AddLeistung(c, name)    => stmtsAddLeistung(p, view, c, name, fresh)
      case Edit.RenameLeistung(l, name) =>
        requireLeistung(view, l).map(_ => List(Stmt(Sql.renameLeistung, (p, l.value, name.value))))
      case Edit.MergeLeistung(from, into) => stmtsMergeLeistung(p, view, from, into)
      case Edit.DeleteLeistung(l)         =>
        requireLeistung(view, l).map(_ =>
          List(
            Stmt(Sql.unassignItems, (p, l.value)),
            Stmt(Sql.deleteCoverageOf, (p, l.value)),
            Stmt(Sql.deleteLeistung, (p, l.value))
          )
        )
      case Edit.AssignItem(o, item, target) => stmtsAssignItem(p, view, o, item, target)
      case Edit.RateContractor(c, rating)   =>
        Right(rating match
          case Some(r) => List(Stmt(Sql.upsertRating, (p, c.value, r.stars.value, r.note)))
          case None    => List(Stmt(Sql.deleteRating, (p, c.value))))
      case Edit.SetWeights(ws) =>
        Right(
          Criterion.values.toList.map(c => Stmt(Sql.upsertWeight, (p, Criterion.code(c), ws.of(c))))
        )
      case Edit.SetCoverage(o, l, coverage) => stmtsSetCoverage(p, view, o, l, coverage)

  /** Resolves a category id on the view, or [[RepoError.CategoryNotFound]]. */
  private def requireCategory(view: ProjectView, c: CategoryId): Either[RepoError, Category] =
    view.project.categories.find(_.id == c).toRight(RepoError.CategoryNotFound(c))

  /** Resolves a Leistung id on the view, or [[RepoError.LeistungNotFound]]. */
  private def requireLeistung(view: ProjectView, l: LeistungId): Either[RepoError, Leistung] =
    view.leistungen.find(_.id == l).toRight(RepoError.LeistungNotFound(l))

  /** Accepted or rejected uploads expose their raw extraction; failed ones do not. */
  private def requireUpload(view: ProjectView, o: OfferId): Either[RepoError, (Upload, RawOffer)] =
    view.uploads
      .find(_.upload.id == o)
      .flatMap(u =>
        u.status match
          case UploadStatus.Accepted(raw, _)    => Some(u.upload -> raw)
          case UploadStatus.Rejected(raw, _)    => Some(u.upload -> raw)
          case UploadStatus.ExtractionFailed(_) => None
      )
      .toRight(RepoError.OfferNotFound(o))

  /** Statements to insert a new category with a fresh id. */
  private def stmtsAddCategory(
      p: String,
      name: CategoryName,
      fresh: String
  ): Either[RepoError, List[Stmt[?]]] =
    CategoryId
      .from(s"k-$fresh")
      .left
      .map(RepoError.Invalid(_))
      .map(c => List(Stmt(Sql.upsertCategory, (p, c.value, name.value, false))))

  /** Statements to soft-delete a category and drop its Leistungen (guards: no uploads/refs). */
  private def stmtsDeleteCategory(
      p: String,
      view: ProjectView,
      c: CategoryId
  ): Either[RepoError, List[Stmt[?]]] =
    val project = view.project
    for
      cat <- requireCategory(view, c)
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
      Stmt(Sql.upsertCategory, (p, c.value, cat.name.value, true)),
      Stmt(Sql.deleteLeistungenOfCategory, (p, c.value))
    )

  /** Statements to insert a new Leistung under a category. */
  private def stmtsAddLeistung(
      p: String,
      view: ProjectView,
      c: CategoryId,
      name: LeistungName,
      fresh: String
  ): Either[RepoError, List[Stmt[?]]] =
    for
      _ <- requireCategory(view, c)
      l <- LeistungId.from(s"l-$fresh").left.map(RepoError.Invalid(_))
    yield List(Stmt(Sql.insertLeistung, (p, l.value, c.value, name.value)))

  /** Statements to merge two Leistungen of the same category (move items/coverage, delete source).
    */
  private def stmtsMergeLeistung(
      p: String,
      view: ProjectView,
      from: LeistungId,
      into: LeistungId
  ): Either[RepoError, List[Stmt[?]]] =
    for
      f <- requireLeistung(view, from)
      t <- requireLeistung(view, into)
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
      val moved = (p, from.value, into.value)
      List(
        Stmt(Sql.moveItems, moved),
        Stmt(Sql.mergeReadCoverage, moved),
        Stmt(Sql.moveCoverage, moved),
        Stmt(Sql.deleteCoverageOf, (p, from.value)),
        Stmt(Sql.deleteLeistung, (p, from.value))
      )

  /** Statements for a manual item assignment (cross-category targets allowed). */
  private def stmtsAssignItem(
      p: String,
      view: ProjectView,
      o: OfferId,
      item: Int,
      target: Option[LeistungId]
  ): Either[RepoError, List[Stmt[?]]] =
    for
      (_, raw) <- requireUpload(view, o)
      _ <- Either.cond(
        raw.lineItems.indices.contains(item),
        (),
        RepoError.ItemNotFound(o, item)
      )
      // Cross-category grouping is allowed: a line item of an offer uploaded under one
      // category may be assigned to a Leistung of another (manual scope grouping).
      _ <- target.traverse(requireLeistung(view, _))
    yield List(Stmt(Sql.upsertManualItem, (p, o.value, item, target.map(_.value))))

  /** Statements to set or clear manual coverage for an offer/Leistung pair. */
  private def stmtsSetCoverage(
      p: String,
      view: ProjectView,
      o: OfferId,
      l: LeistungId,
      coverage: Option[Coverage]
  ): Either[RepoError, List[Stmt[?]]] =
    for
      _ <- requireUpload(view, o)
      _ <- requireLeistung(view, l) // coverage may target a Leistung of another category
    yield coverage match
      case Some(c) =>
        List(Stmt(Sql.upsertManualCoverage, (p, o.value, l.value, Coverage.code(c))))
      case None => List(Stmt(Sql.deleteManualCoverage, (p, o.value, l.value)))

/** Same firm + same recipient in the same category: replace identity via [[OfferReplaceKey]].
  * Missing recipients share [[RecipientKey.Unknown]] and only match other unknowns — they never
  * overwrite a known recipient.
  */
object UploadReplace:
  /** Replace key from a raw extraction (firm + recipient), if the firm is recognised. */
  def keyOf(raw: RawOffer): Option[OfferReplaceKey] =
    raw.contractor.text
      .flatMap(ContractorKey.of)
      .map(firm => OfferReplaceKey(firm, RecipientKey.fromField(raw.recipient)))

  /** Replace key from an upload outcome. */
  def keyOf(outcome: UploadOutcome): Option[OfferReplaceKey] = outcome match
    case UploadOutcome.Read(raw) => keyOf(raw)
    case UploadOutcome.Failed(_) => None

  /** Replace key from a derived upload status. */
  def keyOf(status: UploadStatus): Option[OfferReplaceKey] = status match
    case UploadStatus.Accepted(raw, _)    => keyOf(raw)
    case UploadStatus.Rejected(raw, _)    => keyOf(raw)
    case UploadStatus.ExtractionFailed(_) => None

  /** Uploads of `category` with the same replace key, in load order (oldest first). */
  def sameIdentity(
      view: ProjectView,
      category: CategoryId,
      key: Option[OfferReplaceKey]
  ): List[Upload] =
    key.toList.flatMap(k =>
      view.uploads.collect {
        case UploadView(u, s) if u.category == category && keyOf(s).contains(k) => u
      }
    )

  /** Offer id to keep: the oldest same-identity upload in the category, else the upload's own id.
    */
  def kept(view: ProjectView, upload: Upload): OfferId =
    sameIdentity(view, upload.category, keyOf(upload.outcome)).headOption
      .map(_.id)
      .getOrElse(upload.id)

  /** Same-identity uploads in the category other than [[kept]], whose files and rows go away. */
  def obsolete(view: ProjectView, upload: Upload): List[Upload] =
    val keep = kept(view, upload)
    sameIdentity(view, upload.category, keyOf(upload.outcome)).filterNot(_.id == keep)

/** Skunk-backed [[ProjectRepository]]: load/decode rows, migrations and statement helpers. */
object PostgresProjectRepository:
  /** Version of the [[RawOffer]] JSON written to `uploads.extraction`. Version 2 added the optional
    * Leistung fields, so version 1 still decodes; other versions are not readable.
    */
  /** Version 3 adds optional `recipient` (Adressat); v1/v2 JSON still decodes (recipient absent).
    */
  val ExtractionVersion: Int = 3
  private val ReadableVersions: Set[Int] = Set(1, 2, 3)

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

  /** Decodes an assignment row; invalid ids are dropped. */
  private def toAssignment(r: AssignmentRow): Option[ItemAssignment] =
    for
      o <- OfferId.from(r.offer).toOption
      l <- r.leistung.traverse(LeistungId.from).toOption
    yield ItemAssignment(o, r.item, l, r.manual)

  /** Decodes a coverage row; invalid ids/status are dropped. */
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
    yield Upload(id, cat, r.fileName, ft, r.createdAt, outcome)

  /** Versioned schema migrations, recorded in `schema_migrations`. Applied in order, each in its
    * own transaction; never edit a released migration, add a new one instead.
    */
  private[repo] val migrations: List[(Int, List[Command[Void]])] = List(
    1 -> List(
      sql"""CREATE TABLE offers(project_id TEXT NOT NULL, id TEXT NOT NULL,
            PRIMARY KEY (project_id, id))""".command,
      sql"""CREATE TABLE uploads(project_id TEXT NOT NULL, id TEXT NOT NULL,
            category_id TEXT NOT NULL, file_name TEXT NOT NULL, file_type TEXT NOT NULL,
            created_at TIMESTAMPTZ NOT NULL, extraction TEXT, extraction_version INTEGER,
            failure TEXT, PRIMARY KEY (project_id, id),
            FOREIGN KEY (project_id, id) REFERENCES offers(project_id, id) ON DELETE CASCADE)""".command,
      sql"""CREATE TABLE favorites(project_id TEXT NOT NULL, category_id TEXT NOT NULL,
            offer_id TEXT NOT NULL, PRIMARY KEY (project_id, category_id),
            FOREIGN KEY (project_id, offer_id) REFERENCES offers(project_id, id) ON DELETE CASCADE)""".command,
      // Category changes, Leistungen per category, item assignments and coverage. Rows with
      // manual = true are user edits and take precedence over extracted ones.
      sql"""CREATE TABLE categories(project_id TEXT NOT NULL, id TEXT NOT NULL, name TEXT NOT NULL,
            position INTEGER NOT NULL, deleted BOOLEAN NOT NULL DEFAULT false,
            PRIMARY KEY (project_id, id))""".command,
      sql"""CREATE TABLE leistungen(project_id TEXT NOT NULL, id TEXT NOT NULL,
            category_id TEXT NOT NULL, name TEXT NOT NULL, position INTEGER NOT NULL,
            deleted BOOLEAN NOT NULL DEFAULT false, PRIMARY KEY (project_id, id))""".command,
      sql"""CREATE TABLE item_leistung(project_id TEXT NOT NULL, offer_id TEXT NOT NULL,
            item INTEGER NOT NULL, leistung_id TEXT, manual BOOLEAN NOT NULL,
            PRIMARY KEY (project_id, offer_id, item, manual),
            FOREIGN KEY (project_id, offer_id) REFERENCES offers(project_id, id) ON DELETE CASCADE,
            FOREIGN KEY (project_id, leistung_id) REFERENCES leistungen(project_id, id))""".command,
      sql"""CREATE TABLE abdeckung(project_id TEXT NOT NULL, offer_id TEXT NOT NULL,
            leistung_id TEXT NOT NULL, status TEXT NOT NULL, page INTEGER, confidence TEXT,
            manual BOOLEAN NOT NULL, PRIMARY KEY (project_id, offer_id, leistung_id, manual),
            FOREIGN KEY (project_id, offer_id) REFERENCES offers(project_id, id) ON DELETE CASCADE,
            FOREIGN KEY (project_id, leistung_id) REFERENCES leistungen(project_id, id))""".command,
      // Ranking: reference ratings per contractor (by normalised name) and criterion weights.
      sql"""CREATE TABLE ratings(project_id TEXT NOT NULL, contractor_key TEXT NOT NULL,
            stars INTEGER NOT NULL CHECK (stars BETWEEN 1 AND 5), note TEXT NOT NULL,
            PRIMARY KEY (project_id, contractor_key))""".command,
      sql"""CREATE TABLE weights(project_id TEXT NOT NULL, criterion TEXT NOT NULL,
            weight INTEGER NOT NULL, PRIMARY KEY (project_id, criterion))""".command
    ),
    // New default trade categories (Dach, Fenster, Fassadendämmung, Elektrik, Heizung,
    // Innenausbau). Data stored for the retired default categories (only test data) is removed:
    // their uploaded offers (cascading to uploads, favorites, assignments, coverage), their
    // Leistungen, category edits and favorites, and the retired seed offers. Categories added by
    // users keep their data.
    2 -> List(
      sql"""DELETE FROM offers o WHERE o.id IN ('daemmung-a', 'daemmung-b', 'spengler-a', 'balkon-a')
            OR EXISTS (SELECT 1 FROM uploads u WHERE u.project_id = o.project_id AND u.id = o.id
            AND u.category_id IN ('gauben', 'daemmung', 'spenglerarbeiten', 'balkondaemmung',
            'dachfenster', 'sonstiges'))""".command,
      sql"""DELETE FROM favorites WHERE category_id IN ('gauben', 'daemmung', 'spenglerarbeiten',
            'balkondaemmung', 'dachfenster', 'sonstiges')""".command,
      sql"""DELETE FROM item_leistung i USING leistungen l
            WHERE i.project_id = l.project_id AND i.leistung_id = l.id
            AND l.category_id IN ('gauben', 'daemmung', 'spenglerarbeiten', 'balkondaemmung',
            'dachfenster', 'sonstiges')""".command,
      sql"""DELETE FROM abdeckung a USING leistungen l
            WHERE a.project_id = l.project_id AND a.leistung_id = l.id
            AND l.category_id IN ('gauben', 'daemmung', 'spenglerarbeiten', 'balkondaemmung',
            'dachfenster', 'sonstiges')""".command,
      sql"""DELETE FROM leistungen WHERE category_id IN ('gauben', 'daemmung', 'spenglerarbeiten',
            'balkondaemmung', 'dachfenster', 'sonstiges')""".command,
      sql"""DELETE FROM categories WHERE id IN ('gauben', 'daemmung', 'spenglerarbeiten',
            'balkondaemmung', 'dachfenster', 'sonstiges')""".command
    )
  )

  /** Arbitrary constant for `pg_advisory_xact_lock`, so parallel starts migrate one at a time. */
  private val MigrationLock = 4711_2026L

  /** Applies pending schema migrations under an advisory transaction lock. */
  private def migrate(db: Postgres): IO[Either[RepoError, Unit]] =
    migrations
      .traverse_((version, statements) => EitherT(db.migration(MigrationLock, version, statements)))
      .value

  /** Connects to the database and creates or migrates the schema. */
  def resource(
      config: DbConfig,
      projects: List[Project] = Nil
  ): Resource[IO, PostgresProjectRepository] =
    for
      db <- Postgres.resource(config)
      _ <- Resource.eval(
        migrate(db).flatMap(
          _.fold(
            e => IO.raiseError(IllegalStateException(s"Datenbank-Migration fehlgeschlagen: $e")),
            IO.pure
          )
        )
      )
      lock <- Resource.eval(Mutex[IO])
    yield PostgresProjectRepository(db, lock, projects.map(p => p.id -> p).toMap)

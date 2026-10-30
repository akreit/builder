package builder.repo

import builder.domain.*
import builder.extraction.*
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.Mutex
import cats.syntax.all.*
import io.circe.parser.decode
import io.circe.syntax.*
import skunk.codec.all.*

import java.time.Instant

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

/** One `uploads` row as stored; decoded into an [[Upload]] by [[PostgresProjectRepository]]. */
private final case class UploadRow(
    id: String,
    category: String,
    fileName: String,
    fileType: String,
    createdAt: Instant,
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

/** Postgres-backed repository. Projects (categories, work packages, seed offers) come from `seeds`;
  * uploads and favorites are stored in the database, so they survive restarts. Every read
  * re-validates stored extractions with the domain smart constructors.
  */
final class PostgresProjectRepository private (
    db: Postgres,
    lock: Mutex[IO],
    seeds: Map[ProjectId, Project]
) extends ProjectRepository:
  import PostgresProjectRepository.*

  private def loadRows(id: ProjectId): IO[Either[RepoError, List[UploadRow]]] =
    db.query(
      """SELECT id, category_id, file_name, file_type, created_at, extraction,
        |  extraction_version, failure
        |FROM uploads WHERE project_id = ? ORDER BY created_at, id""".stripMargin,
      List(Param.text(id.value))
    )(
      (text *: text *: text *: text *: timestamptz *: text.opt *: int4.opt *: text.opt).map {
        case (i, c, n, t, at, e, v, f) => UploadRow(i, c, n, t, at.toInstant, e, v, f)
      }
    )

  private def loadCategories(id: ProjectId): IO[Either[RepoError, List[CategoryRow]]] =
    db.query(
      "SELECT id, name, position, deleted FROM categories WHERE project_id = ? ORDER BY position, id",
      List(Param.text(id.value))
    )((text *: text *: int4 *: bool).to[CategoryRow])

  private def loadLeistungen(id: ProjectId): IO[Either[RepoError, List[LeistungRow]]] =
    db.query(
      """SELECT id, category_id, name FROM leistungen
        |WHERE project_id = ? AND NOT deleted ORDER BY position, id""".stripMargin,
      List(Param.text(id.value))
    )((text *: text *: text).to[LeistungRow])

  private def loadAssignments(id: ProjectId): IO[Either[RepoError, List[AssignmentRow]]] =
    db.query(
      "SELECT offer_id, item, leistung_id, manual FROM item_leistung WHERE project_id = ?",
      List(Param.text(id.value))
    )((text *: int4 *: text.opt *: bool).to[AssignmentRow])

  private def loadCoverage(id: ProjectId): IO[Either[RepoError, List[CoverageRow]]] =
    db.query(
      "SELECT offer_id, leistung_id, status, page, confidence, manual FROM abdeckung WHERE project_id = ?",
      List(Param.text(id.value))
    )((text *: text *: text *: int4.opt *: text.opt *: bool).to[CoverageRow])

  private def loadFavorites(id: ProjectId): IO[Either[RepoError, List[(String, String)]]] =
    db.query(
      "SELECT category_id, offer_id FROM favorites WHERE project_id = ? ORDER BY category_id",
      List(Param.text(id.value))
    )(text *: text)

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
          rs <- EitherT(
            db.query(
              "SELECT contractor_key, stars, note FROM ratings WHERE project_id = ?",
              List(Param.text(id.value))
            )(text *: int4 *: text)
          )
          ws <- EitherT(
            db.query(
              "SELECT criterion, weight FROM weights WHERE project_id = ?",
              List(Param.text(id.value))
            )(text *: int4)
          )
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
                    """INSERT INTO favorites(project_id, category_id, offer_id) VALUES (?, ?, ?)
                      |ON CONFLICT (project_id, category_id) DO UPDATE SET offer_id = excluded.offer_id""".stripMargin ->
                      List(
                        Param.text(id.value),
                        Param.text(category.value),
                        Param.text(offer.value)
                      )
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
              (Some(raw.asJson.noSpaces), Some(ExtractionVersion), None)
            case UploadOutcome.Failed(m) => (None, None, Some(m))
          val proposed = upload.outcome match
            case UploadOutcome.Read(raw) => LeistungPlan.proposals(raw).size
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
      version: Option[Int],
      failure: Option[String]
  ): List[(String, List[Param])] =
    List(
      "INSERT INTO offers(id, project_id) VALUES (?, ?)" ->
        List(Param.text(upload.id.value), Param.text(id.value)),
      """INSERT INTO uploads(id, project_id, category_id, file_name, file_type, created_at,
                |  extraction, extraction_version, failure)
                |VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)""".stripMargin -> List(
        Param.text(upload.id.value),
        Param.text(id.value),
        Param.text(upload.category.value),
        Param.text(upload.fileName),
        Param.text(upload.fileType.extension),
        Param.Timestamp(upload.createdAt),
        Param.Text(extraction),
        Param.Int4(version),
        Param.Text(failure)
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
  ): List[(String, List[Param])] =
    val p = Param.text(id.value)
    plan.added.map(l =>
      """INSERT INTO leistungen(project_id, id, category_id, name, position, deleted)
        |VALUES (?, ?, ?, ?, (SELECT COALESCE(MAX(position), 0) + 1 FROM leistungen WHERE project_id = ?), false)""".stripMargin ->
        List(p, Param.text(l.id.value), Param.text(l.category.value), Param.text(l.name.value), p)
    ) ++ plan.items.zipWithIndex.map((l, i) =>
      "INSERT INTO item_leistung(project_id, offer_id, item, leistung_id, manual) VALUES (?, ?, ?, ?, false)" ->
        List(p, Param.text(offer.value), Param.int(i), Param.Text(l.map(_.value)))
    ) ++ plan.coverage.map(c =>
      """INSERT INTO abdeckung(project_id, offer_id, leistung_id, status, page, confidence, manual)
        |VALUES (?, ?, ?, ?, ?, ?, false)""".stripMargin ->
        List(
          p,
          Param.text(offer.value),
          Param.text(c.leistung.value),
          Param.text(Coverage.code(c.coverage)),
          Param.Int4(c.page.map(_.value)),
          Param.text(c.confidence.toString.toLowerCase)
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
  ): Either[RepoError, List[(String, List[Param])]] =
    val p = Param.text(id.value)
    val project = view.project
    def category(c: CategoryId): Either[RepoError, Category] =
      project.categories.find(_.id == c).toRight(RepoError.CategoryNotFound(c))
    def leistung(l: LeistungId): Either[RepoError, Leistung] =
      view.leistungen
        .find(_.id == l)
        .toRight(RepoError.LeistungNotFound(l))
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
          .map(c =>
            List(
              upsertCategory -> List(
                p,
                Param.text(c.value),
                Param.text(name.value),
                p,
                Param.Bool(false)
              )
            )
          )
      case Edit.RenameCategory(c, name) =>
        category(c).map(_ =>
          List(
            upsertCategory -> List(
              p,
              Param.text(c.value),
              Param.text(name.value),
              p,
              Param.Bool(false)
            )
          )
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
          upsertCategory -> List(
            p,
            Param.text(c.value),
            Param.text(cat.name.value),
            p,
            Param.Bool(true)
          ),
          "UPDATE leistungen SET deleted = true WHERE project_id = ? AND category_id = ?" -> List(
            p,
            Param.text(c.value)
          )
        )
      case Edit.AddLeistung(c, name) =>
        for
          _ <- category(c)
          l <- LeistungId.from(s"l-$fresh").left.map(RepoError.Invalid(_))
        yield List(
          """INSERT INTO leistungen(project_id, id, category_id, name, position, deleted)
            |VALUES (?, ?, ?, ?, (SELECT COALESCE(MAX(position), 0) + 1 FROM leistungen WHERE project_id = ?), false)""".stripMargin ->
            List(p, Param.text(l.value), Param.text(c.value), Param.text(name.value), p)
        )
      case Edit.RenameLeistung(l, name) =>
        leistung(l).map(_ =>
          List(
            "UPDATE leistungen SET name = ? WHERE project_id = ? AND id = ?" -> List(
              Param.text(name.value),
              p,
              Param.text(l.value)
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
          val ids = List(p, Param.text(from.value))
          List(
            "UPDATE item_leistung SET leistung_id = ? WHERE project_id = ? AND leistung_id = ?" -> (Param
              .text(into.value) :: ids),
            // Read statuses of the target become "enthalten" where the merged one was.
            """UPDATE abdeckung SET status = 'enthalten'
              |WHERE project_id = ? AND leistung_id = ? AND NOT manual AND offer_id IN
              |  (SELECT offer_id FROM abdeckung WHERE project_id = ? AND leistung_id = ? AND NOT manual
              |   AND status = 'enthalten')""".stripMargin -> List(
              p,
              Param.text(into.value),
              p,
              Param.text(from.value)
            ),
            // Rows that would collide with an existing row of the target are deleted below.
            """UPDATE abdeckung a SET leistung_id = ? WHERE a.project_id = ? AND a.leistung_id = ?
              |AND NOT EXISTS (SELECT 1 FROM abdeckung b WHERE b.project_id = a.project_id
              |  AND b.offer_id = a.offer_id AND b.manual = a.manual AND b.leistung_id = ?)""".stripMargin ->
              ((Param.text(into.value) :: ids) :+ Param.text(into.value)),
            "DELETE FROM abdeckung WHERE project_id = ? AND leistung_id = ?" -> ids,
            "UPDATE leistungen SET deleted = true WHERE project_id = ? AND id = ?" -> ids
          )
      case Edit.DeleteLeistung(l) =>
        leistung(l).map { _ =>
          val ids = List(p, Param.text(l.value))
          List(
            "UPDATE item_leistung SET leistung_id = NULL WHERE project_id = ? AND leistung_id = ?" -> ids,
            "DELETE FROM abdeckung WHERE project_id = ? AND leistung_id = ?" -> ids,
            "UPDATE leistungen SET deleted = true WHERE project_id = ? AND id = ?" -> ids
          )
        }
      case Edit.AssignItem(o, item, target) =>
        for
          (u, raw) <- upload(o)
          _ <- Either.cond(
            raw.lineItems.indices.contains(item),
            (),
            RepoError.ItemNotFound(o, item)
          )
          _ <- target.traverse(l => leistung(l).flatMap(sameCategory(u, _)))
        yield List(
          """INSERT INTO item_leistung(project_id, offer_id, item, leistung_id, manual)
            |VALUES (?, ?, ?, ?, true)
            |ON CONFLICT (project_id, offer_id, item, manual)
            |DO UPDATE SET leistung_id = excluded.leistung_id""".stripMargin -> List(
            p,
            Param.text(o.value),
            Param.int(item),
            Param.Text(target.map(_.value))
          )
        )
      case Edit.RateContractor(c, rating) =>
        Right(rating match
          case Some(r) =>
            List(
              """INSERT INTO ratings(project_id, contractor_key, stars, note) VALUES (?, ?, ?, ?)
                |ON CONFLICT (project_id, contractor_key)
                |DO UPDATE SET stars = excluded.stars, note = excluded.note""".stripMargin ->
                List(p, Param.text(c.value), Param.int(r.stars.value), Param.text(r.note))
            )
          case None =>
            List(
              "DELETE FROM ratings WHERE project_id = ? AND contractor_key = ?" -> List(
                p,
                Param.text(c.value)
              )
            ))
      case Edit.SetWeights(ws) =>
        Right(
          Criterion.values.toList.map(c =>
            """INSERT INTO weights(project_id, criterion, weight) VALUES (?, ?, ?)
              |ON CONFLICT (project_id, criterion) DO UPDATE SET weight = excluded.weight""".stripMargin ->
              List(p, Param.text(Criterion.code(c)), Param.int(ws.of(c)))
          )
        )
      case Edit.SetCoverage(o, l, coverage) =>
        for
          (u, _) <- upload(o)
          _ <- leistung(l).flatMap(sameCategory(u, _))
        yield coverage match
          case Some(c) =>
            List(
              """INSERT INTO abdeckung(project_id, offer_id, leistung_id, status, page, confidence, manual)
                |VALUES (?, ?, ?, ?, NULL, NULL, true)
                |ON CONFLICT (project_id, offer_id, leistung_id, manual)
                |DO UPDATE SET status = excluded.status, page = NULL, confidence = NULL""".stripMargin ->
                List(p, Param.text(o.value), Param.text(l.value), Param.text(Coverage.code(c)))
            )
          case None =>
            List(
              "DELETE FROM abdeckung WHERE project_id = ? AND offer_id = ? AND leistung_id = ? AND manual" ->
                List(p, Param.text(o.value), Param.text(l.value))
            )

object PostgresProjectRepository:
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
    yield Upload(id, cat, r.fileName, ft, r.createdAt, outcome)

  /** Versioned schema migrations, recorded in `schema_migrations`. Applied in order, each in its
    * own transaction; never edit a released migration, add a new one instead.
    */
  private[repo] val migrations: List[(Int, List[String])] = List(
    1 -> List(
      """CREATE TABLE offers(project_id TEXT NOT NULL, id TEXT NOT NULL,
        |  PRIMARY KEY (project_id, id))""".stripMargin,
      """CREATE TABLE uploads(project_id TEXT NOT NULL, id TEXT NOT NULL,
        |  category_id TEXT NOT NULL, file_name TEXT NOT NULL, file_type TEXT NOT NULL,
        |  created_at TIMESTAMPTZ NOT NULL, extraction TEXT, extraction_version INTEGER,
        |  failure TEXT, PRIMARY KEY (project_id, id),
        |  FOREIGN KEY (project_id, id) REFERENCES offers(project_id, id) ON DELETE CASCADE)""".stripMargin,
      """CREATE TABLE favorites(project_id TEXT NOT NULL, category_id TEXT NOT NULL,
        |  offer_id TEXT NOT NULL, PRIMARY KEY (project_id, category_id),
        |  FOREIGN KEY (project_id, offer_id) REFERENCES offers(project_id, id) ON DELETE CASCADE)""".stripMargin,
      // Category changes, Leistungen per category, item assignments and coverage. Rows with
      // manual = true are user edits and take precedence over extracted ones.
      """CREATE TABLE categories(project_id TEXT NOT NULL, id TEXT NOT NULL, name TEXT NOT NULL,
        |  position INTEGER NOT NULL, deleted BOOLEAN NOT NULL DEFAULT false,
        |  PRIMARY KEY (project_id, id))""".stripMargin,
      """CREATE TABLE leistungen(project_id TEXT NOT NULL, id TEXT NOT NULL,
        |  category_id TEXT NOT NULL, name TEXT NOT NULL, position INTEGER NOT NULL,
        |  deleted BOOLEAN NOT NULL DEFAULT false, PRIMARY KEY (project_id, id))""".stripMargin,
      """CREATE TABLE item_leistung(project_id TEXT NOT NULL, offer_id TEXT NOT NULL,
        |  item INTEGER NOT NULL, leistung_id TEXT, manual BOOLEAN NOT NULL,
        |  PRIMARY KEY (project_id, offer_id, item, manual),
        |  FOREIGN KEY (project_id, offer_id) REFERENCES offers(project_id, id) ON DELETE CASCADE,
        |  FOREIGN KEY (project_id, leistung_id) REFERENCES leistungen(project_id, id))""".stripMargin,
      """CREATE TABLE abdeckung(project_id TEXT NOT NULL, offer_id TEXT NOT NULL,
        |  leistung_id TEXT NOT NULL, status TEXT NOT NULL, page INTEGER, confidence TEXT,
        |  manual BOOLEAN NOT NULL, PRIMARY KEY (project_id, offer_id, leistung_id, manual),
        |  FOREIGN KEY (project_id, offer_id) REFERENCES offers(project_id, id) ON DELETE CASCADE,
        |  FOREIGN KEY (project_id, leistung_id) REFERENCES leistungen(project_id, id))""".stripMargin,
      // Ranking: reference ratings per contractor (by normalised name) and criterion weights.
      """CREATE TABLE ratings(project_id TEXT NOT NULL, contractor_key TEXT NOT NULL,
        |  stars INTEGER NOT NULL CHECK (stars BETWEEN 1 AND 5), note TEXT NOT NULL,
        |  PRIMARY KEY (project_id, contractor_key))""".stripMargin,
      """CREATE TABLE weights(project_id TEXT NOT NULL, criterion TEXT NOT NULL,
        |  weight INTEGER NOT NULL, PRIMARY KEY (project_id, criterion))""".stripMargin
    )
  )

  /** Arbitrary constant for `pg_advisory_xact_lock`, so parallel starts migrate one at a time. */
  private val MigrationLock = 4711_2026L

  private def migrate(
      db: Postgres,
      seeds: List[Project],
      leistungen: List[(ProjectId, Leistung)]
  ): IO[Either[RepoError, Unit]] =
    val seedOffers = seeds.flatMap(p => p.offers.map(o => (o.id.value, p.id.value)))
    val steps = for
      _ <- EitherT(
        db.transact(
          List(
            """CREATE TABLE IF NOT EXISTS schema_migrations(version INTEGER PRIMARY KEY,
              |  applied_at TIMESTAMPTZ NOT NULL DEFAULT now())""".stripMargin -> Nil
          )
        )
      )
      _ <- migrations.traverse_((version, statements) =>
        EitherT(db.migration(MigrationLock, version, statements))
      )
      _ <- EitherT(
        db.transact(
          seedOffers.map((o, p) =>
            "INSERT INTO offers(id, project_id) VALUES (?, ?) ON CONFLICT DO NOTHING" ->
              List(Param.text(o), Param.text(p))
          ) ++ leistungen.zipWithIndex.map { case ((p, l), i) =>
            // Seed Leistungen are inserted once; renames and deletions of them are kept.
            """INSERT INTO leistungen(project_id, id, category_id, name, position, deleted)
              |VALUES (?, ?, ?, ?, ?, false) ON CONFLICT DO NOTHING""".stripMargin ->
              List(
                Param.text(p.value),
                Param.text(l.id.value),
                Param.text(l.category.value),
                Param.text(l.name.value),
                Param.int(i - 1000)
              )
          }
        )
      )
    yield ()
    steps.value

  /** Connects to the database and creates or migrates the schema. */
  def resource(
      config: DbConfig,
      seeds: List[Project],
      leistungen: List[(ProjectId, Leistung)] = Nil
  ): Resource[IO, PostgresProjectRepository] =
    for
      db <- Postgres.resource(config)
      _ <- Resource.eval(
        migrate(db, seeds, leistungen).flatMap(
          _.fold(
            e => IO.raiseError(IllegalStateException(s"Datenbank-Migration fehlgeschlagen: $e")),
            IO.pure
          )
        )
      )
      lock <- Resource.eval(Mutex[IO])
    yield PostgresProjectRepository(db, lock, seeds.map(p => p.id -> p).toMap)

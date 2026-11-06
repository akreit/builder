package builder.http

import builder.domain.*
import builder.extraction.*
import builder.repo.*
import builder.storage.FileStore
import builder.totals.*
import builder.ui.Pages
import cats.effect.Clock
import cats.effect.IO
import cats.effect.Outcome
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import org.http4s.HttpRoutes
import sttp.model.StatusCode
import sttp.monad.MonadError
import sttp.tapir.*
import sttp.tapir.files.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.server.http4s.Http4sServerOptions
import sttp.tapir.server.interceptor.DecodeFailureContext
import sttp.tapir.server.interceptor.decodefailure.DecodeFailureHandler
import sttp.tapir.server.interceptor.decodefailure.DefaultDecodeFailureHandler
import sttp.tapir.server.model.EndpointExtensions.*
import sttp.tapir.server.model.ValuedEndpointOutput

/** Tapir endpoint descriptions for health, totals, HTML pages, edits and uploads. */
object Endpoints:
  val comparisonPdf = endpoint.get
    .in("projekte" / path[String]("projectId") / "leistungsvergleich.pdf")
    .out(header("Content-Type", "application/pdf"))
    .out(header("Content-Disposition", "attachment; filename=leistungsvergleich.pdf"))
    .out(byteArrayBody)
    .errorOut(ApiError.output)

  val health =
    endpoint.get.in("health").out(jsonBody[HealthResponse])

  val totals =
    endpoint.get
      .in("projects" / path[String]("id") / "totals")
      .out(jsonBody[TotalsResponse])
      .errorOut(ApiError.output)

  private val htmlError: EndpointOutput[HtmlError] =
    statusCode.and(htmlBodyUtf8).mapTo[HtmlError]

  /** Endpoints whose decode failures are rendered as htmx error fragments. */
  val htmxNames: Set[String] = Set("upload-offer", "mark-favorite", "delete-offer", "edit")

  /** Start route of the home project: redirects to its first category tab. */
  val landing =
    endpoint.get
      .in("")
      .out(
        oneOf[Landing](
          oneOfVariant(
            statusCode(StatusCode.SeeOther).and(header[String]("Location")).mapTo[Landing.Redirect]
          ),
          oneOfVariant(statusCode(StatusCode.Ok).and(htmlBodyUtf8).mapTo[Landing.Page])
        )
      )
      .errorOut(htmlError)

  /** The page (tab) of one category; an unknown project or category is 404. */
  val categoryPage =
    endpoint.get
      .in("projekte" / path[String]("projectId") / "kategorien" / path[String]("categoryId"))
      .out(htmlBodyUtf8)
      .errorOut(htmlError)

  /** htmx fragment error: retargets the swap into the layout's `#fehler` slot. */
  val fragmentError: EndpointOutput[HtmlError] =
    statusCode
      .and(header("HX-Retarget", "#fehler"))
      .and(header("HX-Reswap", "innerHTML"))
      .and(htmlBodyUtf8)
      .mapTo[HtmlError]

  private val htmxHeaders: EndpointInput[HtmxHeaders] =
    header[Option[String]]("HX-Request")
      .and(header[Option[String]]("Origin"))
      .and(header[Option[String]]("Host"))
      .mapTo[HtmxHeaders]

  /** htmx: marks an offer as favorite and returns the re-rendered overview fragment. */
  val markFavorite =
    endpoint.post
      .name("mark-favorite")
      .in(
        "ui" / "projects" / path[String]("projectId") / "categories" / path[String](
          "categoryId"
        ) / "favorite" / path[String]("offerId")
      )
      .securityIn(htmxHeaders)
      .out(htmlBodyUtf8)
      .errorOut(fragmentError)

  /** htmx: deletes an uploaded offer (and cascaded rows) and returns the re-rendered fragment. */
  val deleteOffer =
    endpoint.post
      .name("delete-offer")
      .in(
        "ui" / "projects" / path[String]("projectId") / "categories" / path[String](
          "categoryId"
        ) / "angebote" / path[String]("offerId") / "loeschen"
      )
      .securityIn(htmxHeaders)
      .out(htmlBodyUtf8)
      .errorOut(fragmentError)

  /** htmx multipart: uploads an offer file (PDF/JPEG/PNG) into a category, reads it and returns the
    * re-rendered overview fragment.
    */
  val uploadOffer =
    endpoint.post
      .name("upload-offer")
      .in(
        "ui" / "projects" / path[String]("projectId") / "categories" / path[String](
          "categoryId"
        ) / "angebote"
      )
      .securityIn(htmxHeaders)
      .in(multipartBody[UploadForm])
      .out(htmlBodyUtf8)
      .errorOut(fragmentError)

  /** htmx form post: one manual change to categories, Leistungen, assignments or coverage (field
    * `aktion`, see [[Api.parseEdit]]); returns the re-rendered overview fragment.
    */
  val edit =
    endpoint.post
      .name("edit")
      .in("ui" / "projects" / path[String]("projectId") / "aenderungen")
      .securityIn(htmxHeaders)
      .in(formBody[Map[String, String]])
      .out(header[String]("HX-Push-Url").and(htmlBodyUtf8))
      .errorOut(fragmentError)
      .maxRequestBodyLength(Api.MaxEditBytes)

object Api:
  /** Edit forms carry a handful of short fields; anything larger is rejected (413). */
  val MaxEditBytes: Long = 16L * 1024

  /** Reads a form of the edit endpoint. Fields: `aktion` plus, depending on it, `kategorie`,
    * `leistung`, `ziel` (target Leistung, empty for none), `angebot`, `position` (1-based),
    * `status` (a coverage code or `auto`) and `name`.
    */
  def parseEdit(form: Map[String, String]): Either[String, Edit] =
    def field(k: String): Either[String, String] =
      form.get(k).map(_.trim).filter(_.nonEmpty).toRight(s"Feld '$k' fehlt")
    def domain[A](e: Either[DomainError, A]): Either[String, A] = e.left.map(Pages.describe)
    def category = field("kategorie").flatMap(c => domain(CategoryId.from(c)))
    def categoryName = field("name").flatMap(n => domain(CategoryName.from(n)))
    def newCategoryName =
      form.get("name").map(_.trim).filter(_.nonEmpty) match
        case Some(n) => domain(CategoryName.from(n))
        case None    => Left("Kategorie: Bitte einen Namen eingeben.")
    def leistung(k: String) = field(k).flatMap(l => domain(LeistungId.from(l)))
    def leistungName = field("name").flatMap(n => domain(LeistungName.from(n)))
    def offer = field("angebot").flatMap(o => domain(OfferId.from(o)))
    def position = field("position").flatMap(
      _.toIntOption.filter(_ >= 1).map(_ - 1).toRight("Ungültige Position")
    )
    field("aktion").flatMap {
      case "kategorie-neu"        => newCategoryName.map(Edit.AddCategory(_))
      case "kategorie-umbenennen" => (category, categoryName).mapN(Edit.RenameCategory(_, _))
      case "kategorie-loeschen"   => category.map(Edit.DeleteCategory(_))
      case "leistung-neu"         => (category, leistungName).mapN(Edit.AddLeistung(_, _))
      case "leistung-umbenennen"  =>
        (leistung("leistung"), leistungName).mapN(Edit.RenameLeistung(_, _))
      case "leistung-zusammenfuehren" =>
        (leistung("leistung"), leistung("ziel")).mapN(Edit.MergeLeistung(_, _))
      case "leistung-loeschen" => leistung("leistung").map(Edit.DeleteLeistung(_))
      case "position-zuordnen" =>
        val target = form.get("ziel").map(_.trim).filter(_.nonEmpty) match
          case Some(l) => domain(LeistungId.from(l)).map(Some(_))
          case None    => Right(None)
        (offer, position, target).mapN(Edit.AssignItem(_, _, _))
      case "status-setzen" =>
        val status = field("status").flatMap {
          case "auto" => Right(None)
          case code   => Coverage.fromCode(code).map(Some(_)).toRight(s"Unbekannter Status: $code")
        }
        (offer, leistung("leistung"), status).mapN(Edit.SetCoverage(_, _, _))
      case "bewertung" =>
        val key = field("firma").flatMap(ContractorKey.of(_).toRight("Feld 'firma' fehlt"))
        val note = form.get("notiz").map(_.trim).getOrElse("")
        val noteOk = Either.cond(
          note.length <= Rating.MaxNote,
          (),
          s"Notiz zu lang (höchstens ${Rating.MaxNote} Zeichen)"
        )
        val rating = noteOk.flatMap(_ =>
          form.get("sterne").map(_.trim).filter(_.nonEmpty) match
            case None    => Right(None)
            case Some(n) =>
              n.toIntOption
                .toRight(s"Ungültige Sternezahl: $n")
                .flatMap(i => domain(Stars.from(i)))
                .map(st => Some(Rating(st, note)))
        )
        (key, rating).mapN(Edit.RateContractor(_, _))
      case "gewichtung" =>
        Criterion.values.toList
          .traverse(c =>
            field(s"gewicht-${Criterion.code(c)}")
              .flatMap(_.toIntOption.toRight(s"Ungültige Gewichtung für ${Criterion.label(c)}"))
              .map(c -> _)
          )
          .flatMap(ws => domain(Weights.from(ws.toMap)))
          .map(Edit.SetWeights(_))
      case other => Left(s"Unbekannte Aktion: $other")
    }

  /** Largest file (PDF, 20 MB) plus room for the multipart framing; larger bodies get 413 without
    * being read.
    */
  val MaxRequestBytes: Long = OfferFileType.Pdf.maxBytes.toLong + 64 * 1024

/** HTTP wiring: maps endpoints to repository, file store and HTML pages. */
final class Api(
    repo: ProjectRepository,
    home: ProjectId,
    extractor: OfferExtractor[IO],
    files: FileStore
):
  private def totalsLogic(raw: String): IO[Either[ApiError, TotalsResponse]] =
    ProjectId.from(raw) match
      case Left(_)   => IO.pure(Left(ApiError.InvalidProjectId(raw)))
      case Right(id) =>
        repo
          .find(id)
          .map(
            _.map(v => TotalsResponse.from(Totals.compute(v.project))).left.map {
              case RepoError.Storage(code) => ApiError.StorageFailed(code.value)
              case RepoError.ProjectNotFound(_) | RepoError.CategoryNotFound(_) |
                  RepoError.OfferNotFound(_) | RepoError.LeistungNotFound(_) |
                  RepoError.ItemNotFound(_, _) | RepoError.Invalid(_) | RepoError.Rejected(_) =>
                ApiError.ProjectNotFound(raw)
            }
          )

  private def comparisonPdfLogic(raw: String): IO[Either[ApiError, Array[Byte]]] =
    ProjectId.from(raw) match
      case Left(_)   => IO.pure(Left(ApiError.InvalidProjectId(raw)))
      case Right(id) =>
        repo.find(id).flatMap {
          case Left(RepoError.Storage(code)) => IO.pure(Left(ApiError.StorageFailed(code.value)))
          case Left(
                RepoError.ProjectNotFound(_) | RepoError.CategoryNotFound(_) |
                RepoError.OfferNotFound(_) | RepoError.LeistungNotFound(_) |
                RepoError.ItemNotFound(_, _) | RepoError.Invalid(_) | RepoError.Rejected(_)
              ) =>
            IO.pure(Left(ApiError.ProjectNotFound(raw)))
          case Right(view) =>
            IO.blocking(builder.pdf.ComparisonPdf.render(view))
              .map(_.left.map(_ => ApiError.StorageFailed("pdf-export")))
        }

  private def notFound(what: String): HtmlError =
    HtmlError(StatusCode.NotFound, Pages.message(s"$what nicht gefunden"))

  private def pageError(e: RepoError): HtmlError = e match
    case RepoError.Storage(code) =>
      HtmlError(
        StatusCode.InternalServerError,
        Pages.message(s"Datenbankfehler (Code ${code.value})")
      )
    case RepoError.CategoryNotFound(_) => notFound("Kategorie")
    case RepoError.ProjectNotFound(_) | RepoError.OfferNotFound(_) | RepoError.LeistungNotFound(_) |
        RepoError.ItemNotFound(_, _) | RepoError.Invalid(_) | RepoError.Rejected(_) =>
      notFound("Projekt")

  private val landing: IO[Either[HtmlError, Landing]] =
    repo
      .find(home)
      .map(
        _.map(v =>
          Pages.activeCategory(v, None) match
            case Some(c) => Landing.Redirect(Pages.categoryUrl(v.project.id, c.id))
            case None    => Landing.Page(Pages.categoryPage(v, None))
        ).left.map(pageError)
      )

  private def categoryPageLogic(
      projectId: String,
      categoryId: String
  ): IO[Either[HtmlError, String]] =
    (ProjectId.from(projectId), CategoryId.from(categoryId)).tupled match
      case Left(_)       => IO.pure(Left(notFound("Seite")))
      case Right((p, c)) =>
        repo
          .find(p)
          .map(
            _.flatMap(v =>
              v.project.categories
                .find(_.id == c)
                .toRight(RepoError.CategoryNotFound(c))
                .map(cat => Pages.categoryPage(v, Some(cat)))
            ).left.map(pageError)
          )

  private def fragmentError(status: StatusCode, text: String): HtmlError =
    HtmlError(status, Pages.errorFragment(text))

  /** CSRF guard: only htmx requests, and an Origin (if sent) must match the Host. */
  private def checkRequest(h: HtmxHeaders): IO[Either[HtmlError, Unit]] = IO.pure {
    val forbidden = fragmentError(StatusCode.Forbidden, _)
    val sameOrigin = h.origin.forall(o => o.split("://", 2).lift(1).exists(h.host.contains))
    if !h.hxRequest.contains("true") then
      Left(forbidden("Anfrage abgelehnt: nur über die Oberfläche erlaubt"))
    else if !sameOrigin then Left(forbidden("Anfrage abgelehnt: fremde Herkunft"))
    else Right(())
  }

  private def favoriteLogic(
      projectId: String,
      categoryId: String,
      offerId: String
  ): IO[Either[HtmlError, String]] =
    val ids = for
      p <- ProjectId.from(projectId)
      c <- CategoryId.from(categoryId)
      o <- OfferId.from(offerId)
    yield (p, c, o)
    ids match
      case Left(e) => IO.pure(Left(fragmentError(StatusCode.BadRequest, Pages.describe(e))))
      case Right((p, c, o)) =>
        repo.setFavorite(p, c, o).map {
          case Right(view) =>
            Right(Pages.projectFragment(view, Pages.activeCategory(view, Some(c))))
          case Left(e) => Left(repoError(e))
        }

  private def deleteOfferLogic(
      projectId: String,
      categoryId: String,
      offerId: String
  ): IO[Either[HtmlError, String]] =
    val ids = for
      p <- ProjectId.from(projectId)
      c <- CategoryId.from(categoryId)
      o <- OfferId.from(offerId)
    yield (p, c, o)
    ids match
      case Left(e) => IO.pure(Left(fragmentError(StatusCode.BadRequest, Pages.describe(e))))
      case Right((p, c, o)) =>
        // File cleanup runs inside deleteOffer under the repo Mutex after the txn (upload finish
        // pattern). Do not call deleteAll here — that would race a concurrent upload and orphan on
        // crash between unlock and deleteAll.
        repo.deleteOffer(p, c, o, files.deleteAll(o)).map {
          case Left(e)     => Left(repoError(e))
          case Right(view) =>
            Right(Pages.projectFragment(view, Pages.activeCategory(view, Some(c))))
        }

  private def repoError(e: RepoError): HtmlError = e match
    case RepoError.ProjectNotFound(_) =>
      fragmentError(StatusCode.NotFound, "Projekt nicht gefunden")
    case RepoError.CategoryNotFound(c) =>
      fragmentError(StatusCode.NotFound, s"Kategorie nicht gefunden: ${c.value}")
    case RepoError.OfferNotFound(o) =>
      fragmentError(StatusCode.NotFound, s"Angebot nicht gefunden: ${o.value}")
    case RepoError.LeistungNotFound(l) =>
      fragmentError(StatusCode.NotFound, s"Leistung nicht gefunden: ${l.value}")
    case RepoError.ItemNotFound(_, item) =>
      fragmentError(StatusCode.UnprocessableEntity, s"Position ${item + 1} gibt es nicht")
    case RepoError.Invalid(e) =>
      fragmentError(StatusCode.UnprocessableEntity, Pages.describe(e))
    case RepoError.Rejected(reason) =>
      fragmentError(StatusCode.Conflict, reason)
    case RepoError.Storage(code) =>
      fragmentError(StatusCode.InternalServerError, s"Datenbankfehler (Code ${code.value})")

  /** Re-renders the tab the edit came from (field `ansicht`); if that category is gone, the first
    * tab, whose URL is pushed so the address bar matches.
    */
  private def editLogic(
      projectId: String,
      form: Map[String, String]
  ): IO[Either[HtmlError, (String, String)]] =
    (ProjectId.from(projectId).left.map(Pages.describe), Api.parseEdit(form)).tupled match
      case Left(text)    => IO.pure(Left(fragmentError(StatusCode.BadRequest, text)))
      case Right((p, e)) =>
        val requested = form.get("ansicht").flatMap(CategoryId.from(_).toOption)
        repo
          .edit(p, e)
          .map(_.map { v =>
            val active = Pages.activeCategory(v, requested)
            val url = active.fold("/")(c => Pages.categoryUrl(v.project.id, c.id))
            (url, Pages.projectFragment(v, active))
          }.left.map(repoError))

  private def newOfferId: IO[Either[DomainError, OfferId]] =
    UUIDGen[IO].randomUUID.map(u => OfferId.from(s"upl-${u.toString.replace("-", "").take(12)}"))

  private def uploadLogic(
      projectId: String,
      categoryId: String,
      form: UploadForm
  ): IO[Either[HtmlError, String]] =
    val checked = for
      datei <- form.datei.toRight(fragmentError(StatusCode.BadRequest, "Keine Datei ausgewählt"))
      bytes = datei.body
      ids <- (for
        p <- ProjectId.from(projectId)
        c <- CategoryId.from(categoryId)
      yield (p, c)).left.map(e => fragmentError(StatusCode.BadRequest, Pages.describe(e)))
      fileType <- OfferFileType
        .detect(bytes)
        .toRight(
          fragmentError(
            StatusCode.UnsupportedMediaType,
            "Bitte ein Angebot als PDF, JPEG oder PNG hochladen"
          )
        )
      _ <- Either.cond(
        bytes.length <= fileType.maxBytes,
        (),
        fragmentError(
          StatusCode.PayloadTooLarge,
          s"Datei zu groß (${fileType.extension.toUpperCase} höchstens ${fileType.maxMegabytes} MB)"
        )
      )
    yield (ids._1, ids._2, fileType, bytes, datei.fileName)
    checked match
      case Left(e)                                  => IO.pure(Left(e))
      case Right((p, c, fileType, bytes, fileName)) =>
        repo.find(p).flatMap {
          case Left(e)                                             => IO.pure(Left(repoError(e)))
          case Right(v) if !v.project.categories.exists(_.id == c) =>
            IO.pure(Left(repoError(RepoError.CategoryNotFound(c))))
          case Right(v) =>
            val known = v.leistungen.filter(_.category == c)
            newOfferId.flatMap {
              case Left(e) =>
                IO.pure(Left(fragmentError(StatusCode.InternalServerError, Pages.describe(e))))
              case Right(freshId) =>
                val name = fileName.getOrElse(s"angebot.${fileType.extension}")
                // Read first (cancel mid-read leaves no file). Keep/obsolete are decided only
                // inside addUpload's lock; the file is staged anonymously, then installed on the
                // kept id under the lock before the txn (prepare), and rolled back if it fails.
                val read = extractor
                  .extract(bytes, fileType, known)
                  .flatMap(extracted =>
                    Clock[IO].realTimeInstant.map { now =>
                      val outcome = extracted.fold(
                        e => UploadOutcome.Failed(Pages.describe(e)),
                        UploadOutcome.Read(_)
                      )
                      Upload(freshId, c, name, fileType, now, outcome)
                    }
                  )
                IO.uncancelable(poll =>
                  poll(read).flatMap { draft =>
                    poll(files.stage(bytes)).flatMap { staged =>
                      // The file is moved into place (old one kept as backup) under addUpload's
                      // lock before the txn; a failed/canceled txn restores the old file.
                      val prepare: UploadStored => IO[FileStep] = {
                        case UploadStored(keep, obsolete) =>
                          files
                            .install(staged, keep, fileType)
                            .map(installed =>
                              FileStep(
                                files.rollback(installed),
                                files.finish(installed) >> obsolete.traverse_(files.deleteAll)
                              )
                            )
                      }
                      val stored = poll(repo.addUpload(p, draft, prepare)).flatMap {
                        case Left(e) =>
                          files.discard(staged).as(Left(e): Either[RepoError, Unit])
                        case Right(_) => IO.pure(Right(()): Either[RepoError, Unit])
                      }
                      stored.guaranteeCase {
                        case Outcome.Succeeded(_)                    => IO.unit
                        case Outcome.Errored(_) | Outcome.Canceled() => files.discard(staged)
                      }
                    }
                  }
                ).flatMap {
                  case Left(e)   => IO.pure(Left(repoError(e)))
                  case Right(()) =>
                    repo
                      .find(p)
                      .map(
                        _.map(v => Pages.projectFragment(v, Pages.activeCategory(v, Some(c)))).left
                          .map(repoError)
                      )
                }
            }
        }

  private val defaultDecodeFailures: DefaultDecodeFailureHandler[IO] =
    DefaultDecodeFailureHandler[IO]

  /** Decode failures of the htmx endpoints (e.g. a body over the limit: 413) as German error
    * fragments for `#fehler`.
    */
  private val htmxDecodeFailures: DefaultDecodeFailureHandler[IO] =
    defaultDecodeFailures.copy[IO](response = (status, _, _) =>
      val text = status match
        case StatusCode.PayloadTooLarge =>
          s"Datei zu groß (höchstens ${OfferFileType.Pdf.maxMegabytes} MB)"
        case other => s"Ungültige Anfrage (Status ${other.code})"
      ValuedEndpointOutput(Endpoints.fragmentError, fragmentError(status, text)))

  /** htmx endpoints get fragments, all others tapir's default responses. */
  private val decodeFailureHandler: DecodeFailureHandler[IO] = new DecodeFailureHandler[IO]:
    def apply(ctx: DecodeFailureContext)(implicit
        monad: MonadError[IO]
    ): IO[Option[ValuedEndpointOutput[?]]] =
      if ctx.endpoint.info.name.exists(Endpoints.htmxNames.contains) then htmxDecodeFailures(ctx)
      else defaultDecodeFailures(ctx)

  val routes: HttpRoutes[IO] = Http4sServerInterpreter[IO](
    Http4sServerOptions.customiseInterceptors[IO].decodeFailureHandler(decodeFailureHandler).options
  ).toRoutes(
    List(
      Endpoints.health.serverLogicSuccess[IO](_ => IO.pure(HealthResponse("ok"))),
      Endpoints.totals.serverLogic[IO](totalsLogic),
      Endpoints.comparisonPdf.serverLogic[IO](comparisonPdfLogic),
      Endpoints.landing.serverLogic[IO](_ => landing),
      Endpoints.categoryPage.serverLogic[IO](categoryPageLogic.tupled),
      Endpoints.markFavorite
        .serverSecurityLogic[Unit, IO](checkRequest)
        .serverLogic(_ => favoriteLogic.tupled),
      Endpoints.deleteOffer
        .serverSecurityLogic[Unit, IO](checkRequest)
        .serverLogic(_ => deleteOfferLogic.tupled),
      Endpoints.edit
        .serverSecurityLogic[Unit, IO](checkRequest)
        .serverLogic(_ => editLogic.tupled),
      Endpoints.uploadOffer
        .serverSecurityLogic[Unit, IO](checkRequest)
        .serverLogic(_ => uploadLogic.tupled)
        .maxRequestBodyLength(Api.MaxRequestBytes),
      staticResourcesGetServerEndpoint[IO]("static")(getClass.getClassLoader, "static")
    )
  )

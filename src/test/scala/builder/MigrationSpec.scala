package builder

import builder.RawOffers.FakeExtractor
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import org.http4s.*
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class MigrationSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers:

  private val extractor = FakeExtractor(Right(RawOffers.dachfenster))

  private def status(app: TestApp, path: String): IO[(Status, String)] =
    app.http
      .run(Request[IO](Method.GET, Uri.unsafeFromString(path)))
      .flatMap(r => r.bodyText.compile.string.map(r.status -> _))

  private def upload(id: String, category: String): String =
    s"""INSERT INTO uploads(project_id, id, category_id, file_name, file_type, created_at,
       |extraction, extraction_version, failure)
       |VALUES ('demo', '$id', '$category', '$id.pdf', 'pdf', now(), NULL, NULL, 'kaputt')""".stripMargin

  /** A database at schema version 1 holding data of the old default categories (`gauben`,
    * `dachfenster`, `sonstiges`, seed offer `daemmung-a`) and of a user-added category.
    */
  private val version1WithOldData: List[String] = List(
    "DELETE FROM schema_migrations WHERE version = 2",
    "INSERT INTO offers VALUES ('demo', 'upl-alt'), ('demo', 'upl-nutzer'), ('demo', 'daemmung-a')",
    upload("upl-alt", "gauben"),
    upload("upl-nutzer", "k-geruest"),
    "INSERT INTO favorites VALUES ('demo', 'gauben', 'upl-alt'), ('demo', 'k-geruest', 'upl-nutzer')",
    """INSERT INTO categories VALUES ('demo', 'sonstiges', 'Diverses', 5, false),
       ('demo', 'k-geruest', 'Gerüstbau', 10, false)""",
    """INSERT INTO leistungen VALUES ('demo', 'l-alt', 'dachfenster', 'Alt', 1, false),
       ('demo', 'l-nutzer', 'k-geruest', 'Gerüst stellen', 2, false)""",
    "INSERT INTO abdeckung VALUES ('demo', 'upl-nutzer', 'l-alt', 'enthalten', 1, 'high', true)",
    "INSERT INTO abdeckung VALUES ('demo', 'upl-nutzer', 'l-nutzer', 'enthalten', 1, 'high', true)"
  )

  private def count(dir: java.nio.file.Path, query: String): IO[Int] =
    TestDb.ints(dir, s"SELECT count(*)::int FROM $query").map(_.sum)

  "migration 2 removes the old default categories with their data and keeps user categories" in {
    TestApp.tempDir.use(dir =>
      for
        _ <- TestApp.resource(dir, extractor).use(_ => IO.unit)
        _ <- TestDb.sql(dir, version1WithOldData*)
        (old, user, versions) <- TestApp
          .resource(dir, extractor)
          .use(app =>
            (
              status(app, "/projekte/demo/kategorien/gauben"),
              status(app, "/projekte/demo/kategorien/k-geruest"),
              TestDb.ints(dir, "SELECT version FROM schema_migrations ORDER BY version")
            ).tupled
          )
        oldRows <- List(
          "offers WHERE id IN ('upl-alt', 'daemmung-a')",
          "uploads WHERE category_id = 'gauben'",
          "favorites WHERE category_id = 'gauben'",
          "categories WHERE id = 'sonstiges'",
          "leistungen WHERE category_id = 'dachfenster'",
          "abdeckung WHERE leistung_id = 'l-alt'"
        ).traverse(count(dir, _))
        userRows <- List(
          "offers WHERE id = 'upl-nutzer'",
          "uploads WHERE category_id = 'k-geruest'",
          "favorites WHERE category_id = 'k-geruest'",
          "categories WHERE id = 'k-geruest'",
          "leistungen WHERE category_id = 'k-geruest'",
          "abdeckung WHERE leistung_id = 'l-nutzer'"
        ).traverse(count(dir, _))
      yield
        versions shouldBe List(1, 2)
        oldRows shouldBe List.fill(6)(0)
        userRows shouldBe List.fill(6)(1)
        old._1 shouldBe Status.NotFound
        user._1 shouldBe Status.Ok
        user._2 should include("Gerüstbau")
        user._2 should include("upl-nutzer.pdf")
        user._2 should include("Gerüst stellen")
    )
  }

  "runtime startup and restart leave an empty database empty" in {
    TestApp.tempDir.use(dir =>
      TestDb
        .config(dir)
        .flatMap(config =>
          List(1, 2)
            .traverse_ { _ =>
              builder.repo.PostgresProjectRepository.resource(config).use { repo =>
                val home = builder.domain.ProjectId.from("default").toOption.get
                val demo = builder.domain.ProjectId.from("demo").toOption.get
                val http = builder.http
                  .Api(repo, home, extractor, builder.storage.FileStore(dir.resolve("angebote")))
                  .routes
                  .orNotFound
                for
                  project <- repo.find(demo)
                  response <- http.run(Request[IO](Method.GET, Uri.unsafeFromString("/")))
                  totals <- http
                    .run(Request[IO](Method.GET, Uri.unsafeFromString("/projects/demo/totals")))
                  rows <- List(
                    "offers",
                    "uploads",
                    "categories",
                    "leistungen",
                    "favorites",
                    "item_leistung",
                    "abdeckung",
                    "ratings",
                    "weights"
                  ).traverse(count(dir, _))
                yield
                  project shouldBe Left(builder.repo.RepoError.ProjectNotFound(demo))
                  response.status shouldBe Status.NotFound
                  totals.status shouldBe Status.NotFound
                  rows shouldBe List.fill(9)(0)
              }
            }
            .as(succeed)
        )
    )
  }

  "runtime default shell serves home without writing rows" in {
    TestApp.tempDir.use(dir =>
      TestDb
        .config(dir)
        .flatMap(config =>
          List(1, 2)
            .traverse_ { _ =>
              val shell = Main.defaultProject.toOption.get
              builder.repo.PostgresProjectRepository.resource(config, List(shell)).use { repo =>
                val http = builder.http
                  .Api(
                    repo,
                    shell.id,
                    extractor,
                    builder.storage.FileStore(dir.resolve("angebote"))
                  )
                  .routes
                  .orNotFound
                for
                  response <- http.run(Request[IO](Method.GET, Uri.unsafeFromString("/")))
                  rows <- List("offers", "uploads", "categories", "leistungen", "favorites")
                    .traverse(count(dir, _))
                yield
                  shell.offers shouldBe Nil
                  shell.categories shouldBe Nil
                  response.status shouldBe Status.Ok
                  rows shouldBe List.fill(5)(0)
              }
            }
            .as(succeed)
        )
    )
  }

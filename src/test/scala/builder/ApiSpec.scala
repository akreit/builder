package builder
import builder.http.Api
import builder.repo.InMemoryProjectRepository
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import io.circe.Json
import io.circe.parser.parse
import org.http4s.*
import org.http4s.implicits.*
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class ApiSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers:
  import Fixtures.*

  private val sample = SampleData.project.value

  /** A fresh app per test, since favorites change repository state. */
  private val app: IO[HttpApp[IO]] =
    InMemoryProjectRepository.of(List(sample)).map(repo => Api(repo, sample.id).routes.orNotFound)

  private def run(req: Request[IO]): IO[(Status, String, Option[String])] =
    app
      .flatMap(_.run(req))
      .flatMap(r =>
        r.bodyText.compile.string
          .map(b =>
            (
              r.status,
              b,
              r.contentType.map(m => s"${m.mediaType.mainType}/${m.mediaType.subType}")
            )
          )
      )

  private def json(path: Uri): IO[(Status, Json)] =
    run(Request[IO](Method.GET, path)).map((s, b, _) => s -> parse(b).getOrElse(Json.Null))

  "GET /health returns 200" in {
    json(uri"/health").asserting(
      _ shouldBe (Status.Ok -> Json.obj("status" -> Json.fromString("ok")))
    )
  }

  "GET /projects/demo/totals returns the running total of the sample project" in {
    json(uri"/projects/demo/totals").asserting { (status, js) =>
      status shouldBe Status.Ok
      val c = js.hcursor
      c.downField("net").as[String] shouldBe Right("21118.50")
      c.downField("missingCategories").as[List[String]] shouldBe Right(
        List("Dachfenster", "Sonstiges")
      )
      c.downField("undecidedCategories").as[List[String]] shouldBe Right(List("Dämmung"))
    }
  }

  "GET /projects/{id}/totals returns 404 for an unknown project" in {
    json(uri"/projects/nope/totals").asserting(_._1 shouldBe Status.NotFound)
  }

  "GET /projects/{id}/totals returns 400 for a malformed id" in {
    json(uri"/projects/%20bad%21/totals").asserting(_._1 shouldBe Status.BadRequest)
  }

  "GET / renders the German Projektübersicht as HTML" in {
    run(Request[IO](Method.GET, uri"/")).asserting { (status, body, ct) =>
      status shouldBe Status.Ok
      ct shouldBe Some("text/html")
      body should include("Projektübersicht: Dach")
      body should include("Gesamtsumme")
      body should include("21.118,50 €")
      body should include("Entscheidung offen")
      body should include("Kategorien ohne Angebot")
      body should include("Als Favorit markieren")
      body should include("/static/htmx.min.js")
    }
  }

  "GET /static/htmx.min.js serves the vendored htmx" in {
    run(Request[IO](Method.GET, uri"/static/htmx.min.js")).asserting { (status, body, _) =>
      status shouldBe Status.Ok
      body should startWith("var htmx=")
    }
  }

  "GET on an unknown path is 404, not the overview page" in {
    run(Request[IO](Method.GET, uri"/gibt-es-nicht")).asserting(_._1 shouldBe Status.NotFound)
  }

  "POST favorite updates the favorite and re-renders the totals with the new sum" in {
    val post = Request[IO](
      Method.POST,
      uri"/ui/projects/demo/categories/daemmung/favorite/daemmung-b"
    )
    for
      a <- app
      r <- a.run(post)
      fragment <- r.bodyText.compile.string
      after <- a
        .run(Request[IO](Method.GET, uri"/projects/demo/totals"))
        .flatMap(_.bodyText.compile.string)
    yield
      r.status shouldBe Status.Ok
      fragment should include("id=\"projekt\"")
      fragment should not include "<html"
      // 21.118,50 € + Holzbau Fiktiv KG 120 m² × 89,90 € = 10.788,00 €
      fragment should include("31.906,50 €")
      fragment should not include "Entscheidung offen"
      parse(after).flatMap(_.hcursor.downField("net").as[String]) shouldBe Right("31906.50")
  }

  "POST favorite with an offer of another category is rejected with 422 and changes nothing" in {
    val post =
      Request[IO](Method.POST, uri"/ui/projects/demo/categories/dachfenster/favorite/gauben-b")
    for
      a <- app
      r <- a.run(post)
      body <- r.bodyText.compile.string
      after <- a
        .run(Request[IO](Method.GET, uri"/projects/demo/totals"))
        .flatMap(_.bodyText.compile.string)
    yield
      r.status shouldBe Status.UnprocessableContent
      body should include("gehört nicht zur Kategorie")
      parse(after).flatMap(_.hcursor.downField("net").as[String]) shouldBe Right("21118.50")
  }

  "POST favorite for an unknown project is 404" in {
    run(Request[IO](Method.POST, uri"/ui/projects/nope/categories/gauben/favorite/gauben-b"))
      .asserting(_._1 shouldBe Status.NotFound)
  }

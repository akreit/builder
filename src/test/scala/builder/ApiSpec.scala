package builder
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import io.circe.Json
import io.circe.parser.parse
import org.http4s.*
import org.http4s.implicits.*
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class ApiSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers:

  /** A fresh app per test, since favorites change repository state. */
  private def withApp[A](f: HttpApp[IO] => IO[A]): IO[A] =
    TestApp.fresh(RawOffers.FakeExtractor(Right(RawOffers.dachfenster))).use(a => f(a.http))

  private def run(req: Request[IO]): IO[(Status, String, Option[String])] =
    withApp(
      _.run(req).flatMap(r =>
        r.bodyText.compile.string.map(b =>
          (r.status, b, r.contentType.map(m => s"${m.mediaType.mainType}/${m.mediaType.subType}"))
        )
      )
    )

  private def favorite(path: Uri, headers: Header.ToRaw*): Request[IO] =
    Request[IO](Method.POST, path).putHeaders(headers*)

  private val hx: Header.ToRaw = "HX-Request" -> "true"

  private def runWithHeaders(req: Request[IO]): IO[(Status, String, Headers)] =
    withApp(_.run(req).flatMap(r => r.bodyText.compile.string.map(b => (r.status, b, r.headers))))

  private def retargeted(h: Headers) =
    (
      h.get(org.typelevel.ci.CIString("HX-Retarget")).map(_.head.value),
      h.get(org.typelevel.ci.CIString("HX-Reswap")).map(_.head.value)
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
    val post = favorite(uri"/ui/projects/demo/categories/daemmung/favorite/daemmung-b", hx)
    withApp { a =>
      for
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
  }

  "POST favorite with an offer of another category is rejected with 422 and changes nothing" in {
    val post = favorite(uri"/ui/projects/demo/categories/dachfenster/favorite/gauben-b", hx)
    withApp { a =>
      for
        r <- a.run(post)
        body <- r.bodyText.compile.string
        after <- a
          .run(Request[IO](Method.GET, uri"/projects/demo/totals"))
          .flatMap(_.bodyText.compile.string)
      yield
        r.status shouldBe Status.UnprocessableContent
        retargeted(r.headers) shouldBe (Some("#fehler"), Some("innerHTML"))
        body should include("gehört nicht zur Kategorie")
        body should include("class=\"fehlermeldung\"")
        body should not include "id=\"fehler\""
        parse(after).flatMap(_.hcursor.downField("net").as[String]) shouldBe Right("21118.50")
    }
  }

  "POST favorite for an unknown project is 404" in {
    runWithHeaders(favorite(uri"/ui/projects/nope/categories/gauben/favorite/gauben-b", hx))
      .asserting { (status, body, h) =>
        status shouldBe Status.NotFound
        retargeted(h) shouldBe (Some("#fehler"), Some("innerHTML"))
        body should include("Projekt nicht gefunden")
      }
  }

  "POST favorite for an unknown category is 404" in {
    runWithHeaders(favorite(uri"/ui/projects/demo/categories/gibtsnicht/favorite/gauben-b", hx))
      .asserting { (status, body, _) =>
        status shouldBe Status.NotFound
        body should include("Kategorie nicht gefunden")
      }
  }

  "POST favorite for an unknown offer is 404" in {
    runWithHeaders(favorite(uri"/ui/projects/demo/categories/gauben/favorite/gibtsnicht", hx))
      .asserting { (status, body, _) =>
        status shouldBe Status.NotFound
        body should include("Angebot nicht gefunden")
      }
  }

  "POST favorite without HX-Request header is 403" in {
    runWithHeaders(favorite(uri"/ui/projects/demo/categories/daemmung/favorite/daemmung-b"))
      .asserting { (status, body, h) =>
        status shouldBe Status.Forbidden
        retargeted(h) shouldBe (Some("#fehler"), Some("innerHTML"))
        body should include("Anfrage abgelehnt")
      }
  }

  "POST favorite from a foreign Origin is 403" in {
    runWithHeaders(
      favorite(
        uri"/ui/projects/demo/categories/daemmung/favorite/daemmung-b",
        hx,
        "Origin" -> "https://evil.example",
        "Host" -> "127.0.0.1:8080"
      )
    ).asserting { (status, body, _) =>
      status shouldBe Status.Forbidden
      body should include("fremde Herkunft")
    }
  }

  "POST favorite from the same Origin is accepted" in {
    runWithHeaders(
      favorite(
        uri"/ui/projects/demo/categories/daemmung/favorite/daemmung-b",
        hx,
        "Origin" -> "http://127.0.0.1:8080",
        "Host" -> "127.0.0.1:8080"
      )
    ).asserting(_._1 shouldBe Status.Ok)
  }

  "GET / has the #fehler slot and the htmx error responseHandling config" in {
    run(Request[IO](Method.GET, uri"/")).asserting { (_, body, _) =>
      body should include("<div id=\"fehler\"")
      body should include("name=\"htmx-config\"")
      body should include("&quot;[45]..&quot;,&quot;swap&quot;:true")
    }
  }

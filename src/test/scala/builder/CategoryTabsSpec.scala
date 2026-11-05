package builder

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import org.http4s.*
import org.http4s.implicits.*
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.ci.CIString

class CategoryTabsSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers:

  private def withApp[A](f: HttpApp[IO] => IO[A]): IO[A] =
    TestApp.fresh(RawOffers.FakeExtractor(Right(RawOffers.dachfenster))).use(a => f(a.http))

  private def get(app: HttpApp[IO], path: Uri): IO[(Status, String, Headers)] =
    app
      .run(Request[IO](Method.GET, path))
      .flatMap(r => r.bodyText.compile.string.map(b => (r.status, b, r.headers)))

  private def page(path: Uri): IO[(Status, String, Headers)] = withApp(get(_, path))

  private val expected =
    List("Dach", "Fenster", "Fassadendämmung", "Elektrik", "Heizung", "Innenausbau")

  /** Tab labels in document order. */
  private def tabLabels(body: String): List[String] =
    "<span class=\"tab-name\">([^<]+)</span>".r.findAllMatchIn(body).map(_.group(1)).toList

  private def sections(body: String): List[String] =
    "<section class=\"kategorie\" data-kategorie=\"([^\"]+)\"".r
      .findAllMatchIn(body)
      .map(_.group(1))
      .toList

  "GET / redirects to the first tab (Dach)" in {
    page(uri"/").asserting { (status, _, h) =>
      status shouldBe Status.SeeOther
      h.get(CIString("Location")).map(_.head.value) shouldBe Some(
        "/projekte/demo/kategorien/dach"
      )
    }
  }

  "every category page lists all tabs in the fixed order" in {
    page(uri"/projekte/demo/kategorien/heizung").asserting { (status, body, _) =>
      status shouldBe Status.Ok
      tabLabels(body) shouldBe expected
    }
  }

  "each tab names its status in text, not only by color" in {
    page(uri"/projekte/demo/kategorien/dach").asserting { (_, body, _) =>
      val statuses = "<span class=\"tab-status\">([^<]+)</span>".r
        .findAllMatchIn(body)
        .map(_.group(1))
        .toList
      statuses shouldBe List(
        "gezählt",
        "kein Angebot",
        "Entscheidung offen",
        "gezählt",
        "gezählt",
        "kein Angebot"
      )
      body should include("aria-label=\"Fassadendämmung, Entscheidung offen\"")
    }
  }

  "the active tab is marked, the others are not" in {
    page(uri"/projekte/demo/kategorien/fassadendaemmung").asserting { (_, body, _) =>
      val active = "<a href=\"([^\"]+)\" class=\"tab [^\"]*aktiv\"[^>]*aria-current=\"page\"".r
        .findAllMatchIn(body)
        .map(_.group(1))
        .toList
      active shouldBe List("/projekte/demo/kategorien/fassadendaemmung")
    }
  }

  "a category page shows only that category's offers and section" in {
    page(uri"/projekte/demo/kategorien/dach").asserting { (_, body, _) =>
      sections(body) shouldBe List("dach")
      body should include("Musterzimmerei Holzwurm GmbH")
      body should include("Zimmerei Beispiel &amp; Söhne")
      body should not include "Holzbau Fiktiv KG"
      body should not include "Heizungsbau Probe"
      body should include("Summe Dach")
    }
  }

  "an unknown category is 404" in {
    page(uri"/projekte/demo/kategorien/gauben").asserting { (status, body, _) =>
      status shouldBe Status.NotFound
      body should include("Kategorie nicht gefunden")
    }
  }

  "an unknown project is 404" in {
    page(uri"/projekte/nope/kategorien/dach").asserting(_._1 shouldBe Status.NotFound)
  }

  "a favorite change re-renders the same tab" in {
    withApp(app =>
      app
        .run(
          Request[IO](
            Method.POST,
            uri"/ui/projects/demo/categories/fassadendaemmung/favorite/fassade-b"
          ).putHeaders("HX-Request" -> "true")
        )
        .flatMap(_.bodyText.compile.string)
    ).asserting(body => sections(body) shouldBe List("fassadendaemmung"))
  }

  "deleting the shown category falls back to the first tab and pushes its URL" in {
    withApp(app =>
      app
        .run(
          Request[IO](Method.POST, uri"/ui/projects/demo/aenderungen")
            .putHeaders("HX-Request" -> "true")
            .withEntity(
              UrlForm(
                "aktion" -> "kategorie-loeschen",
                "kategorie" -> "innenausbau",
                "ansicht" -> "innenausbau"
              )
            )
        )
        .flatMap(r => r.bodyText.compile.string.map(b => (r.status, b, r.headers)))
    ).asserting { (status, body, h) =>
      status shouldBe Status.Ok
      sections(body) shouldBe List("dach")
      tabLabels(body) should not contain "Innenausbau"
      h.get(CIString("HX-Push-Url")).map(_.head.value) shouldBe Some(
        "/projekte/demo/kategorien/dach"
      )
    }
  }

  private def headerText(body: String): Option[String] =
    "<header class=\"kopfzeile\" id=\"kopfzeile\"><a href=\"/\">([^<]+)</a></header>".r
      .findFirstMatchIn(body)
      .map(_.group(1))

  "the start route lands on a page with the fixed header showing the project name" in {
    withApp(app =>
      app
        .run(Request[IO](Method.GET, uri"/"))
        .map(_.headers.get(CIString("Location")).map(_.head.value).getOrElse(""))
        .flatMap(location => get(app, Uri.unsafeFromString(location)))
    ).asserting((status, body, _) =>
      status shouldBe Status.Ok
      headerText(body) shouldBe Some("Dach")
    )
  }

  "every category page has the header above the sidebar" in {
    page(uri"/projekte/demo/kategorien/elektrik").asserting { (_, body, _) =>
      headerText(body) shouldBe Some("Dach")
      body should include("class=\"mit-seitenleiste\"")
      body.indexOf("id=\"kopfzeile\"") should be < body.indexOf("<nav class=\"tabs\"")
    }
  }

  "a page outside a project shows the app name in the header" in {
    page(uri"/projekte/nope/kategorien/dach").asserting((_, body, _) =>
      headerText(body) shouldBe Some("Builder")
    )
  }

  "the header falls back to the app name for a blank project name" in {
    IO(builder.ui.layout.Header.title(Some("  "))).asserting(_ shouldBe "Builder")
  }

package builder

import builder.domain.*
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

  private val chfProject =
    project(Nil, List(offer("x", Category.Other, 100, Currency.CHF)), id = "chf")

  private val badFavoriteProject =
    project(
      Nil,
      List(offer("floor-a", Category.Flooring, 100)),
      Map(Category.Painting -> offerId("floor-a")),
      id = "badfav"
    )

  private val app =
    Api(
      InMemoryProjectRepository.of(List(SampleData.project.value, chfProject, badFavoriteProject))
    ).routes.orNotFound

  private def call(path: Uri): IO[(Status, Json)] =
    app
      .run(Request[IO](Method.GET, path))
      .flatMap(r => r.bodyText.compile.string.map(b => r.status -> parse(b).getOrElse(Json.Null)))

  "GET /health returns 200" in {
    call(uri"/health").asserting(
      _ shouldBe (Status.Ok -> Json.obj("status" -> Json.fromString("ok")))
    )
  }

  "GET /projects/demo/totals returns the running total of the sample project" in {
    call(uri"/projects/demo/totals").asserting { (status, json) =>
      status shouldBe Status.Ok
      val c = json.hcursor
      c.downField("net").as[String] shouldBe Right("36500.00")
      c.downField("missingCategories").as[List[String]] shouldBe Right(List("Plumbing"))
      c.downField("undecidedCategories").as[List[String]] shouldBe Right(List("Electrical"))
    }
  }

  "GET /projects/{id}/totals returns 404 for an unknown project" in {
    call(uri"/projects/nope/totals").asserting(_._1 shouldBe Status.NotFound)
  }

  "GET /projects/{id}/totals returns 400 for a malformed id" in {
    call(uri"/projects/%20bad%21/totals").asserting(_._1 shouldBe Status.BadRequest)
  }

  "currency mismatch is reported as 422" in {
    call(uri"/projects/chf/totals").asserting { (status, json) =>
      status shouldBe Status.UnprocessableContent
      json.hcursor.downField("actual").as[String] shouldBe Right("CHF")
    }
  }

  "unknown favorite is reported as 422 with its own body" in {
    call(uri"/projects/badfav/totals").asserting(
      _ shouldBe (Status.UnprocessableContent -> Json.obj(
        "category" -> Json.fromString("Painting"),
        "offerId" -> Json.fromString("floor-a")
      ))
    )
  }

package builder

import builder.http.Api
import builder.repo.Edit
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParseEditSpec extends AnyFunSuite with Matchers:
  test("kategorie-neu without name asks for a category name") {
    Api.parseEdit(Map("aktion" -> "kategorie-neu")) shouldBe Left(
      "Kategorie: Bitte einen Namen eingeben."
    )
  }

  test("kategorie-neu with blank/whitespace name asks for a category name") {
    Api.parseEdit(Map("aktion" -> "kategorie-neu", "name" -> "   ")) shouldBe Left(
      "Kategorie: Bitte einen Namen eingeben."
    )
  }

  test("kategorie-neu with a name succeeds") {
    Api.parseEdit(Map("aktion" -> "kategorie-neu", "name" -> " Gerüstbau ")) match
      case Right(Edit.AddCategory(n)) => n.value shouldBe "Gerüstbau"
      case other                      => fail(s"unexpected $other")
  }

  test("unrelated missing name field keeps the generic message") {
    Api.parseEdit(Map("aktion" -> "leistung-neu", "kategorie" -> "dach")) shouldBe Left(
      "Feld 'name' fehlt"
    )
  }

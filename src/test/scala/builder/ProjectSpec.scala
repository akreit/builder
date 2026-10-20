package builder

import builder.domain.*
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ProjectSpec extends AnyFunSuite with Matchers with EitherValues:
  import Fixtures.*

  test("category names are trimmed and must not be blank") {
    CategoryName.from("  Dämmung ").value.value shouldBe "Dämmung"
    CategoryName.from("   ").left.value shouldBe DomainError.InvalidCategoryName("   ")
  }

  test("a project keeps its categories in the given order") {
    val cats = List(Other, Windows, Roofing)
    project(Nil, Nil, categories = cats).categories shouldBe cats
  }

  test("duplicate category ids are rejected") {
    projectE(
      Nil,
      Nil,
      categories = List(Windows, category("windows", "Fenster 2"))
    ).left.value shouldBe
      DomainError.DuplicateCategoryId("windows")
  }

  test("an offer referencing a category the project does not have is rejected") {
    projectE(Nil, List(offer("a", Painting, 100)), categories = List(Windows)).left.value shouldBe
      DomainError.UnknownCategory("offer", "painting")
  }

  test("a work package referencing an unknown category is rejected") {
    projectE(List(Plumbing), Nil, categories = List(Windows)).left.value shouldBe
      DomainError.UnknownCategory("work package", "plumbing")
  }

  test("a favorite for an unknown category is rejected") {
    val a = offer("a", Windows, 100)
    projectE(Nil, List(a), Map(Roofing -> a.id), categories = List(Windows)).left.value shouldBe
      DomainError.UnknownCategory("favorite", "roofing")
  }

  test("duplicate offer ids are rejected") {
    projectE(Nil, List(offer("dup", Windows, 100), offer("dup", Roofing, 200))).left.value shouldBe
      DomainError.DuplicateOfferId("dup")
  }

  test("an offer in a different currency than the project is rejected") {
    projectE(
      Nil,
      List(offer("a", Other, 100), offer("x", Other, 100, Currency.CHF))
    ).left.value shouldBe
      DomainError.OfferCurrencyMismatch("x", Currency.EUR, Currency.CHF)
  }

  test("a favorite pointing to an offer of another category is rejected") {
    val a = offer("a", Flooring, 100)
    projectE(Nil, List(a), Map(Painting -> a.id)).left.value shouldBe
      DomainError.FavoriteNotInCategory("painting", "a")
  }

  test("a favorite pointing to a non-existent offer is rejected") {
    projectE(Nil, Nil, Map(Painting -> offerId("ghost"))).left.value shouldBe
      DomainError.FavoriteNotInCategory("painting", "ghost")
  }

  test("withFavorite sets a valid favorite and re-validates") {
    val a = offer("a", Windows, 100)
    val b = offer("b", Windows, 200)
    val p = project(Nil, List(a, b))
    p.withFavorite(Windows.id, b.id).value.favorites shouldBe Map(Windows.id -> b.id)
    p.withFavorite(Roofing.id, b.id).left.value shouldBe
      DomainError.FavoriteNotInCategory("roofing", "b")
  }

  test("the sample project is valid") {
    SampleData.project.value.categories.map(_.name.value) shouldBe
      List("Gauben", "Dämmung", "Spenglerarbeiten", "Balkondämmung", "Dachfenster", "Sonstiges")
  }

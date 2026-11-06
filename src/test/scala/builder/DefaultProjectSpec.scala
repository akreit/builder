package builder

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DefaultProjectSpec extends AnyFunSuite with Matchers:
  test("name comes from BUILDER_PROJECT_NAME and falls back without hardcoding the street") {
    DefaultProject.nameFromEnv(Map(DefaultProject.NameEnv -> "Mathildenstraße 11")) shouldBe
      "Mathildenstraße 11"
    DefaultProject.nameFromEnv(Map.empty) shouldBe DefaultProject.FallbackName
    DefaultProject.nameFromEnv(
      Map(DefaultProject.NameEnv -> "  ")
    ) shouldBe DefaultProject.FallbackName
  }

  test("shell has structural categories in order and no offers") {
    val p = DefaultProject.shell(Map(DefaultProject.NameEnv -> "Mathildenstraße 11")).toOption.get
    p.name shouldBe "Mathildenstraße 11"
    p.categories.map(_.name.value) shouldBe List(
      "Dach",
      "Fenster",
      "Fassadendämmung",
      "Elektrik",
      "Heizung",
      "Innenausbau"
    )
    p.offers shouldBe Nil
    p.id.value shouldBe "default"
  }

package builder

import builder.ui.Pages
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MessagesSpec extends AnyFunSuite with Matchers:
  test("error fragment is dismissible without a form submit") {
    val html = Pages.errorFragment("Kategorie: Bitte einen Namen eingeben.")
    html should include("""class="fehlermeldung""")
    html should include("""type="button""")
    html should include("""aria-label="Meldung schließen""")
    html should include("fehlermeldung-schliessen")
    html should include("closest('#fehler').replaceChildren()")
    html should not include ("hx-post")
  }

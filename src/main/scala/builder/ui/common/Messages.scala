package builder.ui.common
import scalatags.Text.TypedTag
import scalatags.Text.all.*

/** Error and message snippets. */
object Messages:
  /** Error fragment swapped into the `#fehler` slot of the layout. Dismissible via the close
    * button; clearing `#fehler` does not touch project state. A later error response replaces the
    * slot again.
    */
  def errorFragment(text: String): TypedTag[String] =
    div(cls := "fehlermeldung")(
      span(cls := "fehlermeldung-text")(text),
      button(
        tpe := "button",
        cls := "fehlermeldung-schliessen",
        attr("aria-label") := "Meldung schließen",
        onclick := "this.closest('#fehler').replaceChildren()"
      )("×")
    )

package builder.ui.common
import scalatags.Text.TypedTag
import scalatags.Text.all.*

/** Error and message snippets. */
object Messages:
  /** Error fragment swapped into the `#fehler` slot of the layout. */
  def errorFragment(text: String): TypedTag[String] = p(cls := "fehlermeldung")(text)

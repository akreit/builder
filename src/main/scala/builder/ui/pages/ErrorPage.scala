package builder.ui.pages

import builder.ui.layout.Layout.*
import scalatags.Text.all.*

/** A full page for an HTML error response (e.g. 404), with the header of the app. */
object ErrorPage:
  /** Minimal HTML error page with the given message. */
  def errorPage(text: String): String =
    layout(text, None, false, div(cls := "fehler")(text))

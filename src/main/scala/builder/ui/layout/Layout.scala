package builder.ui.layout
import builder.ui.layout.Styles.*
import scalatags.Text.all.*
import scalatags.Text.tags2

/** The HTML document frame shared by all pages. */
object Layout:
  /** A full page: fixed header (project or app name), then the content; `withSidebar` leaves room
    * for the category sidebar on the left.
    */
  private[ui] def layout(
      pageTitle: String,
      projectName: Option[String],
      withSidebar: Boolean,
      content: Frag*
  ): String =
    doctype("html")(
      html(lang := "de")(
        head(
          meta(charset := "utf-8"),
          meta(name := "viewport", attr("content") := "width=device-width, initial-scale=1"),
          tags2.title(pageTitle),
          tags2.style(raw(styles)),
          meta(name := "htmx-config", attr("content") := htmxConfig),
          script(src := "/static/htmx.min.js")
        ),
        body(if withSidebar then cls := "mit-seitenleiste" else frag())(
          Header.bar(projectName),
          div(cls := "inhalt")(div(id := "fehler", attr("role") := "alert"), frag(content*))
        )
      )
    ).render

  /** htmx 2 defaults to not swapping 4xx/5xx; swap them (still flagged as errors) so HX-Retarget
    * works.
    */
  private[ui] val htmxConfig: String =
    """{"responseHandling":[{"code":"204","swap":false},{"code":"[23]..","swap":true},{"code":"[45]..","swap":true,"error":true}]}"""

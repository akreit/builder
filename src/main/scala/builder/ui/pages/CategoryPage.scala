package builder.ui.pages

import builder.domain.*
import builder.repo.*
import builder.ui.layout.Layout.*
import builder.ui.project.ProjectFragment.*
import scalatags.Text.all.*

/** The page of one category tab. */
object CategoryPage:
  /** The page of one category tab (or, without categories, the bare project page). */
  def categoryPage(view: ProjectView, active: Option[Category]): String =
    val project = view.project
    layout(
      active.fold(s"Projekt – ${project.name}")(c => s"${c.name.value} – ${project.name}"),
      h1(s"Projekt: ${project.name}"),
      p(cls := "hinweis")(
        s"Währung ${project.currency.code} · Beispieldaten (fiktive Firmen und Beträge) · hochgeladene Angebote werden gespeichert"
      ),
      projectFragment(view, active)
    )

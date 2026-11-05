package builder.ui.navigation

import builder.domain.*
import builder.repo.*
import builder.totals.*
import builder.ui.category.Status.*
import scalatags.Text.all.*
import scalatags.Text.tags2

/** Category tabs: URLs, active tab and the tab bar. */
object Tabs:
  /** URL of the page (tab) of one category. */
  def categoryUrl(project: ProjectId, category: CategoryId): String =
    s"/projekte/${project.value}/kategorien/${category.value}"

  /** The requested category if the project has it, else the first one (e.g. after deleting it). */
  def activeCategory(view: ProjectView, requested: Option[CategoryId]): Option[Category] =
    val cats = view.project.categories
    requested.flatMap(r => cats.find(_.id == r)).orElse(cats.headOption)

  /** One tab per category, in the project's order; the active one is marked. */
  private[ui] def tabs(project: Project, totals: ProjectTotals, active: Option[Category]): Frag =
    tags2.nav(cls := "tabs", attr("aria-label") := "Kategorien")(
      ul(
        totals.categories.map(ct =>
          val isActive = active.exists(_.id == ct.category.id)
          li(
            a(
              href := categoryUrl(project.id, ct.category.id),
              cls := s"tab ${statusClass(ct.status)}${if isActive then " aktiv" else ""}",
              attr("data-kategorie") := ct.category.id.value,
              if isActive then attr("aria-current") := "page" else frag()
            )(ct.category.name.value)
          )
        )
      )
    )

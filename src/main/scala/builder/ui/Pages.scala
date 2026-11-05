package builder.ui

import builder.domain.*
import builder.extraction.*
import builder.repo.*
import builder.ui.common.Messages
import builder.ui.navigation.Tabs
import builder.ui.pages.CategoryPage
import builder.ui.project.ProjectFragment
import builder.ui.text.Describe

/** The UI's entry points for the backend: data in, rendered HTML (or German text) out. All markup
  * lives in the sub-packages of `builder.ui`; nothing outside `ui` builds HTML.
  */
object Pages:
  /** URL of the page (tab) of one category. */
  def categoryUrl(project: ProjectId, category: CategoryId): String =
    Tabs.categoryUrl(project, category)

  /** The requested category if the project has it, else the first one (e.g. after deleting it). */
  def activeCategory(view: ProjectView, requested: Option[CategoryId]): Option[Category] =
    Tabs.activeCategory(view, requested)

  /** Full page of one category tab (or, without categories, the bare project page). */
  def categoryPage(view: ProjectView, active: Option[Category]): String =
    CategoryPage.categoryPage(view, active)

  /** The htmx-swappable `#projekt` fragment for the given tab. */
  def projectFragment(view: ProjectView, active: Option[Category]): String =
    ProjectFragment.projectFragment(view, active).render

  /** Error fragment swapped into the layout's `#fehler` slot. */
  def errorFragment(text: String): String = Messages.errorFragment(text).render

  /** A standalone error message (body of an HTML error response). */
  def message(text: String): String = Messages.message(text).render

  def describe(e: DomainError): String = Describe.describe(e)
  def describe(i: Issue): String = Describe.describe(i)
  def describe(e: ExtractionError): String = Describe.describe(e)

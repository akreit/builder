package builder.ui.common

import builder.domain.*
import scalatags.Text.all.*

/** htmx attributes and the shared edit form of the UI. */
object Htmx:
  private[ui] val hxPost = attr("hx-post")
  private[ui] val hxTarget = attr("hx-target")
  private[ui] val hxSwap = attr("hx-swap")
  private[ui] val hxEncoding = attr("hx-encoding")
  private[ui] val hxIndicator = attr("hx-indicator")
  private[ui] val hxTrigger = attr("hx-trigger")
  private[ui] val hxConfirm = attr("hx-confirm")

  /** A form posting one [[Edit]]; `auto` forms submit on every change of a select. */
  private[ui] def editForm(project: Project, fields: List[(String, String)], auto: Boolean)(
      content: Frag*
  ): Frag =
    form(
      cls := "aendern",
      hxPost := s"/ui/projects/${project.id.value}/aenderungen",
      hxTarget := "#projekt",
      hxSwap := "outerHTML",
      if auto then hxTrigger := "change" else frag()
    )(fields.map((k, v) => input(tpe := "hidden", name := k, value := v)), frag(content*))

package builder.ui.project

import builder.domain.*
import builder.repo.*
import builder.ui.common.Htmx.*
import scalatags.Text.all.*
import scalatags.Text.tags2

/** Weights of the ranking criteria. */
object WeightsForm:
  private[ui] def weightsForm(view: ProjectView): Frag =
    tags2.section(cls := "gewichtung", id := "gewichtung")(
      h2("Gewichtung der Rangliste"),
      editForm(view.project, List("aktion" -> "gewichtung"), auto = false)(
        Criterion.values.toList.map(c =>
          label(
            s"${Criterion.label(c)} ",
            input(
              tpe := "number",
              name := s"gewicht-${Criterion.code(c)}",
              attr("min") := "0",
              attr("max") := "100",
              value := view.weights.of(c).toString
            ),
            " % "
          )
        ),
        button(tpe := "submit")("Gewichtung speichern")
      )
    )

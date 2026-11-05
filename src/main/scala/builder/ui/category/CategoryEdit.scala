package builder.ui.category

import builder.domain.*
import builder.ui.common.Htmx.*
import scalatags.Text.all.*

/** Rename and delete controls of a category. */
object CategoryEdit:
  private[ui] def categoryEdit(project: Project, category: Category): Frag =
    tag("details")(cls := "kategorie-bearbeiten")(
      tag("summary")(cls := "hinweis")("Kategorie bearbeiten"),
      editForm(
        project,
        List("aktion" -> "kategorie-umbenennen", "kategorie" -> category.id.value),
        auto = false
      )(
        input(
          name := "name",
          value := category.name.value,
          attr("aria-label") := "Neuer Kategoriename"
        ),
        button(tpe := "submit")("Umbenennen")
      ),
      editForm(
        project,
        List("aktion" -> "kategorie-loeschen", "kategorie" -> category.id.value),
        auto = false
      )(
        button(tpe := "submit", hxConfirm := s"Kategorie „${category.name.value}“ löschen?")(
          "Löschen"
        )
      )
    )

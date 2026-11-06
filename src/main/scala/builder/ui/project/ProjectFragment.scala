package builder.ui.project

import builder.domain.*
import builder.repo.*
import builder.totals.*
import builder.ui.Format
import builder.ui.category.CategorySection.*
import builder.ui.common.Htmx.*
import builder.ui.navigation.Tabs.*
import builder.ui.project.WeightsForm.*
import scalatags.Text.TypedTag
import scalatags.Text.all.*
import scalatags.Text.tags2

/** The htmx-swappable part of a category page. */
object ProjectFragment:
  /** The part of the page htmx replaces after a change: tabs, the active category and totals. The
    * active category rides along with every edit (`hx-vals`), so the response re-renders the same
    * tab.
    */
  def projectFragment(view: ProjectView, active: Option[Category]): TypedTag[String] =
    val totals = Totals.compute(view.project)
    val current = active.flatMap(c => totals.categories.find(_.category.id == c.id))
    div(
      id := "projekt",
      current.fold(frag())(ct => attr("hx-vals") := s"""{"ansicht":"${ct.category.id.value}"}""")
    )(
      if view.warnings.isEmpty then frag()
      else
        div(cls := "warnung", id := "datenwarnungen")(
          strong("Gespeicherte Daten teilweise nicht verwendbar:"),
          ul(view.warnings.map(li(_)))
        )
      ,
      tabs(view.project, totals, active),
      current.fold[Frag](p(cls := "hinweis")("Noch keine Kategorie angelegt."))(ct =>
        frag(categorySection(view, ct), categoryTotal(ct))
      ),
      weightsForm(view),
      editForm(view.project, List("aktion" -> "kategorie-neu"), auto = false)(
        input(
          name := "name",
          placeholder := "Neue Kategorie",
          attr("aria-label") := "Neue Kategorie"
        ),
        button(tpe := "submit")("Kategorie hinzufügen")
      ),
      // Aggregate scope summary: categories + totals only — no line-item dump (assignment stays in
      // Leistungsvergleich). Visually mirrors the comparison table layout.
      tags2.section(cls := "leistungsverzeichnis", id := "leistungsverzeichnis")(
        h2("Leistungsverzeichnis"),
        table(
          thead(
            tr(
              th("Kategorie"),
              th("Leistungen"),
              th("Angebote"),
              th(cls := "betrag")("Netto"),
              th("Status")
            )
          ),
          tbody(
            totals.categories.map { ct =>
              val leistungCount =
                view.leistungen.count(_.category == ct.category.id)
              val offerCount = view.project.offers.count(_.category == ct.category.id)
              val (net, statusLabel) = ct.status match
                case CategoryStatus.Counted(_, _, net, _, _) =>
                  (Format.money(net), "gezählt")
                case CategoryStatus.NeedsDecision(_) =>
                  ("–", "Entscheidung offen")
                case CategoryStatus.NoOffer =>
                  ("–", "kein Angebot")
              tr(
                attr("data-verzeichnis-kategorie") := ct.category.id.value
              )(
                td(ct.category.name.value),
                td(leistungCount.toString),
                td(offerCount.toString),
                td(cls := "betrag")(net),
                td(statusLabel)
              )
            }
          )
        )
      ),
      tags2.section(cls := "summe", id := "summe")(
        h2("Gesamtsumme Projekt"),
        table(
          tr(th("Gesamtsumme netto"), td(cls := "betrag")(Format.money(totals.net))),
          tr(th("Gesamtsumme brutto"), td(cls := "betrag")(Format.money(totals.gross)))
        ),
        if totals.isComplete then p("Alle Kategorien sind entschieden.")
        else
          p(cls := "hinweis")(
            "Vorläufige Summe: Kategorien mit offener Entscheidung oder ohne Angebot sind nicht enthalten."
          )
      )
    )

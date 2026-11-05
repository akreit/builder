package builder.ui.category

import builder.domain.*
import builder.ranking.*
import builder.repo.*
import builder.ui.Format
import builder.ui.common.Htmx.*
import scalatags.Text.all.*
import scalatags.Text.tags2

/** The ranking (Rangliste) of a category. */
object RankingSection:
  private[ui] def points(d: BigDecimal): String = Format.decimal(d)
  private[ui] def criterionCell(cs: CriterionScore): Frag = cs.result match
    case CriterionResult.Scored(p, detail) =>
      td(cls := "betrag")(strong(s"${points(p)} Pkt."), div(cls := "seite")(detail))
    case CriterionResult.NotAvailable(reason) =>
      td(cls := "betrag pruefen")(strong(reason))
  private[ui] def ratingForm(project: Project, e: RankedOffer): Frag =
    e.firm.fold[Frag](span(cls := "hinweis")("Firma nicht erkannt"))(_ => ratingFields(project, e))
  private[ui] def ratingFields(project: Project, e: RankedOffer): Frag =
    editForm(project, List("aktion" -> "bewertung", "firma" -> e.contractor), auto = false)(
      select(name := "sterne", attr("aria-label") := s"Sterne für ${e.contractor}")(
        option(value := "", if e.rating.isEmpty then selected := "selected" else frag())(
          "nicht bewertet"
        ),
        (1 to 5).map(n =>
          option(
            value := n.toString,
            if e.rating.exists(_.stars.value == n) then selected := "selected" else frag()
          )(
            "★" * n
          )
        )
      ),
      input(
        name := "notiz",
        value := e.rating.fold("")(_.note),
        placeholder := "Notiz zu Referenzen",
        attr("maxlength") := Rating.MaxNote.toString,
        attr("aria-label") := s"Notiz zu ${e.contractor}"
      ),
      button(tpe := "submit")("Speichern")
    )
  private[ui] def rankingRow(project: Project, rank: Option[Int], e: RankedOffer): Frag =
    tr(attr("data-firma") := e.contractor)(
      td(rank.fold("–")(r => s"$r.")),
      td(
        strong(e.contractor),
        div(cls := "seite")(
          e.price.comparable.fold("kein Vergleichspreis")(m =>
            s"Vergleichspreis ${Format.money(m)}"
          )
        ),
        if e.comparable then frag() else div(cls := "pruefen")(Ranking.NotComparable),
        if e.price.missing.isEmpty then frag()
        else
          div(cls := "pruefen")(
            s"Fehlt gegenüber anderen: ${e.price.missing.map(_.name.value).mkString(", ")}"
          )
        ,
        e.rating
          .filter(_.note.nonEmpty)
          .fold(frag())(r => div(cls := "hinweis")(s"Notiz: ${r.note}"))
      ),
      td(cls := "betrag gesamt")(
        e.total.fold[Frag](span(cls := "hinweis")("nicht bewertbar"))(t =>
          strong(s"${points(t)} Pkt.")
        )
      ),
      e.criteria.map(criterionCell),
      td(ratingForm(project, e))
    )
  private[ui] def rankingSection(view: ProjectView, category: Category): Frag =
    val project = view.project
    val r =
      Ranking.build(Matrix.build(view, category.id), view.ratings, view.weights, project.currency)
    if r.ranked.isEmpty && r.unranked.isEmpty then frag()
    else
      tags2.section(cls := "ranking", attr("data-kategorie") := category.id.value)(
        h3("Rangliste"),
        p(cls := "hinweis")(
          if r.common.isEmpty then
            "Preisvergleich nicht möglich: keine Leistung ist in allen Angeboten mit Preis enthalten."
          else
            s"Preis verglichen auf gemeinsamen Leistungen: ${r.common.map(_.name.value).mkString(", ")}. " +
              "Günstigstes Angebot = 100 Punkte, die anderen anteilig. Referenzen: 20 Punkte je Stern."
        ),
        table(cls := "rangliste")(
          thead(
            tr(
              th("Rang"),
              th("Firma"),
              th(cls := "betrag")("Gesamt"),
              Criterion.values.toList.map(c =>
                th(cls := "betrag")(s"${Criterion.label(c)} (${view.weights.of(c)} %)")
              ),
              th("Referenzen bewerten")
            )
          ),
          tbody(
            r.ranked.zipWithIndex.map((e, i) => rankingRow(project, Some(i + 1), e)),
            r.unranked.map(e => rankingRow(project, None, e))
          )
        ),
        if r.unranked.isEmpty then frag()
        else
          p(cls := "hinweis")(
            "Ohne Rang: nicht alle gewichteten Kriterien sind bewertet oder das Angebot hat Abweichungen."
          )
      )

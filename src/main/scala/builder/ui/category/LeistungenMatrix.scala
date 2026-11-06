package builder.ui.category

import builder.domain.*
import builder.extraction.*
import builder.repo.*
import builder.ui.Format
import builder.ui.common.Htmx.*
import builder.ui.offer.OfferDetail
import scalatags.Text.all.*
import scalatags.Text.tags2

/** The Leistungen matrix of a category and its edit controls. Cells that map line items show those
  * positions in place as a native `<details>` block (collapsed by default), reusing
  * [[OfferDetail.collapsiblePositions]].
  */
object LeistungenMatrix:
  private[ui] def coverageLabel(c: Coverage): String = c match
    case Coverage.Enthalten      => "enthalten"
    case Coverage.NichtEnthalten => "nicht enthalten"
    case Coverage.Ausgeschlossen => "ausgeschlossen"
    case Coverage.Unklar         => "unklar"
  private[ui] def coverageClass(c: Coverage): String = c match
    case Coverage.Enthalten      => "st-enthalten"
    case Coverage.NichtEnthalten => "st-nicht"
    case Coverage.Ausgeschlossen => "st-ausgeschlossen"
    case Coverage.Unklar         => "st-unklar"
  private[ui] def sourceText(s: CoverageSource): String = s match
    case CoverageSource.Read(page, conf) =>
      page.fold("ohne Seite")(p => s"S. ${p.value}") + (conf match
        case Confidence.High   => ""
        case Confidence.Medium => " · mittel sicher"
        case Confidence.Low    => " · unsicher")
    case CoverageSource.FromItems => "aus Positionen"
    case CoverageSource.Manual    => "manuell"
    case CoverageSource.Unknown   => "nicht erkannt"
  private[ui] def columnTitle(c: MatrixColumn): String =
    c.raw.contractor.value.filter(_.trim.nonEmpty).getOrElse(c.upload.upload.fileName)

  /** German summary for the cell's position toggle ("1 Position" / "N Positionen"). */
  private[ui] def positionsSummaryLabel(count: Int): String =
    if count == 1 then "1 Position" else s"$count Positionen"

  /** Line items of `col` that this cell maps, in index order, for in-cell display. */
  private[ui] def cellLineItems(col: MatrixColumn, c: Cell): List[(Int, RawLineItem)] =
    c.items.flatMap(i => col.raw.lineItems.lift(i).map(li => i -> li))

  /** Collapsible block of the cell's mapped positions; empty when the cell has none. Collapsed by
    * default (`<details>` without `open`). Missing Menge/Preis show only the per-position flags —
    * no extra consistency warning.
    */
  private[ui] def cellPositions(col: MatrixColumn, c: Cell, cur: Currency): Frag =
    val items = cellLineItems(col, c)
    items match
      case Nil => frag()
      case all =>
        tag("details")(cls := "zellen-positionen")(
          tag("summary")(positionsSummaryLabel(all.size)),
          OfferDetail.collapsiblePositions(all, cur)
        )

  private[ui] def matrixCell(project: Project, col: MatrixColumn, l: Leistung, c: Cell): Frag =
    val chosen = c.source match
      case CoverageSource.Manual => Coverage.code(c.coverage)
      case CoverageSource.Read(_, _) | CoverageSource.FromItems | CoverageSource.Unknown => "auto"
    td(
      cls := s"zelle ${coverageClass(c.coverage)}${if c.flagged then " pruefen" else ""}",
      attr("data-zelle") := s"${l.id.value}|${col.upload.upload.id.value}",
      attr("data-status") := Coverage.code(c.coverage)
    )(
      div(
        strong(coverageLabel(c.coverage)),
        if c.flagged then span(cls := "flag niedrig")("prüfen") else frag()
      ),
      div(cls := "betrag")(c.price.fold(if c.items.nonEmpty then "Preis ?" else "–")(Format.money)),
      div(cls := "seite")(
        sourceText(c.source) + (if c.items.isEmpty then ""
                                else s" · Pos. ${c.items.map(_ + 1).mkString(", ")}")
      ),
      cellPositions(col, c, project.currency),
      editForm(
        project,
        List(
          "aktion" -> "status-setzen",
          "angebot" -> col.upload.upload.id.value,
          "leistung" -> l.id.value
        ),
        auto = true
      )(
        select(
          name := "status",
          attr("aria-label") := s"Status ${l.name.value} für ${columnTitle(col)}"
        )(
          (("auto" -> "automatisch") :: Coverage.values.toList.map(c =>
            Coverage.code(c) -> coverageLabel(c)
          ))
            .map((v, t) =>
              option(value := v, if v == chosen then selected := "selected" else frag())(t)
            )
        )
      )
    )
  private[ui] def leistungHeader(project: Project, l: Leistung, others: List[Leistung]): Frag =
    th(cls := "leistung", attr("data-leistung") := l.id.value)(
      l.name.value,
      tag("details")(
        tag("summary")(cls := "hinweis")("bearbeiten"),
        editForm(
          project,
          List("aktion" -> "leistung-umbenennen", "leistung" -> l.id.value),
          auto = false
        )(
          input(name := "name", value := l.name.value, attr("aria-label") := "Neuer Name"),
          button(tpe := "submit")("Umbenennen")
        ),
        if others.isEmpty then frag()
        else
          editForm(
            project,
            List("aktion" -> "leistung-zusammenfuehren", "leistung" -> l.id.value),
            auto = false
          )(
            select(name := "ziel", attr("aria-label") := "Zusammenführen mit")(
              others.map(o => option(value := o.id.value)(o.name.value))
            ),
            button(tpe := "submit")("Zusammenführen")
          )
        ,
        editForm(
          project,
          List("aktion" -> "leistung-loeschen", "leistung" -> l.id.value),
          auto = false
        )(
          button(tpe := "submit", hxConfirm := s"Leistung „${l.name.value}“ löschen?")("Löschen")
        )
      )
    )
  private[ui] def leistungenSection(view: ProjectView, category: Category): Frag =
    val project = view.project
    val m = Matrix.build(view, category.id)
    val leistungen = m.rows.map(_.leistung)
    tags2.section(cls := "leistungen", attr("data-kategorie") := category.id.value)(
      h3("Leistungsvergleich"),
      a(href := s"/projekte/${project.id.value}/leistungsvergleich.pdf", cls := "button")(
        "PDF exportieren"
      ),
      if m.rows.isEmpty && m.columns.isEmpty then
        p(cls := "hinweis")(
          "Noch keine Leistungen – sie werden beim ersten hochgeladenen Angebot erkannt."
        )
      else
        table(cls := "matrix")(
          thead(tr(th("Leistung"), m.columns.map(c => th(columnTitle(c))))),
          tbody(
            m.rows.map(r =>
              tr(
                leistungHeader(project, r.leistung, leistungen.filterNot(_.id == r.leistung.id)),
                m.columns.zip(r.cells).map((col, c) => matrixCell(project, col, r.leistung, c))
              )
            ),
            if m.unassigned.isEmpty then frag()
            else
              tr(cls := "nicht-zugeordnet")(
                th("Nicht zugeordnet"),
                m.columns.map(col =>
                  td(
                    m.unassigned
                      .filter(_.offer == col.upload.upload.id)
                      .map(u => div(cls := "pruefen")(s"Pos. ${u.item + 1}: ${u.description}"))
                  )
                )
              )
          ),
          tfoot(
            tr(cls := "summen")(
              th("Summe Zellen + nicht zugeordnet"),
              m.totals.map(t =>
                td(
                  cls := (if t.mismatch then "betrag pruefen" else "betrag"),
                  attr("data-abweichung") := t.mismatch.toString
                )(
                  t.sum.map(Format.money).getOrElse("nicht lesbar"),
                  div(cls := "hinweis")(
                    s"Netto laut Angebot: ${t.net.map(Format.money).getOrElse("nicht lesbar")}"
                  ),
                  if t.mismatch then div("Weicht vom Netto ab – Zuordnung prüfen") else frag()
                )
              )
            )
          )
        )
      ,
      m.columns.zip(m.dropped).collect {
        case (col, n) if n > 0 =>
          p(cls := "pruefen verworfen")(
            s"${columnTitle(col)}: $n weitere Leistungsvorschläge verworfen " +
              s"(höchstens ${LeistungPlan.MaxNew} neue je Angebot)."
          )
      },
      if m.columns.isEmpty then frag()
      else
        tag("details")(cls := "zuordnung")(
          tag("summary")("Positionen zuordnen"),
          m.columns.map { col =>
            val assigned = Matrix.effective(view.assignments)
            div(
              h4(columnTitle(col)),
              table(
                col.raw.lineItems.zipWithIndex.map((li, i) =>
                  val current = assigned.get((col.upload.upload.id, i)).flatten
                  tr(
                    td(s"${i + 1}. ${li.description.value.getOrElse("")}"),
                    td(cls := "betrag")(li.total.value.getOrElse("")),
                    td(
                      editForm(
                        project,
                        List(
                          "aktion" -> "position-zuordnen",
                          "angebot" -> col.upload.upload.id.value,
                          "position" -> (i + 1).toString
                        ),
                        auto = true
                      )(
                        select(
                          name := "ziel",
                          attr("aria-label") := s"Leistung für Position ${i + 1}"
                        )(
                          option(
                            value := "",
                            if current.isEmpty then selected := "selected" else frag()
                          )(
                            "– keine –"
                          ),
                          leistungen.map(l =>
                            option(
                              value := l.id.value,
                              if current.contains(l.id) then selected := "selected" else frag()
                            )(
                              l.name.value
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          }
        )
      ,
      editForm(
        project,
        List("aktion" -> "leistung-neu", "kategorie" -> category.id.value),
        auto = false
      )(
        input(
          name := "name",
          placeholder := "Neue Leistung",
          attr("aria-label") := "Neue Leistung"
        ),
        button(tpe := "submit")("Leistung hinzufügen")
      )
    )

package builder.ui.category

import builder.domain.*
import builder.extraction.*
import builder.repo.*
import builder.ui.common.Fields.*
import builder.ui.common.Htmx.*
import builder.ui.text.Describe.*
import scalatags.Text.all.*
import scalatags.Text.tags2

/** Uploaded offers (extraction results) and the upload form. */
object Uploads:
  private[ui] def extractedOffer(raw: RawOffer, cur: Currency): Frag =
    frag(
      table(cls := "felder")(
        raw.labelled.map((l, f) =>
          tr(unsureClass(f))(th(l), td(shown(l, f, cur)), td(pageRef(f)), td(flag(f)))
        )
      ),
      if raw.lineItems.isEmpty then p(cls := "hinweis")("Keine Einzelpositionen (Pauschalangebot).")
      else
        table(
          thead(
            tr(
              th("Pos."),
              raw.lineItems.headOption.toList.flatMap(_.labelled).map((l, _) => th(l)),
              th("Seite")
            )
          ),
          tbody(
            raw.lineItems.zipWithIndex.map((li, i) =>
              tr(
                td(i + 1),
                li.labelled.map((l, f) =>
                  td(unsureClass(f), if amountLabels(l) then cls := "betrag" else ())(
                    shown(l, f, cur),
                    " ",
                    flag(f)
                  )
                ),
                td(pageRef(li.description))
              )
            )
          )
        )
      ,
      textList("Hinweise", raw.notes),
      textList("Nicht enthalten", raw.exclusions)
    )
  private[ui] def textList(title: String, items: List[Field]): Frag =
    items.filter(_.text.nonEmpty) match
      case Nil            => frag()
      case all @ (_ :: _) =>
        frag(
          h4(title),
          ul(all.map(n => li(unsureClass(n))(n.text.getOrElse(""), " ", pageRef(n), " ", flag(n))))
        )
  private[ui] def uploadBlock(u: UploadView, cur: Currency): Frag =
    val heading = h3(s"Hochgeladen: ${u.upload.fileName}")
    div(cls := "upload", attr("data-upload") := u.upload.id.value)(
      u.status match
        case UploadStatus.ExtractionFailed(m) =>
          frag(heading, p(cls := "abweichung")(s"Auslesen fehlgeschlagen: $m"))
        case UploadStatus.Rejected(raw, issues) =>
          frag(
            heading,
            div(cls := "abweichung")(
              strong("Abweichungen gefunden – Angebot wird nicht in die Summe übernommen:"),
              ul(issues.toList.map(i => li(describe(i))))
            ),
            lowConfidenceWarning(raw),
            extractedOffer(raw, cur)
          )
        case UploadStatus.Accepted(raw, _) =>
          frag(
            heading,
            p("Ausgelesen und geprüft: Positionen, Netto, MwSt. und Brutto stimmen überein."),
            lowConfidenceWarning(raw),
            tags2.details(attr("open") := "open")(
              tags2.summary("Ausgelesene Angebotsdaten mit Seitenangaben"),
              extractedOffer(raw, cur)
            )
          )
    )
  private[ui] def lowConfidenceWarning(raw: RawOffer): Frag =
    raw.lowConfidenceFields match
      case Nil    => frag()
      case fields =>
        div(cls := "warnung")(
          strong(s"⚠ ${fields.size} Felder mit geringer Sicherheit – bitte im Original prüfen:"),
          ul(fields.map(li(_)))
        )
  private[ui] def uploadForm(project: Project, category: Category): Frag =
    val indicator = s"lade-${category.id.value}"
    form(
      cls := "hochladen",
      hxPost := s"/ui/projects/${project.id.value}/categories/${category.id.value}/angebote",
      hxEncoding := "multipart/form-data",
      hxTarget := "#projekt",
      hxSwap := "outerHTML",
      hxIndicator := s"#$indicator"
    )(
      label(s"Angebot für ${category.name.value} hochladen (PDF, JPEG oder PNG): "),
      input(
        tpe := "file",
        name := "datei",
        required,
        attr("accept") := ".pdf,.jpg,.jpeg,.png,application/pdf,image/jpeg,image/png"
      ),
      button(tpe := "submit")("Hochladen und auslesen"),
      span(id := indicator, cls := "htmx-indicator hinweis")(
        " Wird ausgelesen … (bis zu einer Minute)"
      )
    )

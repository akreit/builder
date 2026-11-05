package builder.ui.category

import builder.domain.*
import builder.extraction.*
import builder.repo.*
import builder.ui.Format
import builder.ui.common.Htmx.*
import builder.ui.offer.OfferDetail
import builder.ui.text.Describe.*
import scalatags.Text.all.*
import scalatags.Text.tags2

/** Uploaded offers (extraction results) and the upload form. */
object Uploads:
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
            OfferDetail.render(raw, cur)
          )
        case UploadStatus.Accepted(raw, offer) =>
          frag(
            heading,
            p(acceptanceMessage(offer)),
            lowConfidenceWarning(raw),
            tags2.details(attr("open") := "open")(
              tags2.summary("Ausgelesene Angebotsdaten mit Seitenangaben"),
              OfferDetail.render(raw, cur)
            )
          )
    )

  /** German status line for an accepted offer. Position sum is mentioned only when every position
    * has a price (`lineSumCheck` is defined); missing prices are normal — no extra warning.
    */
  private def acceptanceMessage(offer: Offer): String =
    val amountsOk = "Netto, MwSt. und Brutto stimmen überein"
    offer.lineSumCheck match
      case Some(LineSumCheck.Consistent) if offer.lineItems.nonEmpty =>
        s"Ausgelesen und geprüft: Positionen, $amountsOk."
      case Some(LineSumCheck.Consistent) | None =>
        s"Ausgelesen und geprüft: $amountsOk."
      case Some(LineSumCheck.Mismatch(sum, net)) =>
        // Accepted offers cannot carry a mismatch (Offer.from rejects it); keep exhaustiveness.
        s"Ausgelesen und geprüft: $amountsOk. Positionssumme ${Format.money(sum)} ≠ Netto ${Format.money(net)}."

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

package builder.ui.common

import builder.domain.*
import builder.extraction.*
import builder.ui.Format
import scalatags.Text.all.*

import java.time.LocalDate
import scala.util.Try

/** Display helpers for extracted fields (value, page, confidence). */
object Fields:
  private[ui] def pageRef(f: Field): Frag =
    span(cls := "seite")(f.pageNumber.fold("–")(n => s"S. ${n.value}"))
  private[ui] def flag(f: Field): Frag = f.confidence match
    case Confidence.Low =>
      span(cls := "flag niedrig", title := "Geringe Sicherheit – bitte im Original prüfen")(
        "⚠ prüfen"
      )
    case Confidence.Medium => span(cls := "flag mittel")("mittel")
    case Confidence.High   => frag()
  private[ui] val amountLabels = Set("Netto", "MwSt.", "Brutto", "Einzelpreis", "Gesamtpreis")
  private[ui] val dateLabels = Set("Angebotsdatum", "Gültig bis")

  /** German display of a raw extracted value; unparseable values are shown as read. */
  private[ui] def shown(label: String, f: Field, cur: Currency): String =
    f.text.fold("—")(t =>
      if amountLabels(label) then
        Try(BigDecimal(t)).toOption
          .flatMap(Money.fromDecimal(_, cur).toOption)
          .fold(t)(Format.money)
      else if dateLabels(label) then Try(LocalDate.parse(t)).toOption.fold(t)(Format.date)
      else if label == "MwSt.-Satz" then s"$t %"
      else t
    )
  private[ui] def unsureClass(f: Field): Modifier =
    if f.isLowConfidence then cls := "unsicher" else frag()

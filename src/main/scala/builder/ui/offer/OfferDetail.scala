package builder.ui.offer

import builder.domain.*
import builder.extraction.*
import builder.ui.Format
import scalatags.Text.all.*

import java.time.LocalDate
import scala.util.Try

/** Angebots-Detailsicht: ausgelesene Felder und die Positionsliste (je Position als natives
  * `<details>`/`<summary>`, standardmäßig eingeklappt). Kein JS-/htmx-Zustand. Dieselbe
  * Positionsliste wird in Matrix-Zellen des Leistungsvergleichs wiederverwendet.
  */
object OfferDetail:
  private def pageRef(f: Field): Frag =
    span(cls := "seite")(f.pageNumber.fold("–")(n => s"S. ${n.value}"))

  private def flag(f: Field): Frag = f.confidence match
    case Confidence.Low =>
      span(cls := "flag niedrig", title := "Geringe Sicherheit – bitte im Original prüfen")(
        "⚠ prüfen"
      )
    case Confidence.Medium => span(cls := "flag mittel")("mittel")
    case Confidence.High   => frag()

  private val amountLabels = Set("Netto", "MwSt.", "Brutto", "Einzelpreis", "Gesamtpreis")
  private val dateLabels = Set("Angebotsdatum", "Gültig bis")

  /** German display of a raw extracted value; unparseable values are shown as read. */
  private def shown(label: String, f: Field, cur: Currency): String =
    f.text.fold("—")(t =>
      if amountLabels(label) then
        Try(BigDecimal(t)).toOption
          .flatMap(Money.fromDecimal(_, cur).toOption)
          .fold(t)(Format.money)
      else if dateLabels(label) then Try(LocalDate.parse(t)).toOption.fold(t)(Format.date)
      else if label == "MwSt.-Satz" then s"$t %"
      else t
    )

  private def unsureClass(f: Field): Modifier =
    if f.isLowConfidence then cls := "unsicher" else frag()

  /** Flag shown next to an empty Menge/Preis field. */
  private def missingFlag(label: String, f: Field): Frag =
    if f.text.nonEmpty then frag()
    else
      label match
        case "Menge"                       => frag(" ", span(cls := "fehlend")("Menge fehlt"))
        case "Einzelpreis" | "Gesamtpreis" =>
          frag(" ", span(cls := "fehlend")("Preis fehlt"))
        case "Beschreibung" | "Einheit" | "Maße" | "Hersteller" | "Modell" => frag()

  /** Summary line for a collapsed position: number, description, short amounts, missing flags. */
  private def positionSummary(i: Int, li: RawLineItem, cur: Currency): Frag =
    val desc = li.description.text.fold("ohne Beschreibung")(identity)
    val qty = li.quantity.text.fold("")(q => li.unit.text.fold(q)(u => s"$q $u"))
    val tot = li.total.text.fold("")(_ => shown("Gesamtpreis", li.total, cur))
    val bits = List(s"Pos. ${i + 1}", desc, qty, tot).filter(_.nonEmpty)
    frag(
      bits.mkString(" · "),
      " ",
      if li.quantity.text.isEmpty then span(cls := "fehlend")("Menge fehlt") else frag(),
      " ",
      if li.unitPrice.text.isEmpty && li.total.text.isEmpty then
        span(cls := "fehlend")("Preis fehlt")
      else if li.unitPrice.text.isEmpty then span(cls := "fehlend")("Einzelpreis fehlt")
      else if li.total.text.isEmpty then span(cls := "fehlend")("Gesamtpreis fehlt")
      else frag()
    )

  /** One position: summary visible, full fields (quantity, sizes, manufacturer/model, prices, page
    * refs) inside `<details>`, collapsed by default.
    */
  private def positionRow(i: Int, li: RawLineItem, cur: Currency): Frag =
    tag("details")(cls := "position")(
      tag("summary")(positionSummary(i, li, cur)),
      table(cls := "felder")(
        li.labelled.map((l, f) =>
          tr(unsureClass(f))(
            th(l),
            td(
              unsureClass(f),
              if amountLabels(l) then cls := "betrag" else (),
              shown(l, f, cur),
              " ",
              flag(f),
              missingFlag(l, f)
            ),
            td(pageRef(f))
          )
        )
      )
    )

  private def textList(title: String, items: List[Field]): Frag =
    items.filter(_.text.nonEmpty) match
      case Nil            => frag()
      case all @ (_ :: _) =>
        frag(
          h4(title),
          ul(
            all.map(n =>
              li(unsureClass(n))(n.text.fold("")(identity), " ", pageRef(n), " ", flag(n))
            )
          )
        )

  /** Collapsible position list (0-based indices as shown). Empty input yields an empty fragment.
    * Each position is a native `<details>` without `open`, so collapsed by default. Shared by the
    * offer detail view and Leistungsvergleich matrix cells.
    */
  def collapsiblePositions(items: List[(Int, RawLineItem)], cur: Currency): Frag =
    items match
      case Nil            => frag()
      case all @ (_ :: _) =>
        div(cls := "positionen")(all.map((i, li) => positionRow(i, li, cur)))

  /** Full offer detail block: header fields, collapsible positions, notes and exclusions. */
  def render(raw: RawOffer, cur: Currency): Frag =
    frag(
      table(cls := "felder")(
        raw.labelled.map((l, f) =>
          tr(unsureClass(f))(th(l), td(shown(l, f, cur)), td(pageRef(f)), td(flag(f)))
        )
      ),
      if raw.lineItems.isEmpty then p(cls := "hinweis")("Keine Einzelpositionen (Pauschalangebot).")
      else collapsiblePositions(raw.lineItems.zipWithIndex.map((li, i) => i -> li), cur),
      textList("Hinweise", raw.notes),
      textList("Nicht enthalten", raw.exclusions)
    )

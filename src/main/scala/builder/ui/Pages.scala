package builder.ui

import builder.domain.*
import builder.totals.*
import scalatags.Text.TypedTag
import scalatags.Text.all.*
import scalatags.Text.tags2

/** Server-rendered German UI (Scalatags + htmx). */
object Pages:
  private val hxPost = attr("hx-post")
  private val hxTarget = attr("hx-target")
  private val hxSwap = attr("hx-swap")

  private val styles =
    """body{font-family:system-ui,sans-serif;max-width:60rem;margin:2rem auto;padding:0 1rem;color:#222}
      |h1{margin-bottom:.25rem}section.kategorie{border:1px solid #ddd;border-radius:6px;padding:.5rem 1rem;margin:1rem 0}
      |table{border-collapse:collapse;width:100%}th,td{text-align:left;padding:.3rem .5rem;border-bottom:1px solid #eee}
      |td.betrag,th.betrag{text-align:right;font-variant-numeric:tabular-nums}
      |.status{display:inline-block;padding:.1rem .5rem;border-radius:4px;font-size:.85rem;margin-left:.5rem}
      |.gezaehlt{background:#d9f2d9}.offen{background:#ffe9b3}.fehlt{background:#f5d0d0}
      |.favorit{font-weight:bold}.summe{background:#f4f6fa;padding:1rem;border-radius:6px}
      |.hinweis{color:#666;font-size:.9rem}""".stripMargin

  private def statusBadge(s: CategoryStatus): Frag = s match
    case CategoryStatus.Counted(_, _, _, _, CountedReason.Favorite) =>
      span(cls := "status gezaehlt")("gezählt (Favorit)")
    case CategoryStatus.Counted(_, _, _, _, CountedReason.OnlyOffer) =>
      span(cls := "status gezaehlt")("gezählt (einziges Angebot)")
    case CategoryStatus.NeedsDecision(_) => span(cls := "status offen")("Entscheidung offen")
    case CategoryStatus.NoOffer          => span(cls := "status fehlt")("kein Angebot")

  private def favoriteUrl(project: Project, category: Category, offer: Offer): String =
    s"/ui/projects/${project.id.value}/categories/${category.id.value}/favorite/${offer.id.value}"

  private def offerRow(project: Project, category: Category, offer: Offer): Frag =
    val isFavorite = project.favorites.lift(category.id).contains(offer.id)
    tr(if isFavorite then cls := "favorit" else ())(
      td(offer.contractor.value.name),
      td(Format.date(offer.date.value)),
      td(cls := "betrag")(Format.money(offer.net.value)),
      td(cls := "betrag")(Format.money(offer.gross.value)),
      td(
        if isFavorite then span("★ Favorit")
        else
          button(
            hxPost := favoriteUrl(project, category, offer),
            hxTarget := "#projekt",
            hxSwap := "outerHTML"
          )("Als Favorit markieren")
      )
    )

  private def categorySection(project: Project, ct: CategoryTotal): Frag =
    val offers = project.offers.filter(_.category == ct.category.id)
    tags2.section(cls := "kategorie", attr("data-kategorie") := ct.category.id.value)(
      h2(ct.category.name.value, statusBadge(ct.status)),
      if offers.isEmpty then p(cls := "hinweis")("Noch kein Angebot eingegangen.")
      else
        table(
          thead(
            tr(
              th("Firma"),
              th("Datum"),
              th(cls := "betrag")("Netto"),
              th(cls := "betrag")("Brutto"),
              th("")
            )
          ),
          tbody(offers.map(o => offerRow(project, ct.category, o)))
        )
    )

  /** The part of the page htmx replaces after a favorite change: categories and totals. */
  def projectFragment(project: Project): TypedTag[String] =
    val totals = Totals.compute(project)
    div(id := "projekt")(
      totals.categories.map(ct => categorySection(project, ct)),
      tags2.section(cls := "summe", id := "summe")(
        h2("Gesamtsumme"),
        table(
          tr(th("Gesamtsumme netto"), td(cls := "betrag")(Format.money(totals.net))),
          tr(th("Gesamtsumme brutto"), td(cls := "betrag")(Format.money(totals.gross)))
        ),
        if totals.isComplete then p("Alle Kategorien sind entschieden.")
        else
          p(cls := "hinweis")(
            "Vorläufige Summe: Kategorien mit offener Entscheidung oder ohne Angebot sind nicht enthalten."
          )
      ),
      tags2.section(id := "ohne-angebot")(
        h2("Kategorien ohne Angebot"),
        if totals.missingCategories.isEmpty then
          p("Keine – für jede Kategorie liegt ein Angebot vor.")
        else ul(totals.missingCategories.map(c => li(c.name.value)))
      )
    )

  private def layout(pageTitle: String, content: Frag*): String =
    doctype("html")(
      html(lang := "de")(
        head(
          meta(charset := "utf-8"),
          meta(name := "viewport", attr("content") := "width=device-width, initial-scale=1"),
          tags2.title(pageTitle),
          tags2.style(raw(styles)),
          script(src := "/static/htmx.min.js")
        ),
        body(content*)
      )
    ).render

  /** Projektübersicht. */
  def overview(project: Project): String =
    layout(
      s"Projektübersicht – ${project.name}",
      h1(s"Projektübersicht: ${project.name}"),
      p(cls := "hinweis")(
        s"Währung ${project.currency.code} · Beispieldaten (fiktive Firmen und Beträge)"
      ),
      projectFragment(project)
    )

  def message(text: String): TypedTag[String] = div(id := "fehler", cls := "fehler")(text)

  /** German description of a validation error, for the UI. */
  def describe(e: DomainError): String = e match
    case DomainError.InvalidId(kind, raw)          => s"Ungültige Kennung ($kind): $raw"
    case DomainError.InvalidPageNumber(n)          => s"Ungültige Seitenzahl: $n"
    case DomainError.InvalidVatRate(pct)           => s"Ungültiger MwSt.-Satz: $pct %"
    case DomainError.InvalidQuantity(q)            => s"Ungültige Menge: $q"
    case DomainError.InvalidDate(y, m, d)          => s"Ungültiges Datum: $d.$m.$y"
    case DomainError.InvalidMoney(err)             => s"Ungültiger Betrag: $err"
    case DomainError.NegativeAmount(field, amount) =>
      s"Negativer Betrag ($field): ${Format.money(amount)}"
    case DomainError.GrossMismatch(net, vat, gross) =>
      s"Brutto ${Format.money(gross)} ≠ Netto ${Format.money(net)} + MwSt. ${Format.money(vat)}"
    case DomainError.VatMismatch(expected, actual) =>
      s"MwSt. ${Format.money(actual)} statt erwartet ${Format.money(expected)}"
    case DomainError.LineItemsMismatch(sum, net) =>
      s"Summe der Positionen ${Format.money(sum)} ≠ Netto ${Format.money(net)}"
    case DomainError.DuplicateOfferId(o)             => s"Angebot doppelt vorhanden: $o"
    case DomainError.InvalidCategoryName(raw)        => s"Ungültiger Kategoriename: '$raw'"
    case DomainError.DuplicateCategoryId(c)          => s"Kategorie doppelt vorhanden: $c"
    case DomainError.UnknownCategory(by, c)          => s"Unbekannte Kategorie '$c' ($by)"
    case DomainError.LineItemTotalMismatch(exp, act) =>
      s"Positionssumme ${Format.money(act)} statt erwartet ${Format.money(exp)}"
    case DomainError.OfferCurrencyMismatch(o, exp, act) =>
      s"Angebot $o in ${act.code} statt ${exp.code}"
    case DomainError.FavoriteNotInCategory(c, o) =>
      s"Angebot $o gehört nicht zur Kategorie $c"

package builder.ui

import builder.domain.*
import builder.extraction.*
import builder.repo.*
import builder.totals.*
import scalatags.Text.TypedTag
import scalatags.Text.all.*
import scalatags.Text.tags2

import java.time.LocalDate
import scala.util.Try

/** Server-rendered German UI (Scalatags + htmx). */
object Pages:
  private val hxPost = attr("hx-post")
  private val hxTarget = attr("hx-target")
  private val hxSwap = attr("hx-swap")
  private val hxEncoding = attr("hx-encoding")
  private val hxIndicator = attr("hx-indicator")

  private val styles =
    """body{font-family:system-ui,sans-serif;max-width:60rem;margin:2rem auto;padding:0 1rem;color:#222}
      |h1{margin-bottom:.25rem}section.kategorie{border:1px solid #ddd;border-radius:6px;padding:.5rem 1rem;margin:1rem 0}
      |table{border-collapse:collapse;width:100%}th,td{text-align:left;padding:.3rem .5rem;border-bottom:1px solid #eee}
      |td.betrag,th.betrag{text-align:right;font-variant-numeric:tabular-nums}
      |.status{display:inline-block;padding:.1rem .5rem;border-radius:4px;font-size:.85rem;margin-left:.5rem}
      |.gezaehlt{background:#d9f2d9}.offen{background:#ffe9b3}.fehlt{background:#f5d0d0}
      |.favorit{font-weight:bold}.summe{background:#f4f6fa;padding:1rem;border-radius:6px}
      |.hinweis{color:#666;font-size:.9rem}.fehlermeldung{background:#f5d0d0;padding:.5rem 1rem;border-radius:6px}
      |.upload{border-top:1px dashed #ccc;margin-top:.75rem;padding-top:.5rem}.upload h3{font-size:1rem;margin:.25rem 0}
      |.unsicher{background:#fff3cd}.flag{font-size:.8rem;padding:0 .3rem;border-radius:3px;white-space:nowrap}
      |.flag.niedrig{background:#f0ad4e;color:#000}.flag.mittel{background:#eee}.seite{color:#666;font-size:.8rem;white-space:nowrap}
      |.warnung{background:#fff3cd;padding:.5rem 1rem;border-radius:6px}.abweichung{background:#f5d0d0;padding:.5rem 1rem;border-radius:6px}
      |table.felder th{width:10rem}form.hochladen{margin:.5rem 0}.htmx-indicator{display:none}.htmx-request .htmx-indicator,.htmx-request.htmx-indicator{display:inline}
      |table.matrix td,table.matrix th{vertical-align:top;border:1px solid #e3e3e3}table.matrix th.leistung{min-width:9rem}
      |.st-enthalten{background:#e3f4e3}.st-nicht{background:#f3f3f3}.st-ausgeschlossen{background:#f8dede}.st-unklar{background:#fff3cd}
      |td.pruefen,div.pruefen{outline:2px solid #f0ad4e;outline-offset:-2px}form.aendern{display:inline-block;margin:.15rem .25rem .15rem 0}
      |table.matrix select{font-size:.8rem}details summary{cursor:pointer}section.leistungen{margin-top:1rem}""".stripMargin

  private def statusBadge(s: CategoryStatus): Frag = s match
    case CategoryStatus.Counted(_, _, _, _, CountedReason.Favorite) =>
      span(cls := "status gezaehlt")("gezählt (Favorit)")
    case CategoryStatus.Counted(_, _, _, _, CountedReason.OnlyOffer) =>
      span(cls := "status gezaehlt")("gezählt (einziges Angebot)")
    case CategoryStatus.NeedsDecision(_) => span(cls := "status offen")("Entscheidung offen")
    case CategoryStatus.NoOffer          => span(cls := "status fehlt")("kein Angebot")

  private def favoriteUrl(project: Project, category: Category, offer: Offer): String =
    s"/ui/projects/${project.id.value}/categories/${category.id.value}/favorite/${offer.id.value}"

  private def offerRow(
      project: Project,
      category: Category,
      offer: Offer,
      uploads: List[UploadView]
  ): Frag =
    val isFavorite = project.favorites.lift(category.id).contains(offer.id)
    val unsure = uploads.collectFirst {
      case UploadView(u, UploadStatus.Accepted(raw, _)) if u.id == offer.id =>
        raw.lowConfidenceFields
    }
    tr(if isFavorite then cls := "favorit" else ())(
      td(
        offer.contractor.value.name,
        unsure.map(fs =>
          if fs.isEmpty then span(cls := "flag mittel")("aus Upload")
          else span(cls := "flag niedrig")(s"⚠ ${fs.size} unsichere Felder")
        )
      ),
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

  private def extractedOffer(raw: RawOffer, cur: Currency): Frag =
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

  private def textList(title: String, items: List[Field]): Frag =
    items.filter(_.text.nonEmpty) match
      case Nil            => frag()
      case all @ (_ :: _) =>
        frag(
          h4(title),
          ul(all.map(n => li(unsureClass(n))(n.text.getOrElse(""), " ", pageRef(n), " ", flag(n))))
        )

  private def uploadBlock(u: UploadView, cur: Currency): Frag =
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

  private def lowConfidenceWarning(raw: RawOffer): Frag =
    raw.lowConfidenceFields match
      case Nil    => frag()
      case fields =>
        div(cls := "warnung")(
          strong(s"⚠ ${fields.size} Felder mit geringer Sicherheit – bitte im Original prüfen:"),
          ul(fields.map(li(_)))
        )

  private def uploadForm(project: Project, category: Category): Frag =
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

  private val hxTrigger = attr("hx-trigger")
  private val hxConfirm = attr("hx-confirm")

  private def coverageLabel(c: Coverage): String = c match
    case Coverage.Enthalten      => "enthalten"
    case Coverage.NichtEnthalten => "nicht enthalten"
    case Coverage.Ausgeschlossen => "ausgeschlossen"
    case Coverage.Unklar         => "unklar"

  private def coverageClass(c: Coverage): String = c match
    case Coverage.Enthalten      => "st-enthalten"
    case Coverage.NichtEnthalten => "st-nicht"
    case Coverage.Ausgeschlossen => "st-ausgeschlossen"
    case Coverage.Unklar         => "st-unklar"

  private def sourceText(s: CoverageSource): String = s match
    case CoverageSource.Read(page, conf) =>
      page.fold("ohne Seite")(p => s"S. ${p.value}") + (conf match
        case Confidence.High   => ""
        case Confidence.Medium => " · mittel sicher"
        case Confidence.Low    => " · unsicher")
    case CoverageSource.FromItems => "aus Positionen"
    case CoverageSource.Manual    => "manuell"
    case CoverageSource.Unknown   => "nicht erkannt"

  /** A form posting one [[Edit]]; `auto` forms submit on every change of a select. */
  private def editForm(project: Project, fields: List[(String, String)], auto: Boolean)(
      content: Frag*
  ): Frag =
    form(
      cls := "aendern",
      hxPost := s"/ui/projects/${project.id.value}/aenderungen",
      hxTarget := "#projekt",
      hxSwap := "outerHTML",
      if auto then hxTrigger := "change" else frag()
    )(fields.map((k, v) => input(tpe := "hidden", name := k, value := v)), frag(content*))

  private def columnTitle(c: MatrixColumn): String =
    c.raw.contractor.value.filter(_.trim.nonEmpty).getOrElse(c.upload.upload.fileName)

  private def matrixCell(project: Project, col: MatrixColumn, l: Leistung, c: Cell): Frag =
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

  private def leistungHeader(project: Project, l: Leistung, others: List[Leistung]): Frag =
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

  private def leistungenSection(view: ProjectView, category: Category): Frag =
    val project = view.project
    val m = Matrix.build(view, category.id)
    val leistungen = m.rows.map(_.leistung)
    tags2.section(cls := "leistungen", attr("data-kategorie") := category.id.value)(
      h3("Leistungsvergleich"),
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
          )
        )
      ,
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

  private def categoryEdit(project: Project, category: Category): Frag =
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

  private def categorySection(view: ProjectView, ct: CategoryTotal): Frag =
    val project = view.project
    val uploads = view.uploads.filter(_.upload.category == ct.category.id)
    val offers = project.offers.filter(_.category == ct.category.id)
    tags2.section(cls := "kategorie", attr("data-kategorie") := ct.category.id.value)(
      h2(ct.category.name.value, statusBadge(ct.status)),
      categoryEdit(project, ct.category),
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
          tbody(offers.map(o => offerRow(project, ct.category, o, uploads)))
        )
      ,
      uploads.map(uploadBlock(_, project.currency)),
      leistungenSection(view, ct.category),
      uploadForm(project, ct.category)
    )

  /** The part of the page htmx replaces after a favorite change: categories and totals. */
  def projectFragment(view: ProjectView): TypedTag[String] =
    val totals = Totals.compute(view.project)
    div(id := "projekt")(
      if view.warnings.isEmpty then frag()
      else
        div(cls := "warnung", id := "datenwarnungen")(
          strong("Gespeicherte Daten teilweise nicht verwendbar:"),
          ul(view.warnings.map(li(_)))
        )
      ,
      totals.categories.map(ct => categorySection(view, ct)),
      editForm(view.project, List("aktion" -> "kategorie-neu"), auto = false)(
        input(
          name := "name",
          placeholder := "Neue Kategorie",
          attr("aria-label") := "Neue Kategorie"
        ),
        button(tpe := "submit")("Kategorie hinzufügen")
      ),
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
          meta(name := "htmx-config", attr("content") := htmxConfig),
          script(src := "/static/htmx.min.js")
        ),
        body(div(id := "fehler", attr("role") := "alert"), frag(content*))
      )
    ).render

  /** Projektübersicht. */
  def overview(view: ProjectView): String =
    val project = view.project
    layout(
      s"Projektübersicht – ${project.name}",
      h1(s"Projektübersicht: ${project.name}"),
      p(cls := "hinweis")(
        s"Währung ${project.currency.code} · Beispieldaten (fiktive Firmen und Beträge) · hochgeladene Angebote werden gespeichert"
      ),
      projectFragment(view)
    )

  /** htmx 2 defaults to not swapping 4xx/5xx; swap them (still flagged as errors) so HX-Retarget
    * works.
    */
  private val htmxConfig: String =
    """{"responseHandling":[{"code":"204","swap":false},{"code":"[23]..","swap":true},{"code":"[45]..","swap":true,"error":true}]}"""

  /** Error fragment swapped into the `#fehler` slot of the layout. */
  def errorFragment(text: String): TypedTag[String] = p(cls := "fehlermeldung")(text)

  def message(text: String): TypedTag[String] = div(cls := "fehler")(text)

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
    case DomainError.InvalidLeistungName(raw)        => s"Ungültiger Name der Leistung: '$raw'"
    case DomainError.DuplicateCategoryId(c)          => s"Kategorie doppelt vorhanden: $c"
    case DomainError.UnknownCategory(by, c)          => s"Unbekannte Kategorie '$c' ($by)"
    case DomainError.LineItemTotalMismatch(exp, act) =>
      s"Positionssumme ${Format.money(act)} statt erwartet ${Format.money(exp)}"
    case DomainError.OfferCurrencyMismatch(o, exp, act) =>
      s"Angebot $o in ${act.code} statt ${exp.code}"
    case DomainError.FavoriteNotInCategory(c, o) =>
      s"Angebot $o gehört nicht zur Kategorie $c"

  def describe(i: Issue): String = i match
    case Issue.Missing(field)         => s"$field fehlt"
    case Issue.Unreadable(field, raw) => s"$field nicht lesbar: '$raw'"
    case Issue.Invalid(field, e)      => s"$field: ${describe(e)}"

  def describe(e: ExtractionError): String = e match
    case ExtractionError.MissingApiKey   => "Kein ANTHROPIC_API_KEY gesetzt"
    case ExtractionError.Transport(kind) => s"Verbindung zur Claude-API fehlgeschlagen ($kind)"
    case ExtractionError.HttpStatus(code, errorType) =>
      s"Claude-API antwortete mit Status $code${errorType.fold("")(t => s" ($t)")}"
    case ExtractionError.TimedOut              => "Claude-API hat nicht rechtzeitig geantwortet"
    case ExtractionError.UnexpectedResponse(m) => s"Unerwartete Antwort der Claude-API: $m"

package builder.ui.category

import builder.domain.*
import builder.repo.*
import builder.totals.*
import builder.ui.Format
import builder.ui.category.CategoryEdit.*
import builder.ui.category.LeistungenMatrix.*
import builder.ui.category.OfferTable.*
import builder.ui.category.RankingSection.*
import builder.ui.category.Status.*
import builder.ui.category.Uploads.*
import scalatags.Text.all.*
import scalatags.Text.tags2

/** The section of one category and its running total. */
object CategorySection:
  /** Full category block: status, offers, uploads, matrix and ranking. */
  private[ui] def categorySection(view: ProjectView, ct: CategoryTotal): Frag =
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
      uploads.map(uploadBlock(_, project.currency, project, ct.category)),
      leistungenSection(view, ct.category),
      rankingSection(view, ct.category),
      uploadForm(project, ct.category)
    )

  /** Running-total strip for one category. */
  private[ui] def categoryTotal(ct: CategoryTotal): Frag =
    tags2.section(cls := "summe", id := "kategorie-summe")(
      h3(s"Summe ${ct.category.name.value}"),
      ct.status match
        case CategoryStatus.Counted(_, c, net, gross, _) =>
          table(
            tr(th("Gezählt"), td(c.name)),
            tr(th("Netto"), td(cls := "betrag")(Format.money(net))),
            tr(th("Brutto"), td(cls := "betrag")(Format.money(gross)))
          )
        case CategoryStatus.NeedsDecision(_) =>
          p(cls := "hinweis")("Noch nicht in der Summe: bitte einen Favoriten wählen.")
        case CategoryStatus.NoOffer =>
          p(cls := "hinweis")("Noch kein Angebot – nicht in der Summe.")
    )

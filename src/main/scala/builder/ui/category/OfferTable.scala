package builder.ui.category

import builder.domain.*
import builder.repo.*
import builder.ui.Format
import builder.ui.common.Htmx.*
import scalatags.Text.all.*

/** Offer rows of a category, with the favorite button. */
object OfferTable:
  private[ui] def favoriteUrl(project: Project, category: Category, offer: Offer): String =
    s"/ui/projects/${project.id.value}/categories/${category.id.value}/favorite/${offer.id.value}"
  private[ui] def offerRow(
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

package builder.ui.category

import builder.domain.*
import builder.domain.OfferLabel
import builder.repo.*
import builder.ui.Format
import builder.ui.common.Htmx.*
import scalatags.Text.all.*

/** Offer rows of a category, with favorite and delete actions. */
object OfferTable:
  /** htmx POST URL that marks this offer as favorite. */
  private[ui] def favoriteUrl(project: Project, category: Category, offer: Offer): String =
    s"/ui/projects/${project.id.value}/categories/${category.id.value}/favorite/${offer.id.value}"

  /** htmx POST URL that deletes this offer. */
  private[ui] def deleteUrl(project: Project, category: Category, offerId: OfferId): String =
    s"/ui/projects/${project.id.value}/categories/${category.id.value}/angebote/${offerId.value}/loeschen"

  /** One offer row: contractor, amounts, favorite and delete controls. */
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
        OfferLabel.display(offer),
        offer.recipient.fold(span(cls := "flag mittel")("Adressat unbekannt"))(_ => frag()),
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
        ,
        " ",
        button(
          hxPost := deleteUrl(project, category, offer.id),
          hxTarget := "#projekt",
          hxSwap := "outerHTML",
          hxConfirm := s"Angebot „${OfferLabel.display(offer)}“ wirklich löschen?"
        )("Löschen")
      )
    )

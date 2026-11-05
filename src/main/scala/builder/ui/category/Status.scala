package builder.ui.category
import builder.totals.*
import scalatags.Text.all.*

/** Status badge and status class of a category. */
object Status:
  private[ui] def statusBadge(s: CategoryStatus): Frag = s match
    case CategoryStatus.Counted(_, _, _, _, CountedReason.Favorite) =>
      span(cls := "status gezaehlt")("gezählt (Favorit)")
    case CategoryStatus.Counted(_, _, _, _, CountedReason.OnlyOffer) =>
      span(cls := "status gezaehlt")("gezählt (einziges Angebot)")
    case CategoryStatus.NeedsDecision(_) => span(cls := "status offen")("Entscheidung offen")
    case CategoryStatus.NoOffer          => span(cls := "status fehlt")("kein Angebot")
  private[ui] def statusClass(s: CategoryStatus): String = s match
    case CategoryStatus.Counted(_, _, _, _, _) => "gezaehlt"
    case CategoryStatus.NeedsDecision(_)       => "offen"
    case CategoryStatus.NoOffer                => "fehlt"

  /** Short German status of a category, shown in its sidebar tab next to the status color. */
  private[ui] def statusText(s: CategoryStatus): String = s match
    case CategoryStatus.Counted(_, _, _, _, _) => "gezählt"
    case CategoryStatus.NeedsDecision(_)       => "Entscheidung offen"
    case CategoryStatus.NoOffer                => "kein Angebot"

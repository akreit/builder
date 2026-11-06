package builder.totals

import builder.domain.*

/** Why an offer was chosen as the counted one for a category. */
enum CountedReason:
  case Favorite, OnlyOffer

/** Per-category contribution to the running total (counted, empty or undecided). */
enum CategoryStatus:
  case Counted(
      offerId: OfferId,
      contractor: Contractor,
      net: Money,
      gross: Money,
      reason: CountedReason
  )
  case NoOffer

  /** Several offers and no favorite: the user must decide; nothing is counted. */
  case NeedsDecision(candidates: List[OfferId])

/** One category row in [[ProjectTotals]]. */
final case class CategoryTotal(category: Category, status: CategoryStatus)

/** Aggregated running total for a project across its categories. */
final case class ProjectTotals(
    projectId: ProjectId,
    currency: Currency,
    categories: List[CategoryTotal],
    net: Money,
    gross: Money
):
  /** Categories that still have no offer. */
  def missingCategories: List[Category] =
    categories.collect { case CategoryTotal(c, CategoryStatus.NoOffer) => c }

  /** Categories with several offers and no favorite. */
  def undecidedCategories: List[Category] =
    categories.collect { case CategoryTotal(c, CategoryStatus.NeedsDecision(_)) => c }

  /** True when every category is counted (none missing or undecided). */
  def isComplete: Boolean = missingCategories.isEmpty && undecidedCategories.isEmpty

/** Pure total calculation from a validated [[Project]]. */
object Totals:

  /** Builds a counted status from an offer and the reason it was chosen. */
  private def counted(o: Offer, r: CountedReason): CategoryStatus =
    CategoryStatus.Counted(o.id, o.contractor.value, o.net.value, o.gross.value, r)

  /** Which offer (if any) counts for one category. Never picks among several on its own. A favorite
    * is resolved among `offers`; [[Project.from]] guarantees it is one of them.
    */
  def statusFor(offers: List[Offer], favorite: Option[OfferId]): CategoryStatus =
    favorite.flatMap(fav => offers.find(_.id == fav)) match
      case Some(fav) => counted(fav, CountedReason.Favorite)
      case None      =>
        offers match
          case Nil                     => CategoryStatus.NoOffer
          case only :: Nil             => counted(only, CountedReason.OnlyOffer)
          case several @ (_ :: _ :: _) => CategoryStatus.NeedsDecision(several.map(_.id))

  /** Running total over the project's categories, in the project's category order. Total because
    * [[Project.from]] already validated currencies, category references and favorites.
    */
  def compute(project: Project): ProjectTotals =
    val cur = project.currency
    val byCat = project.offers.groupBy(_.category)
    val cats = project.categories.map(c =>
      CategoryTotal(c, statusFor(byCat.getOrElse(c.id, Nil), project.favorites.lift(c.id)))
    )
    val counted = cats.collect { case CategoryTotal(_, s: CategoryStatus.Counted) => s }
    // Project.from guarantees every amount is in `cur`, so plain minor-unit sums are safe.
    /** Sums a money field over counted categories by minor units (same currency via
      * [[Project.from]]).
      */
    def total(f: CategoryStatus.Counted => Money): Money =
      Money(MinorUnits(counted.map(f(_).minor.value).sum), cur)
    ProjectTotals(project.id, cur, cats, total(_.net), total(_.gross))

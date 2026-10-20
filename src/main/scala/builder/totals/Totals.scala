package builder.totals

import builder.domain.*
import cats.syntax.all.*

enum CountedReason:
  case Favorite, OnlyOffer

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

final case class CategoryTotal(category: Category, status: CategoryStatus)

final case class ProjectTotals(
    projectId: ProjectId,
    currency: Currency,
    categories: List[CategoryTotal],
    net: Money,
    gross: Money
):
  def missingCategories: List[Category] =
    categories.collect { case CategoryTotal(c, CategoryStatus.NoOffer) => c }
  def undecidedCategories: List[Category] =
    categories.collect { case CategoryTotal(c, CategoryStatus.NeedsDecision(_)) => c }
  def isComplete: Boolean = missingCategories.isEmpty && undecidedCategories.isEmpty

enum TotalsError:
  case CurrencyMismatch(offerId: OfferId, expected: Currency, actual: Currency)
  case UnknownFavorite(category: Category, offerId: OfferId)
  case Arithmetic(error: MoneyError)

object Totals:

  /** Categories in scope: those with a work package, an offer or a favorite, in enum order. */
  private def categoriesInScope(p: Project): List[Category] =
    val used =
      p.workPackages.map(_.category).toSet ++ p.offers.map(_.category) ++ p.favorites.keySet
    Category.values.toList.filter(used.contains)

  private def checkCurrency(expected: Currency)(o: Offer): Either[TotalsError, Offer] =
    val monies = List(o.net.value, o.vat.value, o.gross.value) ++
      o.lineItems.flatMap(li => List(li.unitPrice.value, li.total.value))
    monies
      .find(_.currency != expected)
      .map(m => TotalsError.CurrencyMismatch(o.id, expected, m.currency))
      .toLeft(o)

  private def counted(o: Offer, r: CountedReason): CategoryStatus =
    CategoryStatus.Counted(o.id, o.contractor.value, o.net.value, o.gross.value, r)

  /** Which offer (if any) counts for one category. Never picks among several on its own. */
  def statusFor(
      category: Category,
      offers: List[Offer],
      favorite: Option[OfferId]
  ): Either[TotalsError, CategoryStatus] =
    favorite match
      case Some(fav) =>
        offers
          .find(_.id == fav)
          .map(counted(_, CountedReason.Favorite))
          .toRight(TotalsError.UnknownFavorite(category, fav))
      case None =>
        offers match
          case Nil                     => Right(CategoryStatus.NoOffer)
          case only :: Nil             => Right(counted(only, CountedReason.OnlyOffer))
          case several @ (_ :: _ :: _) => Right(CategoryStatus.NeedsDecision(several.map(_.id)))

  def compute(project: Project): Either[TotalsError, ProjectTotals] =
    val cur = project.currency
    for
      offers <- project.offers.traverse(checkCurrency(cur))
      byCat = offers.groupBy(_.category)
      cats <- categoriesInScope(project).traverse(c =>
        statusFor(c, byCat.getOrElse(c, Nil), project.favorites.lift(c)).map(CategoryTotal(c, _))
      )
      counted = cats.collect { case CategoryTotal(_, s: CategoryStatus.Counted) => s }
      net <- Money.sum(cur, counted.map(_.net)).leftMap(TotalsError.Arithmetic(_))
      gross <- Money.sum(cur, counted.map(_.gross)).leftMap(TotalsError.Arithmetic(_))
    yield ProjectTotals(project.id, cur, cats, net, gross)

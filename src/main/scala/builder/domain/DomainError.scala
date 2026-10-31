package builder.domain

/** Validation failures when constructing domain values. */
enum DomainError:
  case InvalidId(kind: String, raw: String)
  case InvalidPageNumber(value: Int)
  case InvalidVatRate(percent: BigDecimal)
  case InvalidQuantity(value: BigDecimal)
  case InvalidDate(year: Int, month: Int, day: Int)
  case InvalidMoney(error: MoneyError)
  case NegativeAmount(field: String, amount: Money)
  case GrossMismatch(net: Money, vat: Money, gross: Money)
  case VatMismatch(expected: Money, actual: Money)
  case LineItemsMismatch(lineItemsTotal: Money, net: Money)
  case DuplicateOfferId(offerId: String)
  case InvalidCategoryName(raw: String)
  case InvalidLeistungName(raw: String)
  case DuplicateCategoryId(categoryId: String)

  /** A work package, offer or favorite references a category the project does not have. */
  case UnknownCategory(referencedBy: String, categoryId: String)
  case LineItemTotalMismatch(expected: Money, actual: Money)
  case OfferCurrencyMismatch(offerId: String, expected: Currency, actual: Currency)
  case FavoriteNotInCategory(categoryId: String, offerId: String)

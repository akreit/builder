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

package builder.domain

/** Validation failures when constructing domain values. */
enum DomainError:
  case InvalidId(kind: String, raw: String)
  case InvalidPageNumber(value: Int)
  case InvalidVatRate(percent: BigDecimal)
  case InvalidQuantity(value: BigDecimal)
  case InvalidDate(year: Int, month: Int, day: Int)
  case InvalidMoney(error: MoneyError)

package builder.domain

enum Currency(val code: String, val minorDigits: Int):
  case EUR extends Currency("EUR", 2)
  case CHF extends Currency("CHF", 2)

/** Integral amount in the minor unit of a currency (e.g. cents). */
opaque type MinorUnits = BigInt

object MinorUnits:
  def apply(value: BigInt): MinorUnits = value
  extension (m: MinorUnits) def value: BigInt = m

/** Currency-safe money: amounts in different currencies can never be combined silently. */
final case class Money(minor: MinorUnits, currency: Currency):
  def plus(other: Money): Either[MoneyError, Money] =
    Either.cond(
      currency == other.currency,
      Money(MinorUnits(minor.value + other.minor.value), currency),
      MoneyError.CurrencyMismatch(currency, other.currency)
    )

  def isNegative: Boolean = minor.value < 0

  def toDecimal: BigDecimal = BigDecimal(minor.value, currency.minorDigits)

  /** Plain decimal string with exactly the currency's minor digits, e.g. "1234.50". */
  def show: String = toDecimal.bigDecimal.toPlainString

object Money:
  def zero(currency: Currency): Money = Money(MinorUnits(BigInt(0)), currency)

  def ofMinor(minor: Long, currency: Currency): Money = Money(MinorUnits(BigInt(minor)), currency)

  /** Parses a major-unit decimal, rejecting amounts with more precision than the currency allows.
    */
  def fromDecimal(amount: BigDecimal, currency: Currency): Either[MoneyError, Money] =
    (amount * BigDecimal(10).pow(currency.minorDigits)).toBigIntExact
      .map(m => Money(MinorUnits(m), currency))
      .toRight(MoneyError.TooPrecise(amount, currency))

  /** Sums amounts that must all be in `currency`. */
  def sum(currency: Currency, amounts: List[Money]): Either[MoneyError, Money] =
    amounts.foldLeft[Either[MoneyError, Money]](Right(zero(currency)))((acc, m) =>
      acc.flatMap(_.plus(m))
    )

enum MoneyError:
  case CurrencyMismatch(expected: Currency, actual: Currency)
  case TooPrecise(amount: BigDecimal, currency: Currency)

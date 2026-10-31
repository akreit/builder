package builder.ui

import builder.domain.*

import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale

/** German display formats: `12.500,00 €`, `15.09.2026`. */
object Format:
  private def symbol(c: Currency): String = c match
    case Currency.EUR => "€"
    case Currency.CHF => "CHF"

  /** DecimalFormat is not thread-safe, so a fresh instance per call. */
  private def pattern(digits: Int): DecimalFormat =
    DecimalFormat(
      if digits > 0 then "#,##0." + "0" * digits else "#,##0",
      DecimalFormatSymbols.getInstance(Locale.GERMANY)
    )

  def money(m: Money): String =
    s"${pattern(m.currency.minorDigits).format(m.toDecimal.bigDecimal)} ${symbol(m.currency)}"

  private val dateFormat = DateTimeFormatter.ofPattern("dd.MM.yyyy")

  /** Score with one decimal, German style ("87,5"). */
  def decimal(d: BigDecimal): String = pattern(1).format(d.bigDecimal)

  def date(d: LocalDate): String = dateFormat.format(d)

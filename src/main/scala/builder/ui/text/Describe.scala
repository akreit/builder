package builder.ui.text

import builder.domain.*
import builder.extraction.*
import builder.ui.Format

/** German texts for domain, check and extraction errors. */
object Describe:
  /** German description of a validation error, for the UI. */
  def describe(e: DomainError): String = e match
    case DomainError.InvalidId(kind, raw)          => s"Ungültige Kennung ($kind): $raw"
    case DomainError.InvalidPageNumber(n)          => s"Ungültige Seitenzahl: $n"
    case DomainError.InvalidVatRate(pct)           => s"Ungültiger MwSt.-Satz: $pct %"
    case DomainError.InvalidQuantity(q)            => s"Ungültige Menge: $q"
    case DomainError.InvalidDate(y, m, d)          => s"Ungültiges Datum: $d.$m.$y"
    case DomainError.InvalidMoney(err)             => s"Ungültiger Betrag: $err"
    case DomainError.NegativeAmount(field, amount) =>
      s"Negativer Betrag ($field): ${Format.money(amount)}"
    case DomainError.GrossMismatch(net, vat, gross) =>
      s"Brutto ${Format.money(gross)} ≠ Netto ${Format.money(net)} + MwSt. ${Format.money(vat)}"
    case DomainError.VatMismatch(expected, actual) =>
      s"MwSt. ${Format.money(actual)} statt erwartet ${Format.money(expected)}"
    case DomainError.LineItemsMismatch(sum, net) =>
      s"Summe der Positionen ${Format.money(sum)} ≠ Netto ${Format.money(net)}"
    case DomainError.DuplicateOfferId(o)      => s"Angebot doppelt vorhanden: $o"
    case DomainError.InvalidCategoryName(raw) => s"Ungültiger Kategoriename: '$raw'"
    case DomainError.InvalidLeistungName(raw) => s"Ungültiger Name der Leistung: '$raw'"
    case DomainError.InvalidStars(n)          => s"Bewertung muss 1 bis 5 Sterne haben, nicht $n"
    case DomainError.InvalidWeights(_)        =>
      "Gewichtungen müssen je zwischen 0 und 100 % liegen und zusammen 100 % ergeben"
    case DomainError.DuplicateCategoryId(c)          => s"Kategorie doppelt vorhanden: $c"
    case DomainError.UnknownCategory(by, c)          => s"Unbekannte Kategorie '$c' ($by)"
    case DomainError.LineItemTotalMismatch(exp, act) =>
      s"Positionssumme ${Format.money(act)} statt erwartet ${Format.money(exp)}"
    case DomainError.OfferCurrencyMismatch(o, exp, act) =>
      s"Angebot $o in ${act.code} statt ${exp.code}"
    case DomainError.FavoriteNotInCategory(c, o) =>
      s"Angebot $o gehört nicht zur Kategorie $c"

  /** German user-facing text for an [[Issue]]. */
  def describe(i: Issue): String = i match
    case Issue.Missing(field)         => s"$field fehlt"
    case Issue.Unreadable(field, raw) => s"$field nicht lesbar: '$raw'"
    case Issue.Invalid(field, e)      => s"$field: ${describe(e)}"

  /** German user-facing text for an [[ExtractionError]] (no secrets/bodies). */
  def describe(e: ExtractionError): String = e match
    case ExtractionError.MissingApiKey   => "Kein ANTHROPIC_API_KEY gesetzt"
    case ExtractionError.Transport(kind) => s"Verbindung zur Claude-API fehlgeschlagen ($kind)"
    case ExtractionError.HttpStatus(code, errorType) =>
      s"Claude-API antwortete mit Status $code${errorType.fold("")(t => s" ($t)")}"
    case ExtractionError.TimedOut              => "Claude-API hat nicht rechtzeitig geantwortet"
    case ExtractionError.UnexpectedResponse(m) => s"Unerwartete Antwort der Claude-API: $m"
    case ExtractionError.UnreadablePdf(reason) => s"PDF konnte nicht gelesen werden ($reason)"

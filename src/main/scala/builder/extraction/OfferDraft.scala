package builder.extraction

import builder.domain.*
import cats.data.NonEmptyList
import cats.syntax.all.*

import java.time.LocalDate
import scala.util.Try

/** Why an extracted offer could not be turned into a validated [[Offer]]. */
enum Issue:
  case Missing(field: String)
  case Unreadable(field: String, raw: String)
  case Invalid(field: String, error: DomainError)

/** Validates a [[RawOffer]] with the domain smart constructors, collecting every problem. */
object OfferDraft:
  type Checked[A] = Either[NonEmptyList[Issue], A]

  private def extracted[A](f: Field, a: A): Extracted[A] = Extracted(a, f.pageNumber, f.confidence)

  private def required(label: String, f: Field): Checked[String] =
    f.text.toRight(NonEmptyList.one(Issue.Missing(label)))

  private def decimal(label: String, s: String): Checked[BigDecimal] =
    Try(BigDecimal(s.replace(" ", ""))).toOption
      .toRight(NonEmptyList.one(Issue.Unreadable(label, s)))

  private def domain[A](label: String, e: Either[DomainError, A]): Checked[A] =
    e.left.map(err => NonEmptyList.one(Issue.Invalid(label, err)))

  private def money(label: String, f: Field, cur: Currency): Checked[Extracted[Money]] =
    for
      s <- required(label, f)
      d <- decimal(label, s)
      m <- domain(label, Money.fromDecimal(d, cur).left.map(DomainError.InvalidMoney(_)))
    yield extracted(f, m)

  private def date(label: String, s: String): Checked[LocalDate] =
    Try(LocalDate.parse(s)).toOption.toRight(NonEmptyList.one(Issue.Unreadable(label, s)))

  private def optionalText(f: Field): Option[Extracted[String]] = f.text.map(extracted(f, _))

  private def optionalDate(label: String, f: Field): Checked[Option[Extracted[LocalDate]]] =
    f.text.traverse(s => date(label, s).map(extracted(f, _)))

  private def lineItem(n: Int, li: RawLineItem, cur: Currency): Checked[LineItem] =
    val p = s"Position $n"
    val quantity = for
      s <- required(s"$p: Menge", li.quantity)
      d <- decimal(s"$p: Menge", s)
      q <- domain(s"$p: Menge", Quantity.from(d))
    yield extracted(li.quantity, q)
    (
      required(s"$p: Beschreibung", li.description).map(extracted(li.description, _)),
      quantity,
      money(s"$p: Einzelpreis", li.unitPrice, cur),
      money(s"$p: Gesamtpreis", li.total, cur)
    ).parTupled.flatMap((d, q, up, t) =>
      domain(
        p,
        LineItem.from(
          d,
          q,
          optionalText(li.unit),
          optionalText(li.sizes),
          optionalText(li.manufacturer),
          optionalText(li.model),
          up,
          t
        )
      )
    )

  /** Builds the offer with [[LineItem.from]] and [[Offer.from]]; every missing, unreadable or
    * inconsistent value is reported, nothing is guessed.
    */
  def validate(raw: RawOffer, id: OfferId, category: CategoryId, cur: Currency): Checked[Offer] =
    val vatRate = for
      s <- required("MwSt.-Satz", raw.vatRate)
      d <- decimal("MwSt.-Satz", s.stripSuffix("%").trim)
      r <- domain("MwSt.-Satz", VatRate.from(d))
    yield extracted(raw.vatRate, r)
    (
      required("Firma", raw.contractor).map(c => extracted(raw.contractor, Contractor(c))),
      required("Angebotsdatum", raw.date)
        .flatMap(date("Angebotsdatum", _))
        .map(extracted(raw.date, _)),
      optionalDate("Gültig bis", raw.validUntil),
      raw.lineItems.zipWithIndex.parTraverse((li, i) => lineItem(i + 1, li, cur)),
      money("Netto", raw.net, cur),
      vatRate,
      money("MwSt.", raw.vat, cur),
      money("Brutto", raw.gross, cur)
    ).parTupled.flatMap((contractor, d, valid, items, net, rate, vat, gross) =>
      domain(
        "Angebot",
        Offer.from(
          id,
          contractor,
          d,
          valid,
          category,
          items,
          net,
          rate,
          vat,
          gross,
          raw.notes.flatMap(_.text),
          raw.exclusions.flatMap(_.text)
        )
      )
    )

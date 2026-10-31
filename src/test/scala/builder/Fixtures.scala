package builder

import builder.domain.*
import org.scalatest.EitherValues

object Fixtures extends EitherValues:
  def offerId(s: String): OfferId = OfferId.from(s).value

  def category(id: String, name: String): Category =
    Category(CategoryId.from(id).value, CategoryName.from(name).value)

  val Windows: Category = category("windows", "Fenster")
  val Roofing: Category = category("roofing", "Dach")
  val Electrical: Category = category("electrical", "Elektro")
  val Plumbing: Category = category("plumbing", "Sanitär")
  val Flooring: Category = category("flooring", "Boden")
  val Painting: Category = category("painting", "Maler")
  val Other: Category = category("other", "Sonstiges")
  val allCategories: List[Category] =
    List(Windows, Roofing, Electrical, Plumbing, Flooring, Painting, Other)

  val vat19: VatRate = VatRate.from(19).value

  def lineItem(description: String, total: Money): LineItem =
    LineItem
      .from(
        description = Extracted.manual(description),
        quantity = Extracted.manual(Quantity.from(1).value),
        unit = None,
        sizes = None,
        manufacturer = None,
        model = None,
        unitPrice = Extracted.manual(total),
        total = Extracted.manual(total)
      )
      .value

  /** Raw arguments to [[Offer.from]], so tests can build inconsistent variants. */
  final case class OfferArgs(
      id: OfferId,
      category: Category,
      lineItems: List[LineItem],
      net: Money,
      vat: Money,
      gross: Money
  ):
    def build: Either[DomainError, Offer] =
      Offer.from(
        id = id,
        contractor = Extracted.manual(Contractor(s"Contractor ${id.value}")),
        date = Extracted.manual(Dates.of(2026, 10, 1).value),
        validUntil = None,
        category = category.id,
        lineItems = lineItems,
        net = Extracted.manual(net),
        vatRate = Extracted.manual(vat19),
        vat = Extracted.manual(vat),
        gross = Extracted.manual(gross),
        notes = Nil,
        exclusions = Nil
      )

  /** Consistent arguments: one line item equal to net, VAT via the shared HALF_UP helper. */
  def offerArgs(
      id: String,
      cat: Category,
      netCents: Long,
      cur: Currency = Currency.EUR
  ): OfferArgs =
    val net = Money.ofMinor(netCents, cur)
    val vat = vat19.vatOn(net)
    OfferArgs(offerId(id), cat, List(lineItem("work", net)), net, vat, net.plus(vat).value)

  def offer(id: String, cat: Category, netCents: Long, cur: Currency = Currency.EUR): Offer =
    offerArgs(id, cat, netCents, cur).build.value

  def projectE(
      wps: List[Category],
      offers: List[Offer],
      favorites: Map[Category, OfferId] = Map.empty,
      id: String = "p1",
      categories: List[Category] = allCategories
  ): Either[DomainError, Project] =
    Project.from(
      ProjectId.from(id).value,
      "Test",
      Currency.EUR,
      categories,
      wps.map(c => WorkPackage(c.id, "wp", Nil)),
      offers,
      favorites.map((c, o) => c.id -> o)
    )

  def project(
      wps: List[Category],
      offers: List[Offer],
      favorites: Map[Category, OfferId] = Map.empty,
      id: String = "p1",
      categories: List[Category] = allCategories
  ): Project = projectE(wps, offers, favorites, id, categories).value

  def eur(cents: Long): Money = Money.ofMinor(cents, Currency.EUR)

/** Builders for extractor output and a fake [[builder.extraction.OfferExtractor]]. */
object RawOffers:
  import builder.extraction.*
  import cats.effect.IO

  def f(value: String, page: Int = 1, c: Confidence = Confidence.High): Field =
    Field(Some(value), Some(page), c)
  val absent: Field = Field(None, None, Confidence.High)

  def item(desc: String, qty: String, unit: String, price: String, total: String): RawLineItem =
    RawLineItem(f(desc, 2), f(qty, 2), f(unit, 2), absent, absent, absent, f(price, 2), f(total, 2))

  /** Fictional Dachfenster offer: 3 × 1.250,00 + 3 × 420,00 = 5.010,00 net, 19 % VAT. */
  val dachfenster: RawOffer = RawOffer(
    contractor = f("Zimmerei Erfunden GmbH"),
    date = f("2026-10-12"),
    validUntil = f("2026-12-31", c = Confidence.Medium),
    lineItems = List(
      item("Dachfenster liefern", "3", "Stk", "1250.00", "3750.00")
        .copy(model = f("GGU 0070", 2, Confidence.Low), sizes = f("78 x 140 cm", 2)),
      item("Einbau inkl. Eindeckrahmen", "3", "Stk", "420.00", "1260.00")
    ),
    net = f("5010.00", 3),
    vatRate = f("19", 3),
    vat = f("951.90", 3),
    gross = f("5961.90", 3),
    notes = List(f("Bindefrist 6 Wochen", 3)),
    exclusions = List(f("Innenfutter", 3))
  )

  /** Returns `result` for every file and remembers how often it was called. */
  final class FakeExtractor(result: Either[ExtractionError, RawOffer]) extends OfferExtractor[IO]:
    def extract(file: Array[Byte], fileType: OfferFileType): IO[Either[ExtractionError, RawOffer]] =
      IO.pure(result)

  /** Like [[FakeExtractor]], and counts its calls. */
  final class CountingExtractor(result: Either[ExtractionError, RawOffer])
      extends OfferExtractor[IO]:
    private val ref = cats.effect.Ref.unsafe[IO, Int](0)
    def calls: IO[Int] = ref.get
    def extract(file: Array[Byte], fileType: OfferFileType): IO[Either[ExtractionError, RawOffer]] =
      ref.update(_ + 1).as(result)

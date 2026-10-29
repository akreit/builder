package builder

import builder.domain.*

/** Demo project "Dach" served by the in-memory repository.
  *
  * SAMPLE DATA: all contractor names ("Musterzimmerei …"), offers and amounts are fictional.
  */
object SampleData:
  private val eur = Currency.EUR

  private final case class Position(description: String, qty: BigDecimal, unit: String, cents: Long)

  private def lineItem(pos: Position, pg: PageNumber): Either[DomainError, LineItem] =
    val ex = [A] => (a: A) => Extracted.fromPage(a, pg, Confidence.High)
    val price = Money.ofMinor(pos.cents, eur)
    Quantity
      .from(pos.qty)
      .flatMap(q =>
        LineItem.from(
          description = ex(pos.description),
          quantity = ex(q),
          unit = Some(ex(pos.unit)),
          sizes = None,
          manufacturer = None,
          model = None,
          unitPrice = ex(price),
          total = ex(price.times(q.value))
        )
      )

  /** An offer; with no positions it is a Pauschalangebot of `lumpSumCents`. */
  private def offer(
      id: String,
      contractor: String,
      category: CategoryId,
      day: Int,
      positions: List[Position],
      lumpSumCents: Long = 0
  ): Either[DomainError, Offer] =
    for
      offerId <- OfferId.from(id)
      pg <- PageNumber.from(1)
      rate <- VatRate.from(19)
      date <- Dates.of(2026, 9, day)
      valid <- Dates.of(2026, 12, day)
      ex = [A] => (a: A) => Extracted.fromPage(a, pg, Confidence.High)
      items <- positions.foldRight[Either[DomainError, List[LineItem]]](Right(Nil))((p, acc) =>
        acc.flatMap(rest => lineItem(p, pg).map(_ :: rest))
      )
      net <- Money.sum(eur, items.map(_.total.value)).left.map(DomainError.InvalidMoney(_))
      netOrLump = if items.isEmpty then Money.ofMinor(lumpSumCents, eur) else net
      vat = rate.vatOn(netOrLump)
      gross <- netOrLump.plus(vat).left.map(DomainError.InvalidMoney(_))
      o <- Offer.from(
        id = offerId,
        contractor = ex(Contractor(contractor)),
        date = ex(date),
        validUntil = Some(Extracted.fromPage(valid, pg, Confidence.Medium)),
        category = category,
        lineItems = items,
        net = ex(netOrLump),
        vatRate = ex(rate),
        vat = ex(vat),
        gross = ex(gross),
        notes = List("Beispieldaten – fiktives Angebot"),
        exclusions = List("Entsorgung Altmaterial")
      )
    yield o

  private def category(id: String, name: String): Either[DomainError, Category] =
    for
      cid <- CategoryId.from(id)
      cn <- CategoryName.from(name)
    yield Category(cid, cn)

  val project: Either[DomainError, Project] =
    for
      id <- ProjectId.from("demo")
      gauben <- category("gauben", "Gauben")
      daemmung <- category("daemmung", "Dämmung")
      spengler <- category("spenglerarbeiten", "Spenglerarbeiten")
      balkon <- category("balkondaemmung", "Balkondämmung")
      fenster <- category("dachfenster", "Dachfenster")
      sonstiges <- category("sonstiges", "Sonstiges")
      g1 <- offer(
        "gauben-a",
        "Musterzimmerei Holzwurm GmbH",
        gauben.id,
        8,
        List(
          Position("Schleppgaube abbinden und aufstellen", 2, "Stk", 625_000),
          Position("Gaubenwangen verkleiden", 18.5, "m²", 8_900)
        )
      )
      g2 <- offer(
        "gauben-b",
        "Zimmerei Beispiel & Söhne",
        gauben.id,
        12,
        List(Position("Satteldachgaube komplett", 2, "Stk", 715_000))
      )
      d1 <- offer(
        "daemmung-a",
        "Musterzimmerei Holzwurm GmbH",
        daemmung.id,
        8,
        List(
          Position("Zwischensparrendämmung WLG 035, 200 mm", 120, "m²", 4_850),
          Position("Dampfbremse verlegen und verkleben", 120, "m²", 1_275)
        )
      )
      d2 <- offer(
        "daemmung-b",
        "Holzbau Fiktiv KG",
        daemmung.id,
        15,
        List(Position("Aufsparrendämmung Holzfaser, 160 mm", 120, "m²", 8_990))
      )
      s1 <- offer(
        "spengler-a",
        "Spenglerei Probe",
        spengler.id,
        20,
        List(
          Position("Dachrinne Titanzink, halbrund", 24, "m", 6_450),
          Position("Fallrohr Titanzink", 12, "m", 5_200)
        )
      )
      b1 <- offer("balkon-a", "Zimmerei Beispiel & Söhne", balkon.id, 22, Nil, 480_000)
      p <- Project.from(
        id = id,
        name = "Dach",
        currency = eur,
        categories = List(gauben, daemmung, spengler, balkon, fenster, sonstiges),
        workPackages = List(
          WorkPackage(gauben.id, "Zwei Gauben straßenseitig", List("Dachgeschoss")),
          WorkPackage(daemmung.id, "Dach komplett dämmen", List("Dachgeschoss")),
          WorkPackage(fenster.id, "Drei Dachfenster tauschen", List("Dachgeschoss"))
        ),
        offers = List(g1, g2, d1, d2, s1, b1),
        favorites = Map(gauben.id -> g1.id)
      )
    yield p

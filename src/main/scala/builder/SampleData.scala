package builder

import builder.domain.*
import cats.syntax.all.*

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
          quantity = QuantityInfo.Known(ex(q)),
          unit = Some(ex(pos.unit)),
          sizes = None,
          manufacturer = None,
          model = None,
          unitPrice = PriceInfo.Known(ex(price)),
          total = PriceInfo.Known(ex(price.times(q.value)))
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
      net <- Money
        .sum(
          eur,
          items.flatMap(li =>
            li.total match
              case PriceInfo.Known(e) => List(e.value)
              case PriceInfo.Missing  => Nil
          )
        )
        .left
        .map(DomainError.InvalidMoney(_))
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

  /** The trade categories of a project, in tab order (explicit, not alphabetical). */
  val categories: Either[DomainError, List[Category]] =
    List(
      "dach" -> "Dach",
      "fenster" -> "Fenster",
      "fassadendaemmung" -> "Fassadendämmung",
      "elektrik" -> "Elektrik",
      "heizung" -> "Heizung",
      "innenausbau" -> "Innenausbau"
    ).traverse(category.tupled)

  val project: Either[DomainError, Project] =
    for
      id <- ProjectId.from("demo")
      cats <- categories
      dach <- category("dach", "Dach")
      fenster <- category("fenster", "Fenster")
      fassade <- category("fassadendaemmung", "Fassadendämmung")
      elektrik <- category("elektrik", "Elektrik")
      heizung <- category("heizung", "Heizung")
      g1 <- offer(
        "gauben-a",
        "Musterzimmerei Holzwurm GmbH",
        dach.id,
        8,
        List(
          Position("Schleppgaube abbinden und aufstellen", 2, "Stk", 625_000),
          Position("Gaubenwangen verkleiden", 18.5, "m²", 8_900)
        )
      )
      g2 <- offer(
        "gauben-b",
        "Zimmerei Beispiel & Söhne",
        dach.id,
        12,
        List(Position("Satteldachgaube komplett", 2, "Stk", 715_000))
      )
      d1 <- offer(
        "fassade-a",
        "Musterzimmerei Holzwurm GmbH",
        fassade.id,
        8,
        List(
          Position("WDVS Mineralwolle WLG 035, 200 mm", 120, "m²", 4_850),
          Position("Armierung und Oberputz", 120, "m²", 1_275)
        )
      )
      d2 <- offer(
        "fassade-b",
        "Holzbau Fiktiv KG",
        fassade.id,
        15,
        List(Position("Holzfaser-Fassadendämmung, 160 mm", 120, "m²", 8_990))
      )
      h1 <- offer(
        "heizung-a",
        "Heizungsbau Probe",
        heizung.id,
        20,
        List(
          Position("Heizungsrohr Kupfer verlegen", 24, "m", 6_450),
          Position("Heizkörper anschließen", 12, "Stk", 5_200)
        )
      )
      e1 <- offer("elektrik-a", "Elektro Beispiel & Söhne", elektrik.id, 22, Nil, 480_000)
      p <- Project.from(
        id = id,
        name = "Dach",
        currency = eur,
        categories = cats,
        workPackages = List(
          WorkPackage(dach.id, "Zwei Gauben straßenseitig", List("Dachgeschoss")),
          WorkPackage(fassade.id, "Fassade komplett dämmen", List("Außenwand")),
          WorkPackage(fenster.id, "Drei Fenster tauschen", List("Dachgeschoss"))
        ),
        offers = List(g1, g2, d1, d2, h1, e1),
        favorites = Map(dach.id -> g1.id)
      )
    yield p

  /** Coarse default Leistungen (work steps) per category, seeded once; extractions add more. */
  val leistungen: Either[DomainError, List[(ProjectId, Leistung)]] =
    for
      pid <- ProjectId.from("demo")
      fenster <- CategoryId.from("fenster")
      ls <- List(
        "fenster-fenster" -> "Fenster inkl. Zubehör",
        "fenster-einbau" -> "Einbau",
        "fenster-innenausbau" -> "Innenausbau",
        "fenster-entsorgung" -> "Entsorgung"
      ).traverse((id, name) =>
        (LeistungId.from(id), LeistungName.from(name)).mapN(Leistung(_, fenster, _))
      )
    yield ls.map(pid -> _)

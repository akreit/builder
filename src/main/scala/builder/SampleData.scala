package builder

import builder.domain.*

/** Demo project served by the in-memory repository. */
object SampleData:
  private val eur = Currency.EUR

  private def offer(
      id: String,
      contractor: String,
      category: Category,
      netCents: Long,
      page: Int
  ): Either[DomainError, Offer] =
    for
      offerId <- OfferId.from(id)
      pg <- PageNumber.from(page)
      rate <- VatRate.from(19)
      qty <- Quantity.from(1)
      date <- Dates.of(2026, 9, 15)
      valid <- Dates.of(2026, 12, 15)
    yield
      def ex[A](a: A) = Extracted.fromPage(a, pg, Confidence.High)
      val vatCents = netCents * 19 / 100
      val net = Money.ofMinor(netCents, eur)
      Offer(
        id = offerId,
        contractor = ex(Contractor(contractor)),
        date = ex(date),
        validUntil = Some(Extracted.fromPage(valid, pg, Confidence.Medium)),
        category = category,
        lineItems = List(
          LineItem(
            description = ex(s"$category works"),
            quantity = ex(qty),
            unit = Some(ex("lump sum")),
            sizes = None,
            manufacturer = None,
            model = None,
            unitPrice = ex(net),
            total = ex(net)
          )
        ),
        net = ex(net),
        vatRate = ex(rate),
        vat = ex(Money.ofMinor(vatCents, eur)),
        gross = ex(Money.ofMinor(netCents + vatCents, eur)),
        notes = Nil,
        exclusions = List("Disposal of old material")
      )

  val project: Either[DomainError, Project] =
    for
      id <- ProjectId.from("demo")
      w1 <- offer("win-a", "Fensterbau Müller", Category.Windows, 1_250_000, 1)
      w2 <- offer("win-b", "Glas & Rahmen GmbH", Category.Windows, 1_180_000, 2)
      r1 <- offer("roof-a", "Dachdeckerei Schmidt", Category.Roofing, 2_400_000, 1)
      e1 <- offer("elec-a", "Elektro Weber", Category.Electrical, 640_000, 1)
      e2 <- offer("elec-b", "Volt Service", Category.Electrical, 590_000, 1)
    yield Project(
      id = id,
      name = "Altbau Sanierung",
      currency = eur,
      workPackages =
        List(
          WorkPackage(Category.Windows, "Replace all windows", List("Living room", "Bedrooms")),
          WorkPackage(Category.Roofing, "Re-roof and insulate", List("Roof")),
          WorkPackage(Category.Electrical, "Rewire ground floor", List("Ground floor")),
          WorkPackage(Category.Plumbing, "New bathroom pipes", List("Bathroom"))
        ),
      offers = List(w1, w2, r1, e1, e2),
      favorites = Map(Category.Windows -> w1.id)
    )

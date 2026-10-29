package builder

import builder.domain.*
import builder.extraction.*
import builder.ranking.*
import builder.repo.*
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import fs2.Stream
import org.http4s.*
import org.http4s.multipart.Multiparts
import org.http4s.multipart.Part
import org.scalatest.EitherValues
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class RankingSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers with EitherValues:
  import RawOffers.*

  private val cat = CategoryId.from("dachfenster").value
  private def leistung(id: String) =
    Leistung(LeistungId.from(id).value, cat, LeistungName.from(id).value)
  private val fenster = leistung("fenster")
  private val einbau = leistung("einbau")

  private def column(id: String, name: String): MatrixColumn =
    val raw = dachfenster.copy(contractor = f(name))
    MatrixColumn(
      UploadView(
        Upload(
          Fixtures.offerId(id),
          cat,
          s"$id.pdf",
          OfferFileType.Pdf,
          Instant.EPOCH,
          UploadOutcome.Read(raw)
        ),
        UploadStatus.ExtractionFailed("egal")
      ),
      raw
    )

  private def has(euros: Long) =
    Cell(
      Coverage.Enthalten,
      CoverageSource.FromItems,
      List(0),
      Some(Fixtures.eur(euros * 100)),
      flagged = false
    )
  private val lacks =
    Cell(Coverage.Ausgeschlossen, CoverageSource.Manual, Nil, None, flagged = false)

  private val colA = column("a", "Firma A")
  private val colB = column("b", "Firma B")

  /** A: Fenster 1000 + Einbau 500 = 1500; B: Fenster 1200, no Einbau = 1200 in total. */
  private val matrix = Matrix(
    cat,
    List(colA, colB),
    List(MatrixRow(fenster, List(has(1000), has(1200))), MatrixRow(einbau, List(has(500), lacks))),
    Nil
  )

  private def key(s: String) = ContractorKey.of(s).fold(fail(s"no key for $s"))(identity)
  private def rating(n: Int) = Rating(Stars.from(n).value, "")
  private def weights(p: Int, r: Int) =
    Weights.from(Map(Criterion.Preis -> p, Criterion.Referenzen -> r)).value
  private def price(e: RankedOffer) = e.criteria.find(_.criterion == Criterion.Preis).map(_.result)
  private def refs(e: RankedOffer) =
    e.criteria.find(_.criterion == Criterion.Referenzen).map(_.result)

  "prices are compared on the Leistungen all offers include, missing ones are named" in {
    val r = Ranking.build(matrix, Map.empty, weights(100, 0), Currency.EUR)
    r.common shouldBe List(fenster)
    // B is cheaper in total only because Einbau is missing; on Fenster alone A is cheaper.
    r.ranked.map(e => (e.contractor, e.price.comparable, e.total)) shouldBe List(
      ("Firma A", Some(Fixtures.eur(100000)), Some(BigDecimal("100.0"))),
      ("Firma B", Some(Fixtures.eur(120000)), Some(BigDecimal("83.3")))
    )
    r.ranked.map(_.price.missing) shouldBe List(Nil, List(einbau))
  }

  "an unrated contractor is 'nicht bewertet' and not ranked, never scored 0" in {
    val r = Ranking.build(matrix, Map(key("Firma B") -> rating(5)), Weights.default, Currency.EUR)
    r.ranked.map(e => (e.contractor, e.total)) shouldBe List("Firma B" -> Some(BigDecimal("91.7")))
    r.unranked.map(_.contractor) shouldBe List("Firma A")
    r.unranked.flatMap(refs) shouldBe List(CriterionResult.NotAvailable("nicht bewertet"))
  }

  "weights decide between a cheaper and a better rated offer" in {
    val ratings = Map(key("firma a") -> rating(2), key(" FIRMA  B ") -> rating(5))
    def order(p: Int, r: Int) =
      Ranking
        .build(matrix, ratings, weights(p, r), Currency.EUR)
        .ranked
        .map(e => e.contractor -> e.total)
    order(80, 20) shouldBe List(
      "Firma A" -> Some(BigDecimal("88.0")),
      "Firma B" -> Some(BigDecimal("86.6"))
    )
    order(50, 50) shouldBe List(
      "Firma B" -> Some(BigDecimal("91.7")),
      "Firma A" -> Some(BigDecimal("70.0"))
    )
  }

  "without a Leistung all offers include with a price, there is no price score" in {
    val none = matrix.copy(rows = List(MatrixRow(einbau, List(has(500), lacks))))
    val r = Ranking.build(none, Map.empty, weights(100, 0), Currency.EUR)
    r.common shouldBe Nil
    r.ranked shouldBe Nil
    r.unranked.flatMap(price).distinct shouldBe
      List(CriterionResult.NotAvailable("keine Leistung, die alle Angebote mit Preis enthalten"))
  }

  "weights must be 0 to 100 for every criterion with at least one above 0; stars 1 to 5" in {
    Weights.from(Map(Criterion.Preis -> 0, Criterion.Referenzen -> 0)).isLeft shouldBe true
    Weights.from(Map(Criterion.Preis -> 101, Criterion.Referenzen -> 0)).isLeft shouldBe true
    Weights.from(Map(Criterion.Preis -> 100)).isLeft shouldBe true
    Stars.from(0).isLeft shouldBe true
    Stars.from(6).isLeft shouldBe true
  }

  // ---- through the app -----------------------------------------------------------------------

  private val pdf: Array[Byte] = "%PDF-1.7\nfiktiv".getBytes("UTF-8")
  private val hx = Header.Raw(org.typelevel.ci.CIString("HX-Request"), "true")

  private def run(app: TestApp, req: Request[IO]): IO[(Status, String)] =
    app.http.run(req).flatMap(r => r.bodyText.compile.string.map(r.status -> _))

  private def upload(app: TestApp): IO[(Status, String)] =
    Multiparts
      .forSync[IO]
      .flatMap(
        _.multipart(Vector(Part.fileData[IO]("datei", "a.pdf", Stream.emits(pdf.toIndexedSeq))))
      )
      .flatMap(mp =>
        run(
          app,
          Request[IO](
            Method.POST,
            Uri.unsafeFromString("/ui/projects/demo/categories/dachfenster/angebote")
          )
            .withEntity(mp)
            .putHeaders(mp.headers)
            .putHeaders(hx)
        )
      )

  private def edit(app: TestApp, fields: (String, String)*): IO[(Status, String)] =
    run(
      app,
      Request[IO](Method.POST, Uri.unsafeFromString("/ui/projects/demo/aenderungen"))
        .withEntity(UrlForm(fields*))
        .putHeaders(hx)
    )

  /** (rank, contractor, total) rows of the Dachfenster ranking. */
  private def ranking(html: String): List[(String, String, String)] =
    """<tr data-firma="([^"]+)"><td>([^<]+)</td>.*?<td class="betrag gesamt">(?:<strong>|<span class="hinweis">)([^<]+)<""".r
      .findAllMatchIn(html)
      .map(m => (m.group(2), m.group(1), m.group(3)))
      .toList

  private val second = dachfensterMitLeistungen.copy(contractor = f("Dachbau Zweit KG"))

  "the ranking lists offers, ratings are editable and survive a restart with the weights" in {
    TestApp.tempDir.use { dir =>
      val extractor = RecordingExtractor(List(dachfensterMitLeistungen, second))
      for
        (_, unrated) <- TestApp.resource(dir, extractor).use(app => upload(app) >> upload(app))
        (rated, afterRating) <- TestApp
          .resource(dir, extractor)
          .use(app =>
            edit(
              app,
              "aktion" -> "bewertung",
              "firma" -> "Zimmerei Erfunden GmbH",
              "sterne" -> "4",
              "notiz" -> "Nachbar zufrieden"
            ) >>
              edit(
                app,
                "aktion" -> "gewichtung",
                "gewicht-preis" -> "70",
                "gewicht-referenzen" -> "30"
              )
          )
        (_, restarted) <- TestApp
          .resource(dir, extractor)
          .use(app => run(app, Request[IO](Method.GET, Uri.unsafeFromString("/"))))
        (badStars, _) <- TestApp
          .resource(dir, extractor)
          .use(app => edit(app, "aktion" -> "bewertung", "firma" -> "X", "sterne" -> "6"))
        (badWeights, _) <- TestApp
          .resource(dir, extractor)
          .use(app =>
            edit(app, "aktion" -> "gewichtung", "gewicht-preis" -> "0", "gewicht-referenzen" -> "0")
          )
      yield
        ranking(unrated).map(_._3) shouldBe List("nicht bewertbar", "nicht bewertbar")
        unrated should include("nicht bewertet")
        rated shouldBe Status.Ok
        // Same prices, so both get 100 price points; only the rated one has a rank: 0.7·100 + 0.3·80.
        ranking(restarted) shouldBe List(
          ("1.", "Zimmerei Erfunden GmbH", "94,0 Pkt."),
          ("–", "Dachbau Zweit KG", "nicht bewertbar")
        )
        restarted should include("Nachbar zufrieden")
        restarted should include("Preis (70 %)")
        afterRating should include("Referenzen (30 %)")
        badStars shouldBe Status.BadRequest
        badWeights shouldBe Status.BadRequest
    }
  }

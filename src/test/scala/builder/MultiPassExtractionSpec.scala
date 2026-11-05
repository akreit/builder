package builder

import builder.domain.Confidence
import builder.extraction.*
import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.testing.scalatest.AsyncIOSpec
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.client.Client
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*

/** Long PDFs are read as text and split into page-aware passes; the Anthropic HTTP API is replaced
  * by a fake client that answers each pass with a fragment derived from the pages it was sent.
  */
class MultiPassExtractionSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers:
  import RawOffers.{absent, f}

  private val prompts = ExtractionPrompts("Lies das Angebot.", "keine", "bekannt:\n", "{label}")
  private val apiKey = ApiKey.from("test-key")
  private val pageMarker = "=== Seite (\\d+) ===".r

  /** What the fake API saw in one call. */
  private final case class Call(pages: List[Int], kinds: List[String])

  private def kinds(body: Json): List[String] =
    body.hcursor
      .downField("messages")
      .downArray
      .downField("content")
      .as[List[Json]]
      .getOrElse(Nil)
      .flatMap(_.hcursor.downField("type").as[String].toOption)

  private def documentText(body: Json): String =
    body.hcursor
      .downField("messages")
      .downArray
      .downField("content")
      .downArray
      .downField("text")
      .as[String]
      .getOrElse("")

  /** Fragment for the given pages: one line item per page, header on page 1, totals on the last. */
  private def fragment(pages: List[Int], lastPage: Int): RawOffer =
    RawOffer(
      contractor = if pages.contains(1) then f("Dachdeckerei Muster GmbH") else absent,
      date = if pages.contains(1) then f("2026-10-01") else absent,
      validUntil = absent,
      lineItems = pages.distinct.map(p => RawOffers.item(s"Seite $p", "1", "Stk", "1.00", "1.00")),
      net = if pages.contains(lastPage) then f("100.00", lastPage) else absent,
      vatRate = if pages.contains(lastPage) then f("19", lastPage) else absent,
      vat = if pages.contains(lastPage) then f("19.00", lastPage) else absent,
      gross = if pages.contains(lastPage) then f("119.00", lastPage) else absent,
      notes = Nil,
      exclusions = Nil
    )

  private def answer(offer: RawOffer, stop: String = "end_turn"): Response[IO] =
    Response[IO](Status.Ok).withEntity(
      Json
        .obj(
          "stop_reason" -> stop.asJson,
          "content" -> List(
            Json.obj("type" -> "text".asJson, "text" -> offer.asJson.noSpaces.asJson)
          ).asJson
        )
        .noSpaces
    )

  /** Fake Anthropic API: records calls, tracks peak concurrency, answers via `respond`. */
  private def fakeApi(
      calls: Ref[IO, List[Call]],
      running: Ref[IO, Int],
      peak: Ref[IO, Int],
      respond: List[Int] => Response[IO]
  ): Client[IO] =
    Client[IO]((req: Request[IO]) =>
      Resource.eval(
        for
          body <- req.bodyText.compile.string.map(parse(_).getOrElse(Json.Null))
          pages = pageMarker.findAllMatchIn(documentText(body)).map(_.group(1).toInt).toList
          _ <- calls.update(_ :+ Call(pages, kinds(body)))
          n <- running.updateAndGet(_ + 1)
          _ <- peak.update(_.max(n))
          _ <- IO.sleep(50.millis)
          _ <- running.update(_ - 1)
        yield respond(pages)
      )
    )

  private final case class Outcome(
      result: Either[ExtractionError, RawOffer],
      calls: List[Call],
      peak: Int
  )

  private def extract(
      pdf: Array[Byte],
      respond: List[Int] => Response[IO],
      budget: PassBudget = PassBudget.default,
      fileType: OfferFileType = OfferFileType.Pdf
  ): IO[Outcome] =
    for
      calls <- Ref.of[IO, List[Call]](Nil)
      running <- Ref.of[IO, Int](0)
      peak <- Ref.of[IO, Int](0)
      claude = ClaudeOfferExtractor(fakeApi(calls, running, peak, respond), apiKey, "m", prompts)
      bounded <- OfferPass.bounded(claude, permits = 2)
      result <- MultiPassOfferExtractor(bounded, budget).extract(pdf, fileType, Nil)
      seen <- calls.get
      max <- peak.get
    yield Outcome(result, seen, max)

  private val longPages = 12

  /** 12 dense pages (70 positions each, ~8k characters per page). */
  private lazy val longPdf = TestPdfs.offer(longPages, linesPerPage = 70)

  "a 12-page offer exceeds the one-pass budget and is read in page-aware passes" in {
    val pdf = longPdf
    extract(pdf, pages => answer(fragment(pages, longPages))).asserting { out =>
      // (a) more than one pass
      out.calls.size shouldBe 3
      // (b) page-aware: every pass gets whole, consecutive pages; together each page exactly once
      out.calls.map(_.pages).sortBy(_.head) shouldBe List(1 to 4, 5 to 8, 9 to 12).map(_.toList)
      out.calls.map(_.kinds).distinct shouldBe List(List("text", "text"))
      // passes share the existing limit of two concurrent calls
      out.peak shouldBe 2
      val merged = out.result.toOption
      // (c) line items of all pages, in page order
      merged.map(_.lineItems.flatMap(_.description.text)) shouldBe
        Some((1 to longPages).toList.map(p => s"Seite $p"))
      merged.flatMap(_.contractor.text) shouldBe Some("Dachdeckerei Muster GmbH")
      merged.flatMap(_.gross.text) shouldBe Some("119.00")
      merged.flatMap(_.gross.page) shouldBe Some(longPages)
    }
  }

  "the long PDF really is too large for one pass by characters, not only by page count" in {
    val pages = PdfText.pages(longPdf).getOrElse(Nil)
    val chars = pages.map(_.text.length).sum
    val oneByChars = PassBudget.default.copy(maxPagesPerPass = 100)
    val plan = MultiPassOfferExtractor(stubPass, oneByChars).plan(pages, Nil)
    (pages.size, chars > oneByChars.documentChars(stubPass.promptText(Nil)), plan.size > 1) shouldBe
      (longPages, true, true)
  }

  "a short PDF still takes exactly one pass and its answer is used unchanged" in {
    extract(TestPdfs.offer(2), pages => answer(fragment(pages, 2))).asserting { out =>
      out.calls.map(_.pages) shouldBe List(List(1, 2))
      out.result shouldBe Right(fragment(List(1, 2), 2))
    }
  }

  "a tight character budget splits at page boundaries, never inside a page that fits" in {
    val pages = PdfText.pages(TestPdfs.offer(6)).getOrElse(Nil)
    val tight = PassBudget.default.copy(inputTokens = pages.head.text.length) // ~3 pages
    val plan = MultiPassOfferExtractor(stubPass, tight).plan(pages, Nil)
    plan.flatMap(_.pages) shouldBe pages
    plan.map(_.pages.map(_.number)).forall(ns => ns == ns.distinct) shouldBe true
    plan.size should be > 1
  }

  "a page longer than a whole pass is cut into pieces that keep its page number" in {
    val chunks = Chunking.chunks(
      List(PdfPage(1, "kurz\n"), PdfPage(2, "zeile\n" * 100), PdfPage(3, "ende\n")),
      maxChars = 120,
      maxPages = 4
    )
    chunks.flatMap(_.pages).map(_.number).distinct shouldBe List(1, 2, 3)
    chunks.flatMap(_.pages).filter(_.number == 2).map(_.text).mkString shouldBe "zeile\n" * 100
    chunks.forall(_.pages.map(_.text.length).sum <= 120) shouldBe true
  }

  "max_tokens in one pass fails the whole offer and names the part (no page dropped)" in {
    extract(
      longPdf,
      pages =>
        if pages.contains(5) then answer(fragment(pages, longPages), "max_tokens")
        else answer(fragment(pages, longPages))
    ).asserting(
      _.result shouldBe Left(
        ExtractionError.UnexpectedResponse(
          "Antwort abgebrochen (max_tokens) in Teil 2/3 (Seiten 5–8)"
        )
      )
    )
  }

  "a single-pass max_tokens keeps the existing message" in {
    extract(TestPdfs.offer(1), pages => answer(fragment(pages, 1), "max_tokens")).asserting(
      _.result shouldBe Left(ExtractionError.UnexpectedResponse("Antwort abgebrochen (max_tokens)"))
    )
  }

  "a PDF without text layer (scan) is sent once as a document" in {
    extract(TestPdfs.blank(3), _ => answer(RawOffers.dachfenster)).asserting { out =>
      out.calls.map(_.kinds) shouldBe List(List("document", "text"))
      out.result shouldBe Right(RawOffers.dachfenster)
    }
  }

  "images keep the single image pass" in {
    extract(
      Array(0x89, 0x50, 0x4e, 0x47).map(_.toByte),
      _ => answer(RawOffers.dachfenster),
      fileType = OfferFileType.Png
    ).asserting(_.calls.map(_.kinds) shouldBe List(List("image", "text")))
  }

  "bytes that are no readable PDF are reported without calling the API" in {
    extract("%PDF-kaputt".getBytes("UTF-8"), _ => answer(RawOffers.dachfenster)).asserting { out =>
      out.calls shouldBe Nil
      out.result.left.map(_.ordinal) shouldBe Left(ExtractionError.UnreadablePdf("").ordinal)
    }
  }

  "merging keeps the first contractor and records a disagreeing one as a low-confidence note" in {
    val chunks = List(1, 2).map(i => TextChunk(i, 2, List(PdfPage(i, ""))))
    val first = fragment(List(1), 2)
    val second = fragment(List(2), 2).copy(contractor = f("Andere Firma GmbH", 2))
    val merged = RawOfferMerge.merge(chunks.zip(List(first, second)))
    merged.contractor.text shouldBe Some("Dachdeckerei Muster GmbH")
    merged.notes.map(n => (n.text, n.confidence)) shouldBe List(
      (
        Some(
          "Abweichende Angabe Firma in Teil 2/2 (Seite 2): Andere Firma GmbH (verwendet: Dachdeckerei Muster GmbH)"
        ),
        Confidence.Low
      )
    )
  }

  "merging coverage keeps the most specific status per Leistung" in {
    val chunks = List(1, 2).map(i => TextChunk(i, 2, List(PdfPage(i, ""))))
    def cov(status: String) = Some(List(RawAbdeckung("dach", status, 1, Confidence.High)))
    val merged = RawOfferMerge.merge(
      chunks.zip(
        List(
          fragment(List(1), 2).copy(abdeckung = cov("unklar")),
          fragment(List(2), 2).copy(abdeckung = cov("enthalten"))
        )
      )
    )
    merged.abdeckung.toList.flatten.map(_.status) shouldBe List("enthalten")
  }

  "the default budget fits input and output into the model's context window" in {
    val b = PassBudget.default
    (b.inputTokens + b.maxOutputTokens <= b.contextTokens, b.maxOutputTokens) shouldBe (true, 16000)
  }

  "conflicting totals use the last part and keep the other value as a low-confidence note" in {
    val chunks = List(1, 2).map(i => TextChunk(i, 2, List(PdfPage(i, ""))))
    val first = fragment(List(1), 1) // has totals 119.00 on page 1
    val second = fragment(List(2), 2).copy(gross = f("120.00", 2))
    val merged = RawOfferMerge.merge(chunks.zip(List(first, second)))
    merged.gross.text shouldBe Some("120.00")
    merged.notes.flatMap(_.text) should contain(
      "Abweichende Angabe Brutto in Teil 1/2 (Seite 1): 119.00 (verwendet: 120.00)"
    )
    merged.notes.forall(_.confidence == Confidence.Low) shouldBe true
  }

  "a repeat of an item on a page cut between two parts is kept once; equal items on other pages stay" in {
    val split = TextChunk(1, 2, List(PdfPage(1, ""), PdfPage(2, "a")))
    val rest = TextChunk(2, 2, List(PdfPage(2, "b"), PdfPage(3, "")))
    def on(page: Int) = RawOffers
      .item("Dachlatten", "1", "Stk", "1.00", "1.00")
      .copy(description = f("Dachlatten", page))
    val merged = RawOfferMerge.merge(
      List(
        split -> fragment(Nil, 9).copy(lineItems = List(on(1), on(2))),
        rest -> fragment(Nil, 9).copy(lineItems = List(on(2), on(3)))
      )
    )
    merged.lineItems.map(_.description.page) shouldBe List(Some(1), Some(2), Some(3))
  }

  "cancelling a running multi-pass extraction releases the permits" in {
    for
      running <- Ref.of[IO, Int](0)
      hanging = new OfferPass[IO]:
        def run(content: PassContent, known: List[builder.domain.Leistung]) =
          content match
            case PassContent.Text(c) if c.total > 1 =>
              (running.update(_ + 1) >> IO.never).onCancel(running.update(_ - 1))
            case PassContent.Text(_) | PassContent.Image(_, _) | PassContent.ScannedPdf(_) =>
              IO.pure(Right(RawOffers.dachfenster))
        def promptText(known: List[builder.domain.Leistung]) = "p"
      bounded <- OfferPass.bounded(hanging, permits = 2)
      extractor = MultiPassOfferExtractor(bounded)
      fiber <- extractor.extract(longPdf, OfferFileType.Pdf, Nil).start
      _ <- running.get.iterateUntil(_ == 2)
      _ <- fiber.cancel
      left <- running.get
      after <- extractor.extract(TestPdfs.offer(1), OfferFileType.Pdf, Nil).timeout(5.seconds)
    yield (left, after) shouldBe (0, Right(RawOffers.dachfenster))
  }

  private lazy val stubPass: OfferPass[IO] = new OfferPass[IO]:
    def run(content: PassContent, known: List[builder.domain.Leistung]) =
      IO.pure(Right(RawOffers.dachfenster))
    def promptText(known: List[builder.domain.Leistung]) = "Lies das Angebot."

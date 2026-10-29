package builder

import builder.extraction.*
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import io.circe.Json
import io.circe.syntax.*
import org.http4s.client.Client
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class ClaudeOfferExtractorSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers:
  private val pdf = "%PDF-1.7".getBytes("UTF-8")

  private def contentTypes(fileType: OfferFileType): List[String] =
    ClaudeOfferExtractor
      .requestBody("claude-sonnet-5-5", pdf, fileType)
      .hcursor
      .downField("messages")
      .downArray
      .downField("content")
      .as[List[Json]]
      .getOrElse(Nil)
      .flatMap(_.hcursor.downField("type").as[String].toOption)

  "a PDF is sent as a document block, images as image blocks" in {
    contentTypes(OfferFileType.Pdf) shouldBe List("document", "text")
    contentTypes(OfferFileType.Png) shouldBe List("image", "text")
    contentTypes(OfferFileType.Jpeg) shouldBe List("image", "text")
  }

  "the structured JSON answer of a Messages API response is read as the offer" in {
    val body = Json.obj(
      "stop_reason" -> "end_turn".asJson,
      "content" -> List(
        Json.obj("type" -> "thinking".asJson, "thinking" -> "…".asJson),
        Json.obj("type" -> "text".asJson, "text" -> RawOffers.dachfenster.asJson.noSpaces.asJson)
      ).asJson
    )
    ClaudeOfferExtractor.parseResponse(body.noSpaces) shouldBe Right(RawOffers.dachfenster)
  }

  "a truncated answer is an UnexpectedResponse, not a half-read offer" in {
    ClaudeOfferExtractor.parseResponse(
      """{"stop_reason":"max_tokens","content":[{"type":"text","text":"{\"contractor\""}]}"""
    ) shouldBe Left(ExtractionError.UnexpectedResponse("Antwort abgebrochen (max_tokens)"))
  }

  "empty values and page 0 mean 'not in the document'" in {
    val absent = """{"value":"","page":0,"confidence":"high"}"""
    io.circe.parser.decode[Field](absent).map(f => (f.text, f.pageNumber)) shouldBe
      Right((None, None))
  }

  "without an API key nothing is sent" in {
    val noNetwork =
      Client[IO](_ => cats.effect.Resource.eval(IO.raiseError(IllegalStateException("network"))))
    ClaudeOfferExtractor(noNetwork, None, "m")
      .extract(pdf, OfferFileType.Pdf)
      .asserting(_ shouldBe Left(ExtractionError.MissingApiKey))
  }

  "file types are recognised by content, not by name" in {
    OfferFileType.detect(pdf) shouldBe Some(OfferFileType.Pdf)
    OfferFileType.detect(Array(0xff, 0xd8, 0xff, 0xe0).map(_.toByte)) shouldBe Some(
      OfferFileType.Jpeg
    )
    OfferFileType.detect("GIF89a".getBytes("UTF-8")) shouldBe None
  }

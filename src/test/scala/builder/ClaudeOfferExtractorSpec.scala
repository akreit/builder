package builder

import builder.extraction.*
import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.testkit.TestControl
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.Header
import org.http4s.Response
import org.http4s.Status
import org.http4s.client.Client
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.ci.CIString

import java.io.IOException
import scala.concurrent.duration.*

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

  private val testKey = ApiKey.from("test-key")

  private val success: Response[IO] =
    Response[IO](Status.Ok).withEntity(
      Json
        .obj(
          "stop_reason" -> "end_turn".asJson,
          "content" -> List(
            Json
              .obj("type" -> "text".asJson, "text" -> RawOffers.dachfenster.asJson.noSpaces.asJson)
          ).asJson
        )
        .noSpaces
    )

  private def apiError(status: Status, errorType: String): Response[IO] =
    Response[IO](status).withEntity(
      s"""{"type":"error","error":{"type":"$errorType","message":"geheime Details"}}"""
    )

  /** A client answering the n-th call with `answers(n)` (the last one repeats). */
  private def scripted(
      calls: Ref[IO, Int],
      answers: List[IO[Response[IO]]]
  ): Client[IO] =
    Client[IO](_ =>
      Resource.eval(
        calls.getAndUpdate(_ + 1).flatMap(n => answers.lift(n).getOrElse(answers.last))
      )
    )

  /** Runs one extraction in virtual time; returns result, number of HTTP calls, elapsed time. */
  private def run(
      answers: List[IO[Response[IO]]]
  ): IO[(Either[ExtractionError, RawOffer], Int, FiniteDuration)] =
    TestControl.executeEmbed(
      for
        calls <- Ref.of[IO, Int](0)
        start <- IO.monotonic
        result <- ClaudeOfferExtractor(scripted(calls, answers), testKey, "m")
          .extract(pdf, OfferFileType.Pdf)
        end <- IO.monotonic
        n <- calls.get
      yield (result, n, end - start)
    )

  "overloaded (529) and server errors are retried with backoff until the answer arrives" in {
    run(
      List(
        IO.pure(
          apiError(Status.fromInt(529).getOrElse(Status.ServiceUnavailable), "overloaded_error")
        ),
        IO.pure(apiError(Status.InternalServerError, "api_error")),
        IO.pure(success)
      )
    ).asserting { (result, calls, elapsed) =>
      result shouldBe Right(RawOffers.dachfenster)
      calls shouldBe 3
      // 1st retry waits 1–2 s, 2nd 2–4 s (base 2 s, equal jitter)
      elapsed should (be >= 3.seconds and be <= 6.seconds)
    }
  }

  "a rate limit (429) waits as long as retry-after asks" in {
    run(
      List(
        IO.pure(
          apiError(Status.TooManyRequests, "rate_limit_error")
            .putHeaders(Header.Raw(CIString("retry-after"), "30"))
        ),
        IO.pure(success)
      )
    ).asserting { (result, calls, elapsed) =>
      result shouldBe Right(RawOffers.dachfenster)
      calls shouldBe 2
      elapsed should be >= 30.seconds
    }
  }

  "a validation error (400) is not retried and only status and error type are kept" in {
    run(List(IO.pure(apiError(Status.BadRequest, "invalid_request_error")))).asserting {
      (result, calls, _) =>
        result shouldBe Left(ExtractionError.HttpStatus(400, Some("invalid_request_error")))
        calls shouldBe 1
    }
  }

  "server errors that persist give up after 3 retries with the last status" in {
    run(List(IO.pure(apiError(Status.ServiceUnavailable, "overloaded_error")))).asserting {
      (result, calls, _) =>
        result shouldBe Left(ExtractionError.HttpStatus(503, Some("overloaded_error")))
        calls shouldBe 4
    }
  }

  "connection errors are retried, then reported as a transport error without details" in {
    run(List(IO.raiseError(IOException("connection reset by 10.0.0.1")))).asserting {
      (result, calls, _) =>
        result shouldBe Left(ExtractionError.Transport("IOException"))
        calls shouldBe 4
    }
  }

  "at most two extractions run at the same time" in {
    TestControl
      .executeEmbed(
        for
          running <- Ref.of[IO, Int](0)
          peak <- Ref.of[IO, Int](0)
          slow = new OfferExtractor[IO]:
            def extract(file: Array[Byte], fileType: OfferFileType) =
              running
                .updateAndGet(_ + 1)
                .flatMap(n => peak.update(_.max(n))) >> IO.sleep(1.second) >>
                running.update(_ - 1).as(Right(RawOffers.dachfenster))
          bounded <- OfferExtractor.bounded(slow, permits = 2)
          results <- List.fill(5)(bounded.extract(pdf, OfferFileType.Pdf)).parSequence
          max <- peak.get
        yield (results.size, max)
      )
      .asserting(_ shouldBe (5, 2))
  }

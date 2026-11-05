package builder

import builder.extraction.ExtractionPrompts
import builder.extraction.PromptLoadError
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/** Verifies packaged prompt bytes and typed loading failures without provider calls. */
class ExtractionPromptsSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers:
  private def sha256(text: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(text.getBytes(StandardCharsets.UTF_8))
      .map(b => f"${b & 0xff}%02x")
      .mkString

  "classpath prompts equal the original embedded UTF-8 text byte for byte" in {
    ExtractionPrompts.load.flatMap(IO.fromEither).asserting { prompts =>
      prompts.offerExtraction should not be empty
      sha256(
        prompts.offerExtraction
      ) shouldBe "e96a6df2f8c31cbb435c2e7b7dc0abe87592b223b464a8433f1942fc7815e527"
      sha256(
        prompts.knownEmpty
      ) shouldBe "06294b3668af785f0644006446960a1ac142addbefbc14384b9b504c132d17bf"
      sha256(
        prompts.knownHeading
      ) shouldBe "a161191b32d860ddda69b8ba9b49ea398b262fdc02a7d46ac43ce0828de6105c"
      sha256(
        prompts.documentPart
      ) shouldBe "3b1877b4cb858785cfdc7dd76b01d834d437ab206a88bef2b427d9c4c2a95bd2"
      prompts.knownText(Nil) shouldBe prompts.knownEmpty
    }
  }

  "a missing classpath resource produces a typed error" in {
    ExtractionPrompts
      .loadText("prompts/missing.md")
      .asserting(
        _ shouldBe Left(PromptLoadError.Missing("prompts/missing.md"))
      )
  }

  "an unreadable UTF-8 resource produces a typed error" in {
    ExtractionPrompts
      .loadText("prompts/invalid-utf8.md")
      .asserting(
        _ shouldBe Left(
          PromptLoadError.Unreadable("prompts/invalid-utf8.md", "MalformedInputException")
        )
      )
  }

  "startup errors identify the resource and unreadable detail in their log message" in {
    PromptLoadError.Missing("prompts/missing.md").getMessage shouldBe
      "Missing prompt resource: prompts/missing.md"
    PromptLoadError
      .Unreadable("prompts/invalid-utf8.md", "MalformedInputException")
      .getMessage shouldBe
      "Unreadable prompt resource: prompts/invalid-utf8.md (MalformedInputException)"
  }

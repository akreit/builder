package builder.extraction

import builder.domain.Leistung
import cats.data.EitherT
import cats.effect.IO
import cats.effect.Resource

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

/** Startup failures while locating or reading a packaged prompt. */
enum PromptLoadError extends Exception:
  case Missing(path: String)
  case Unreadable(path: String, detail: String)

  override def getMessage: String = this match
    case Missing(path)            => s"Missing prompt resource: $path"
    case Unreadable(path, detail) => s"Unreadable prompt resource: $path ($detail)"

/** Immutable prompt text loaded once when wiring the extractor. */
final case class ExtractionPrompts(
    offerExtraction: String,
    knownEmpty: String,
    knownHeading: String,
    documentPart: String
):
  /** Header of a text pass: where the text comes from and how page markers work. */
  def documentPartText(chunk: TextChunk): String = documentPart.replace("{label}", chunk.label)

  def knownText(known: List[Leistung]): String =
    if known.isEmpty then knownEmpty
    else knownHeading + known.map(l => s"- ${l.id.value}: ${l.name.value}").mkString("\n")

/** UTF-8 classpath loading with typed failures and scoped stream ownership. */
object ExtractionPrompts:
  def load: IO[Either[PromptLoadError, ExtractionPrompts]] =
    (for
      offer <- EitherT(loadText("prompts/offer-extraction.md"))
      empty <- EitherT(loadText("prompts/known-leistungen-empty.md"))
      heading <- EitherT(loadText("prompts/known-leistungen-heading.md"))
      part <- EitherT(loadText("prompts/document-part.md"))
    yield ExtractionPrompts(offer, empty, heading, part)).value

  def loadText(path: String): IO[Either[PromptLoadError, String]] =
    IO.blocking(Option(getClass.getClassLoader.getResource(path)))
      .flatMap {
        case None      => IO.pure(Left(PromptLoadError.Missing(path)))
        case Some(url) =>
          Resource
            .fromAutoCloseable(IO.blocking(url.openStream()))
            .use(stream =>
              IO.blocking(
                StandardCharsets.UTF_8
                  .newDecoder()
                  .decode(ByteBuffer.wrap(stream.readAllBytes()))
                  .toString
              )
            )
            .map(Right(_))
      }
      .attempt
      .map {
        case Right(result) => result
        case Left(error)   => Left(PromptLoadError.Unreadable(path, error.getClass.getSimpleName))
      }

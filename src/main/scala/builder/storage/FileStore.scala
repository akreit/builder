package builder.storage

import builder.domain.OfferId
import builder.extraction.OfferFileType
import cats.effect.IO

import java.nio.file.Files
import java.nio.file.Path

/** Uploaded offer files on local disk, one file per offer: `<dir>/<offerId>.<ext>`. */
final class FileStore(dir: Path):
  def save(id: OfferId, fileType: OfferFileType, bytes: Array[Byte]): IO[Path] =
    IO.blocking {
      Files.createDirectories(dir)
      Files.write(dir.resolve(s"${id.value}.${fileType.extension}"), bytes)
    }

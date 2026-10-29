package builder.storage

import builder.domain.OfferId
import builder.extraction.OfferFileType
import cats.effect.IO

import java.nio.file.Files
import java.nio.file.Path

/** Uploaded offer files on local disk, one file per offer: `<dir>/<offerId>.<ext>`. */
final class FileStore(dir: Path):
  private def path(id: OfferId, fileType: OfferFileType): Path =
    dir.resolve(s"${id.value}.${fileType.extension}")

  def save(id: OfferId, fileType: OfferFileType, bytes: Array[Byte]): IO[Path] =
    IO.blocking {
      Files.createDirectories(dir)
      Files.write(path(id, fileType), bytes)
    }

  /** Removes the file of an upload that was not stored; a missing file is fine. */
  def delete(id: OfferId, fileType: OfferFileType): IO[Unit] =
    IO.blocking(Files.deleteIfExists(path(id, fileType))).void

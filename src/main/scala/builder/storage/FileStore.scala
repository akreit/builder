package builder.storage

import builder.domain.OfferId
import builder.extraction.OfferFileType
import cats.effect.IO
import cats.syntax.all.*

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption

/** Uploaded offer files on local disk, one file per offer: `<dir>/<offerId>.<ext>`.
  *
  * Writes go to a temp file first ([[stage]]); after the DB row is committed, [[commit]] moves that
  * temp onto the live path atomically so a cancel or error never truncates an existing offer file.
  */
final class FileStore(dir: Path):
  private def path(id: OfferId, fileType: OfferFileType): Path =
    dir.resolve(s"${id.value}.${fileType.extension}")

  /** Writes `bytes` under `dir` as a new temp file. Caller must [[commit]] or [[discard]] it. */
  def stage(bytes: Array[Byte]): IO[Path] =
    IO.blocking {
      Files.createDirectories(dir)
      val tmp = Files.createTempFile(dir, "upload-", ".part")
      Files.write(tmp, bytes)
      tmp
    }

  /** Atomically replaces the offer's live path with the staged file (same directory). */
  def commit(staged: Path, id: OfferId, fileType: OfferFileType): IO[Path] =
    IO.blocking {
      val target = path(id, fileType)
      Files.createDirectories(dir)
      Files.move(
        staged,
        target,
        StandardCopyOption.ATOMIC_MOVE,
        StandardCopyOption.REPLACE_EXISTING
      )
    }

  /** Removes a staged file that was not committed; a missing file is fine. */
  def discard(staged: Path): IO[Unit] =
    IO.blocking(Files.deleteIfExists(staged)).void

  /** Removes the file of an upload that was not stored; a missing file is fine. */
  def delete(id: OfferId, fileType: OfferFileType): IO[Unit] =
    IO.blocking(Files.deleteIfExists(path(id, fileType))).void

  /** Removes every stored extension of `id` (e.g. after a replace with a different file type). */
  def deleteAll(id: OfferId): IO[Unit] =
    OfferFileType.values.toList.traverse_(ft => delete(id, ft))

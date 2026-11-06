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
  * Writes go to a temp file first ([[stage]]); before the DB transaction, [[install]] moves it onto
  * the live path atomically (backing up the old file); [[rollback]] undoes that when the
  * transaction fails, [[finish]] drops the backup after it committed.
  */
final class FileStore(dir: Path):
  private def path(id: OfferId, fileType: OfferFileType): Path =
    dir.resolve(s"${id.value}.${fileType.extension}")

  /** Writes `bytes` under `dir` as a new temp file. Caller must [[install]] or [[discard]] it. */
  def stage(bytes: Array[Byte]): IO[Path] =
    IO.blocking {
      Files.createDirectories(dir)
      val tmp = Files.createTempFile(dir, "upload-", ".part")
      Files.write(tmp, bytes)
      tmp
    }

  /** Puts the staged file onto the offer's live path before the DB commit. Any existing file of
    * `id` (every extension) is first moved aside as a backup, so [[rollback]] can restore it and
    * [[finish]] drops it. On a failure midway the backups taken so far are restored.
    */
  def install(staged: Path, id: OfferId, fileType: OfferFileType): IO[FileStore.Installed] =
    IO.uncancelable(_ =>
      IO.blocking(Files.createDirectories(dir)) >>
        aside(OfferFileType.values.toList.map(path(id, _)), Nil).flatMap(backups =>
          IO.blocking(
            Files.move(
              staged,
              path(id, fileType),
              StandardCopyOption.ATOMIC_MOVE,
              StandardCopyOption.REPLACE_EXISTING
            )
          ).attempt
            .flatMap {
              case Right(live) => IO.pure(FileStore.Installed(live, backups))
              case Left(e)     => restore(backups) >> IO.raiseError(e)
            }
        )
    )

  /** Undoes [[install]]: removes the new live file and puts the backups back. */
  def rollback(installed: FileStore.Installed): IO[Unit] =
    IO.uncancelable(_ =>
      IO.blocking(Files.deleteIfExists(installed.live)) >> restore(installed.backups)
    )

  /** After the DB commit: the backups of [[install]] are no longer needed. */
  def finish(installed: FileStore.Installed): IO[Unit] =
    installed.backups.traverse_(b => IO.blocking(Files.deleteIfExists(b.backup)).void)

  private def aside(rest: List[Path], done: List[FileStore.Backup]): IO[List[FileStore.Backup]] =
    rest match
      case Nil          => IO.pure(done)
      case live :: tail =>
        moveAside(live).attempt.flatMap {
          case Right(b) => aside(tail, b.toList ++ done)
          case Left(e)  => restore(done) >> IO.raiseError(e)
        }

  private def moveAside(live: Path): IO[Option[FileStore.Backup]] =
    IO.blocking(Files.exists(live))
      .flatMap(exists =>
        if !exists then IO.pure(None)
        else
          IO.randomUUID.flatMap(uuid =>
            val backup = dir.resolve(s".${live.getFileName}.bak-$uuid")
            IO.blocking(Files.move(live, backup, StandardCopyOption.ATOMIC_MOVE))
              .as(Some(FileStore.Backup(backup, live)))
          )
      )

  private def restore(backups: List[FileStore.Backup]): IO[Unit] =
    backups.traverse_(b =>
      IO.blocking(
        Files.move(
          b.backup,
          b.live,
          StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING
        )
      ).void
    )

  /** Removes a staged file that was not committed; a missing file is fine. */
  def discard(staged: Path): IO[Unit] =
    IO.blocking(Files.deleteIfExists(staged)).void

  /** Removes the file of an upload that was not stored; a missing file is fine. */
  def delete(id: OfferId, fileType: OfferFileType): IO[Unit] =
    IO.blocking(Files.deleteIfExists(path(id, fileType))).void

  /** Removes every stored extension of `id` (e.g. after a replace with a different file type). */
  def deleteAll(id: OfferId): IO[Unit] =
    OfferFileType.values.toList.traverse_(ft => delete(id, ft))

/** Path layout and factory for the on-disk [[FileStore]]. */
object FileStore:
  /** An existing live file moved aside by [[FileStore.install]]. */
  final case class Backup(backup: Path, live: Path)

  /** Result of [[FileStore.install]]: the new live file and the backups it replaced. */
  final case class Installed(live: Path, backups: List[Backup])

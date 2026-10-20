package builder.repo

import builder.domain.*
import cats.effect.IO
import cats.effect.Ref

enum RepoError:
  case ProjectNotFound(id: ProjectId)
  case Invalid(error: DomainError)

trait ProjectRepository:
  def find(id: ProjectId): IO[Option[Project]]

  /** Marks `offer` as the favorite of `category` and returns the updated project. */
  def setFavorite(
      id: ProjectId,
      category: CategoryId,
      offer: OfferId
  ): IO[Either[RepoError, Project]]

/** In-memory repository seeded at startup; state lives in a [[Ref]] and is lost on restart. */
final class InMemoryProjectRepository private (state: Ref[IO, Map[ProjectId, Project]])
    extends ProjectRepository:
  def find(id: ProjectId): IO[Option[Project]] = state.get.map(_.lift(id))

  def setFavorite(
      id: ProjectId,
      category: CategoryId,
      offer: OfferId
  ): IO[Either[RepoError, Project]] =
    state.modify { projects =>
      val updated = projects
        .lift(id)
        .toRight(RepoError.ProjectNotFound(id))
        .flatMap(_.withFavorite(category, offer).left.map(RepoError.Invalid(_)))
      updated.fold(_ => projects, p => projects.updated(id, p)) -> updated
    }

object InMemoryProjectRepository:
  def of(projects: List[Project]): IO[InMemoryProjectRepository] =
    Ref
      .of[IO, Map[ProjectId, Project]](projects.map(p => p.id -> p).toMap)
      .map(InMemoryProjectRepository(_))

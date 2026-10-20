package builder.repo

import builder.domain.*
import cats.effect.IO

trait ProjectRepository:
  def find(id: ProjectId): IO[Option[Project]]

/** Read-only in-memory repository seeded at startup. */
final class InMemoryProjectRepository(projects: Map[ProjectId, Project]) extends ProjectRepository:
  def find(id: ProjectId): IO[Option[Project]] = IO.pure(projects.lift(id))

object InMemoryProjectRepository:
  def of(projects: List[Project]): InMemoryProjectRepository =
    InMemoryProjectRepository(projects.map(p => p.id -> p).toMap)

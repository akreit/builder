package builder

import builder.domain.*
import cats.syntax.all.*

/** Runtime home project shell: configurable display name and structural trade categories, never
  * demo offers or example prices. Categories live in the in-memory seed so
  * [[builder.repo.PostgresProjectRepository.withCategories]] can merge renames, soft-deletes and
  * user-added categories from the database without rewriting them on every start.
  */
object DefaultProject:
  /** Environment variable for the home project display name. */
  val NameEnv: String = "BUILDER_PROJECT_NAME"

  /** Fallback when [[NameEnv]] is unset or blank. */
  val FallbackName: String = "Mein Projekt"

  /** Structural trade categories in UI order (ids match historical seed ids). */
  val StructuralCategories: List[(String, String)] = List(
    "dach" -> "Dach",
    "fenster" -> "Fenster",
    "fassadendaemmung" -> "Fassadendämmung",
    "elektrik" -> "Elektrik",
    "heizung" -> "Heizung",
    "innenausbau" -> "Innenausbau"
  )

  /** Display name from `BUILDER_PROJECT_NAME`, or [[FallbackName]] when missing or blank. */
  def nameFromEnv(env: Map[String, String]): String =
    env.get(NameEnv).map(_.trim).filter(_.nonEmpty).getOrElse(FallbackName)

  /** Structural trade categories of the empty home-project shell, in tab order. */
  def categories: Either[DomainError, List[Category]] =
    StructuralCategories.traverse((id, name) =>
      (CategoryId.from(id), CategoryName.from(name)).mapN(Category.apply)
    )

  /** Home project `id=default`: configured name, EUR, structural categories, no offers. */
  def shell(env: Map[String, String]): Either[DomainError, Project] =
    for
      id <- ProjectId.from("default")
      cats <- categories
      project <- Project.from(id, nameFromEnv(env), Currency.EUR, cats, Nil, Nil, Map.empty)
    yield project

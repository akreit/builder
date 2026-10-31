package builder

import builder.repo.DbConfig
import builder.repo.DbConfigError
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DbConfigSpec extends AnyFreeSpec with Matchers:
  "DATABASE_URL takes precedence and is split into its parts" in {
    DbConfig.fromEnv(
      Map(
        "DATABASE_URL" -> "postgres://builder:geheim@db:6543/builder",
        "DATABASE_HOST" -> "ignored"
      )
    ) shouldBe Right(DbConfig("db", 6543, "builder", Some("geheim"), "builder"))
  }

  "single variables work, the port defaults to 5432" in {
    DbConfig.fromEnv(
      Map(
        "DATABASE_HOST" -> "localhost",
        "DATABASE_USER" -> "builder",
        "DATABASE_NAME" -> "builder"
      )
    ) shouldBe Right(DbConfig("localhost", 5432, "builder", None, "builder"))
  }

  "a missing variable or a bad port is reported by name" in {
    DbConfig.fromEnv(Map("DATABASE_HOST" -> "h", "DATABASE_USER" -> "u")) shouldBe
      Left(DbConfigError.Missing("DATABASE_NAME"))
    DbConfig
      .fromEnv(Map("DATABASE_URL" -> "mysql://u:p@h/db"))
      .isLeft shouldBe true
  }

  "the password never appears in the printed configuration" in {
    DbConfig("db", 5432, "builder", Some("geheim"), "builder").toString should not include "geheim"
  }

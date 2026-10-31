ThisBuild / scalaVersion := "3.10.0"
ThisBuild / semanticdbEnabled := true
ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature", "-Wunused:all", "-Werror")

val catsEffectV = "3.7.1"
val fs2V = "3.14.0"
val http4sV = "0.23.38"
val tapirV = "1.14.0"
val circeV = "0.14.17"
val scalatestV = "3.2.20"
val laikaV = "1.3.2"
val scalatagsV = "0.13.1"

lazy val root = (project in file("."))
  .settings(
    name := "builder",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % catsEffectV,
      "co.fs2" %% "fs2-core" % fs2V,
      "org.http4s" %% "http4s-ember-server" % http4sV,
      "org.http4s" %% "http4s-ember-client" % http4sV,
      "org.xerial" % "sqlite-jdbc" % "3.50.3.0",
      "io.circe" %% "circe-core" % circeV,
      "io.circe" %% "circe-generic" % circeV,
      "com.softwaremill.sttp.tapir" %% "tapir-core" % tapirV,
      "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % tapirV,
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirV,
      "com.softwaremill.sttp.tapir" %% "tapir-files" % tapirV,
      "com.lihaoyi" %% "scalatags" % scalatagsV,
      "io.circe" %% "circe-parser" % circeV,
      "org.scalatest" %% "scalatest" % scalatestV % Test,
      "org.typelevel" %% "cats-effect-testkit" % catsEffectV % Test,
      "org.typelevel" %% "cats-effect-testing-scalatest" % "1.8.0" % Test,
      "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test
    ),
    Compile / run / fork := true,
    publish / skip := true
  )

lazy val docs = (project in file("builder-docs"))
  .enablePlugins(MdocPlugin)
  .dependsOn(root)
  .settings(
    name := "builder-docs",
    mdocIn := (ThisBuild / baseDirectory).value / "docs",
    mdocOut := (ThisBuild / baseDirectory).value / "target" / "docs" / "mdoc",
    libraryDependencies += "org.typelevel" %% "laika-io" % laikaV,
    Compile / run / fork := true,
    Compile / run / baseDirectory := (ThisBuild / baseDirectory).value,
    publish / skip := true
  )

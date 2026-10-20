ThisBuild / scalaVersion := "3.10.0"
ThisBuild / semanticdbEnabled := true
ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature", "-Wunused:all")

lazy val root = (project in file("."))
  .settings(name := "builder")

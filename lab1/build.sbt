val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Bot-tender",
    version := "0.2.0",
    scalaVersion := scala3Version,
  )

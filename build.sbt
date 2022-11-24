val scala3Version = "3.2.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "TP2-UnB",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
	libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.7"
  )

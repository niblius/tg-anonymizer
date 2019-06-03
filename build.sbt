val http4sVersion = "0.20.1"
val kindProjectorVersion = "0.10.1"
val circeVersion = "0.11.1"
val catsVersion = "2.0.0-M1"
val fs2Version = "1.0.4"
val loggerVersion =  "0.3.0"
val slf4jVersion = "1.7.25"
val circeConfigVersion = "0.6.1"
val reactiveMongoVersion = "0.17.0"
val flywayVersion = "5.2.4"
val doobieVersion = "0.7.0"

organization := "org.niblius"
name := "tg-anonymizer"
version := "0.0.1-SNAPSHOT"
scalaVersion := "2.12.8"
scalacOptions += "-Ypartial-unification"
libraryDependencies ++= Seq(
  compilerPlugin("org.typelevel" %% "kind-projector" % kindProjectorVersion),
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-literal" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "io.circe" %% "circe-java8" % circeVersion,
  "io.circe" %% "circe-config" % circeConfigVersion,
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsVersion,
  "co.fs2" %% "fs2-core" % fs2Version,
  "io.chrisdavenport" %% "log4cats-core" % loggerVersion,
  "io.chrisdavenport" %% "log4cats-slf4j" % loggerVersion,
  "org.slf4j" % "slf4j-simple" % slf4jVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.flywaydb" % "flyway-core" % flywayVersion,
  "org.tpolecat" %% "doobie-core" % doobieVersion,
  "org.tpolecat" %% "doobie-h2" % doobieVersion,
  "org.tpolecat" %% "doobie-scalatest" % doobieVersion,
  "org.tpolecat" %% "doobie-hikari" % doobieVersion,
)

enablePlugins(ScalafmtPlugin)
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
name := "threeCardPoker"

version := "1.0.0"

scalaVersion := "2.13.10"

val catsVersion         = "2.9.0"
val http4sVersion       = "0.23.18"
val catsEffectVersion   = "3.4.8"
val scalaTestVersion    = "3.2.15"
val circeVersion        = "0.14.5"
val loggerVersion       = "2.0.5"
val scalaLoggingVersion = "3.9.5"
val mockitoVersion      = "3.2.10.0"

libraryDependencies ++= Seq(
  "org.http4s"                 %% "http4s-dsl"             % http4sVersion,
  "org.http4s"                 %% "http4s-ember-server"    % http4sVersion,
  "org.http4s"                 %% "http4s-ember-client"    % http4sVersion,
  "org.http4s"                 %% "http4s-jdk-http-client" % "0.9.0",
  "org.scalatest"              %% "scalatest"              % scalaTestVersion % Test,
  "org.scalatestplus"          %% "mockito-3-4"            % mockitoVersion % Test,
  "io.circe"                   %% "circe-generic"          % circeVersion,
  "io.circe"                   %% "circe-parser"           % circeVersion,
  "org.slf4j"                  % "slf4j-simple"            % loggerVersion,
  "com.typesafe.scala-logging" %% "scala-logging"          % scalaLoggingVersion
)

addCommandAlias("fmt", "scalafmtSbt; scalafmtAll;")

addCommandAlias("compileAllFmt", "test:compile; fmt;")

name := "threeCardPoker"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.13.10"

val catsVersion = "2.9.0"
val http4sVersion = "0.23.18"
val catsEffectVersion = "3.4.8"
val scalaTestVersion = "3.2.15"
val circeVersion = "0.14.5"

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-ember-server" % http4sVersion,
  "org.http4s" %% "http4s-ember-client" % http4sVersion,
  "org.http4s" %% "http4s-jdk-http-client" % "0.9.0",
  "org.slf4j" % "slf4j-nop" % "2.0.5",
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
)
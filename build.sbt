name := "wv-tt_2"

version := "0.02"

scalaVersion := "2.12.7"

lazy val scalatestVersion = "3.0.5"
lazy val scalaLoggingVersion = "3.9.0"
lazy val logbackClassicVersion= "1.2.3"
lazy val catsCoreVersion= "1.5.0"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",   // source files are in UTF-8
  "-deprecation",         // warn about use of deprecated APIs
  "-unchecked",           // warn about unchecked type parameters
  "-feature",             // warn about misused language features
  "-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  "-Xlint",               // enable handy linter warnings
  "-Xfatal-warnings",     // turn compiler warnings into errors
  "-Ypartial-unification",// allow the compiler to unify type constructors of different arities
  "-target:jvm-1.8"
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsCoreVersion,
  "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
  "ch.qos.logback" % "logback-classic" % logbackClassicVersion,
  "org.scalatest" %% "scalatest" % scalatestVersion % "test"
)

//coverageEnabled := true

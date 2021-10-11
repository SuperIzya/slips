ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.0.2"

val zioVersion = "1.0.12"
lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-Ysafe-init"
  ),
  libraryDependencies ++= Seq(
    "org.scala-lang" %% "scala3-staging" % scalaVersion.value
  )
)

lazy val core = (project in file("core")).settings(
  commonSettings,
  scalacOptions ++= Seq("-Xcheck-macros"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.10" % Test
  )
)

lazy val slipsZio = (project in file("slips-zio")).settings (
  commonSettings,
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio" % zioVersion,
    "dev.zio" %% "zio-test" % zioVersion % Test
  ),
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
)


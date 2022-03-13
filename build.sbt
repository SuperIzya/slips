ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.1"
lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-Ysafe-init"
  ),
  libraryDependencies ++= Seq(
    "org.scala-lang" %% "scala3-staging" % scalaVersion.value
  )
)
lazy val root = (project in file("."))
  .settings(
    name := "slips3",
    commonSettings,
    scalacOptions ++= Seq("-Xcheck-macros"),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.10" % Test
    )
  )

import Dependencies.*
import sbt.*

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.8.4"

lazy val root = (project in file(".")).settings(
  name := "slips",
  scalacOptions ++= Seq(
//    "-Vprofile",
    //   "-Yprofile-enabled",
    // "--verbose",
    "-Werror:false",
    "-explain",
    "-feature",
    "-Yprint-debug",
    "-Xcheck-macros",
    "-deprecation",
    "-source:future",
    "-language:strictEquality",
    "-no-indent",
    "-Wsafe-init",
    "-Yshow-suppressed-errors",
    "--rewrite"
  ),
  libraryDependencies ++= libraries,
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
)

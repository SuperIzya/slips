ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val Version = new {
  lazy val cats       = "2.7.0"
  lazy val tests      = "3.2.12"
  lazy val discipline = "2.1.5"
  lazy val shapeless  = "3.1.0"
  lazy val zio = "2.0.0"

}

lazy val root = (project in file(".")).settings(
  name := "slips4",
  scalacOptions ++= Seq(
//    "-explain",
//    "-Xprint-suspension",
    "-Yprint-debug",
    "-print-lines",
    "-Xcheck-macros",
    "-deprecation"
    //   "-Yshow-suppressed-errors"
  ),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core"             % Version.cats,
    "org.typelevel"  % "shapeless3-deriving_3" % Version.shapeless,
    "org.scalatest" %% "scalatest"             % Version.tests      % Test,
    "org.typelevel" %% "discipline-scalatest"  % Version.discipline % Test,
    "dev.zio" %% "zio-test" % Version.zio % Test,
    "dev.zio" %% "zio-test-sbt"      % Version.zio % Test,
  ),
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
)

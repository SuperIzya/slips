ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val Version = new {
  lazy val cats       = "2.10.0"
  lazy val tests      = "3.2.15"
  lazy val magnolia   = "1.3.7"
  lazy val discipline = "2.3.0"
  lazy val zio        = "2.1.1"
}

lazy val root = (project in file(".")).settings(
  name := "slips",
  scalacOptions ++= Seq(
//    "-Vprofile",
    "-explain",
    "-Xprint-suspension",
    "-Yprint-debug",
    "-print-lines",
    "-Xcheck-macros",
    "-deprecation",
    "-source:future"
    //   "-Yshow-suppressed-errors"
  ),
  libraryDependencies ++= Seq(
    "org.typelevel"                %% "cats-core"            % Version.cats,
    "com.softwaremill.magnolia1_3" %% "magnolia"             % Version.magnolia,
    "org.scalatest"                %% "scalatest"            % Version.tests      % Test,
    "org.typelevel"                %% "discipline-scalatest" % Version.discipline % Test,
    "dev.zio"                      %% "zio-test"             % Version.zio        % Test,
    "dev.zio"                      %% "zio-test-sbt"         % Version.zio        % Test
  ),
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
)

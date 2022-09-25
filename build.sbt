ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

lazy val Version = new {
  lazy val cats       = "2.8.0"
  lazy val tests      = "3.2.12"
  lazy val magnolia   = "1.2.0"
  lazy val discipline = "2.2.0"
  lazy val shapeless  = "3.1.0"
  lazy val zio        = "2.0.2"

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
    /*"-Vprofile",
    "-Vprofile-details 5"*/
    //   "-Yshow-suppressed-errors"
  ),
  libraryDependencies ++= Seq(
    "org.typelevel"                %% "cats-core"            % Version.cats,
    // "org.typelevel"  % "shapeless3-deriving_3" % Version.shapeless,
    "com.softwaremill.magnolia1_3" %% "magnolia"             % Version.magnolia,
    "org.scalatest"                %% "scalatest"            % Version.tests      % Test,
    "org.typelevel"                %% "discipline-scalatest" % Version.discipline % Test,
    "dev.zio"                      %% "zio-test"             % Version.zio        % Test,
    "dev.zio"                      %% "zio-test-sbt"         % Version.zio        % Test
  ),
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
)

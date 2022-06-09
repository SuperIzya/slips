ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.2"

lazy val Version = new {
  lazy val cats  = "2.7.0"
  lazy val tests = "3.2.12"

}

lazy val root = (project in file("."))
  .settings(
    name := "slips4",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % Version.cats,
      "org.scalatest" %% "scalatest" % Version.tests % Test
    )
  )

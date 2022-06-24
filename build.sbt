ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"


lazy val Version = new {
  lazy val cats = "2.7.0"
  lazy val tests = "3.2.12"
  lazy val discipline = "2.1.5"
  lazy val shapeless = "3.1.0"
}

lazy val root = (project in file("."))
  .settings(
    name := "slips4",
    scalacOptions ++= Seq("-Ycheck:all", "-explain"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % Version.cats,
      "org.typelevel" % "shapeless3-deriving_3" % Version.shapeless,
      "org.scalatest" %% "scalatest" % Version.tests % Test,
      "org.typelevel" %% "discipline-scalatest" % Version.discipline % Test
    )
  )

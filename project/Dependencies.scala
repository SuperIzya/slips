import sbt.*

object Dependencies {
  object Versions {
    val cats       = "2.13.0"
    val tests      = "3.2.20"
    val magnolia   = "1.3.21"
    val discipline = "2.3.0"
    val zio        = "2.1.26"
  }

  val libraries = List(
    "org.typelevel"                %% "cats-core"            % Versions.cats,
    "com.softwaremill.magnolia1_3" %% "magnolia"             % Versions.magnolia,
    "org.scalatest"                %% "scalatest"            % Versions.tests      % Test,
    "org.typelevel"                %% "discipline-scalatest" % Versions.discipline % Test,
    "dev.zio"                      %% "zio-test"             % Versions.zio        % Test,
    "dev.zio"                      %% "zio-test-sbt"         % Versions.zio        % Test
  )
}

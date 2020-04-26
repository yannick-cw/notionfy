import sbt._

object Dependencies {

  object Versions {
    val cats       = "2.0.0"
    val catsEffect = "2.0.0"

    // Test
    val scalaTest  = "3.0.8"
    val scalaCheck = "1.14.2"

    val sttp = "2.1.0-RC1"

    val circe = "0.13.0"

    // Compiler
    val kindProjector    = "0.10.3"
    val betterMonadicFor = "0.3.0"
  }

  object Libraries {
    lazy val cats       = "org.typelevel" %% "cats-core"     % Versions.cats
    lazy val catsEffect = "org.typelevel" %% "cats-effect"   % Versions.catsEffect
    lazy val catsMtl    = "org.typelevel" %% "cats-mtl-core" % "0.7.1"

    lazy val circe = "io.circe" %% "circe-core" % Versions.circe
    lazy val circeGeneric = "io.circe" %% "circe-generic" % Versions.circe

    lazy val sttp      = "com.softwaremill.sttp.client" %% "core"  % Versions.sttp
    lazy val sttpCirce = "com.softwaremill.sttp.client" %% "circe" % Versions.sttp

    lazy val atto = "org.tpolecat" %% "atto-core" % "0.7.0"

    lazy val osLib = "com.lihaoyi" %% "os-lib" % "0.6.3"

    lazy val decline = "com.monovore" %% "decline-effect" % "1.2.0"

    // Test
    lazy val scalaTest  = "org.scalatest"  %% "scalatest"  % Versions.scalaTest
    lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % Versions.scalaCheck

    // Compiler
    lazy val kindProjector    = "org.typelevel" %% "kind-projector"     % Versions.kindProjector
    lazy val betterMonadicFor = "com.olegpy"    %% "better-monadic-for" % Versions.betterMonadicFor
  }

}

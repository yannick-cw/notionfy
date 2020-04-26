import Dependencies.Libraries

name := """notionfys"""

organization in ThisBuild := "notionfys"

scalaVersion in ThisBuild := "2.13.1"

enablePlugins(GraalVMNativeImagePlugin)

mappings in (Compile, packageDoc) := Seq()

val maybeGraalDockerVersion = sys.env.get("GRAAL_DOCKER_VERSION") //e.g. 20.0.0

graalVMNativeImageOptions ++= Seq(
  "--no-fallback",
  "--allow-incomplete-classpath",
  "--report-unsupported-elements-at-runtime",
  "--initialize-at-build-time",
  "--enable-https",
  "-J-Xmx8g"
) ++ maybeGraalDockerVersion.map(_ => "--static")

val nativeImagePath = sys.env.get("NATIVE_IMAGE_PATH")
  .map(path => Seq(graalVMNativeImageCommand := path))
  .getOrElse(Seq())

lazy val commonSettings = Seq(
  organizationName := "notionfys",
  scalafmtOnCompile := true,
  libraryDependencies ++= Seq(
    Libraries.cats,
    Libraries.catsEffect,
    Libraries.catsMtl,
    Libraries.osLib,
    Libraries.atto,
    Libraries.decline,
    Libraries.circe,
    Libraries.circeGeneric,
    Libraries.sttp,
    Libraries.sttpCirce,
    Libraries.scalaTest  % Test,
    Libraries.scalaCheck % Test,
    compilerPlugin(Libraries.kindProjector),
    compilerPlugin(Libraries.betterMonadicFor)
  ),
  graalVMNativeImageGraalVersion := maybeGraalDockerVersion,
) ++ nativeImagePath

lazy val root =
  (project in file("."))
  .settings(commonSettings: _*)

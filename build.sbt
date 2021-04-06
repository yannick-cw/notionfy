import Dependencies.Libraries

name := """notionfys"""

organization in ThisBuild := "notionfys"

scalaVersion in ThisBuild := "2.13.1"

version in ThisBuild := "0.3.1"

enablePlugins(GraalVMNativeImagePlugin)

mappings in (Compile, packageDoc) := Seq()

enablePlugins(NativeImagePlugin)

nativeImageOptions ++= List(
  "--no-fallback",
  "--allow-incomplete-classpath",
  "--report-unsupported-elements-at-runtime",
  "--initialize-at-build-time",
  "--enable-https",
  "-J-Xmx8g"
)

graalVMNativeImageOptions ++= Seq(
  "--no-fallback",
  "--allow-incomplete-classpath",
  "--report-unsupported-elements-at-runtime",
  "--initialize-at-build-time",
  "--enable-https",
  "-J-Xmx8g"
)

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
    Libraries.scalaTest % Test,
    Libraries.scalaCheck % Test,
    compilerPlugin(Libraries.kindProjector),
    compilerPlugin(Libraries.betterMonadicFor)
  )
) ++ nativeImagePath

lazy val root =
  (project in file("."))
    .settings(Compile / mainClass := Some("notionfys.Main"))
    .settings(commonSettings)

ThisBuild / scalaVersion     := "2.12.18"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val commonSettings = Seq(
  libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.18",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test", 
  libraryDependencies += "org.scalameta" %% "scalameta" % "4.9.1",
  libraryDependencies += scalaVersion("org.scala-lang" % "scala-reflect" % _).value,
  libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.3.1",
)

lazy val core = (project in file("core"))
  .settings(
    name := "BSPModel-core",
    commonSettings
  )

lazy val opt = (project in file("opt"))
  .settings(
    name := "BSPModel",
    commonSettings
).dependsOn(macros, core)

lazy val macros = (project in file("macros"))
  .settings(
    name := "BSPModel-macros",
    commonSettings
  ).dependsOn(core)
// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

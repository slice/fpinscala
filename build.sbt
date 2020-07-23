ThisBuild / scalaVersion := "2.13.3"
ThisBuild / organization := "zone.slice"

lazy val fpinscala = (project in file("."))
  .settings(
    name := "fpinscala",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.9" % Test,
    Test / parallelExecution := false,
    testFrameworks += new TestFramework("munit.Framework")
  )

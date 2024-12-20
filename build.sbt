val scala3Version = "3.5.2"

enablePlugins(JavaAppPackaging)

lazy val root = project
  .in(file("."))
  .settings(
    name := "langur",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.4" % "test",
    //    libraryDependencies += "com.lihaoyi" %% "mainargs" % "0.2.2",

    testFrameworks += new TestFramework("utest.runner.Framework")
  )

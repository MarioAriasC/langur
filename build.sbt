val scala3Version = "3.8.2"

enablePlugins(JavaAppPackaging)
enablePlugins(GraalVMNativeImagePlugin)

lazy val root = project
  .in(file("."))
  .settings(
    name := "langur",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.9.5" % "test",

    testFrameworks += new TestFramework("utest.runner.Framework")
  )

Compile / mainClass := Some("org.marioarias.langur.main.benchmark")
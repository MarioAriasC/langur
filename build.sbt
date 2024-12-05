val scala3Version = "3.3.3"

enablePlugins(JavaAppPackaging)

enablePlugins(ScalaNativePlugin)

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

import scala.scalanative.build._

nativeConfig ~= { c =>
    c.withLTO(LTO.none)
      .withMode(Mode.debug)
      .withGC(GC.immix)
}
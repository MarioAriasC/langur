val scala3Version = "3.8.2"

enablePlugins(ScalaNativePlugin)
enablePlugins(ScalaNativeJUnitPlugin)

lazy val root = project
  .in(file("."))
  .settings(
    name := "langur",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "junit" % "junit" % "4.13.2"
  )

import scala.scalanative.build._

nativeConfig ~= { c =>
    c.withLTO(LTO.none)
      .withMode(Mode.debug)
      .withGC(GC.commix)
}
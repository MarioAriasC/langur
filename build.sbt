import scala.scalanative.build.{GC, LTO, Mode}

scalaVersion := "2.13.7"

// Set to false or remove if you want to show stubs as linking errors
nativeLinkStubs := true

enablePlugins(ScalaNativePlugin)

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.10" % "test"
libraryDependencies += "com.lihaoyi" %%% "mainargs" % "0.2.2"

testFrameworks += new TestFramework("utest.runner.Framework")

nativeConfig ~= {
  _.withLTO(LTO.thin).withMode(Mode.releaseFast).withGC(GC.commix)
}
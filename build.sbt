scalaVersion := "2.13.7"

// Set to false or remove if you want to show stubs as linking errors
nativeLinkStubs := true

enablePlugins(ScalaNativePlugin)

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.10" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")
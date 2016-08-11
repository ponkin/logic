name := "logic"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalacheck" % "scalacheck_2.11" % "1.13.2" % "test"

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "100", "-workers", "1", "-verbosity", "1")

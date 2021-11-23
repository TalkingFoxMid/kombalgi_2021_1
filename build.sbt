name := "ostov"

version := "0.1"

scalaVersion := "2.13.6"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.2.9"
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
// https://mvnrepository.com/artifact/org.scalatest/scalatest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % Test

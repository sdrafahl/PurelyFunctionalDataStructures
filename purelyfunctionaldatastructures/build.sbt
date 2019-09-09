import Dependencies._

ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

lazy val root = (project in file("."))
  .settings(
    name := "purelyFunctionalDataStructures",
    libraryDependencies += scalaTest % Test
  )

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"


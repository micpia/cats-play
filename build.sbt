name := """cats-play"""
organization := "goals"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.13.3"

libraryDependencies ++= {
  Seq(
    guice,
    "org.typelevel" %% "cats-core" % "2.3.0",

    "org.scalatestplus.play" %% "scalatestplus-play" % "5.0.0" % Test
  )
}

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "goals.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "goals.binders._"

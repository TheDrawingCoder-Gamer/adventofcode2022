ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "aoc2022",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.2",
    libraryDependencies += "org.typelevel" %% "cats-collections-core" % "0.9.5",
    libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.7"
  )

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "neuro-evolution",
    Compile / scalacOptions ++= Seq(
      "-Ykind-projector:underscores"
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.8.0",
      "org.typelevel" %% "cats-effect" % "3.3.14"
    )
  )

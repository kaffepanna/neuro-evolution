ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"

lazy val root = (project in file("."))
  .settings(
    name := "neuro-evolution",
    Compile / scalacOptions ++= Seq(
      "-Ykind-projector:underscores"
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.8.0",
      "org.typelevel" %% "cats-free" % "2.8.0",
      "org.typelevel" %% "cats-effect" % "3.5.4",
      "co.fs2"        %% "fs2-core"    % "3.10.2",
      "co.fs2"        %% "fs2-io"      % "3.10.2",
      "co.fs2"        %% "fs2-process" % "3.10.2"
    )
  )

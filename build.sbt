ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"

lazy val osName = System.getProperty("os.name").toLowerCase match {
  case n if n.contains("win")   => "win"
  case n if n.contains("mac")   => "mac"
  case n if n.contains("linux") => "linux"
  case _ => throw new Exception("Unknown platform")
}

lazy val catsDependencies = Seq(
  "org.typelevel" %% "cats-core" % "2.8.0",
  "org.typelevel" %% "cats-free" % "2.8.0",
  "org.typelevel" %% "cats-effect" % "3.5.4",
)

lazy val pureConfigDependencies = Seq(
    "com.github.pureconfig" %% "pureconfig-core" % "0.17.9",
    "com.github.pureconfig" %% "pureconfig-cats-effect" % "0.17.9",
)

lazy val fs2Dependencies = Seq(
    "co.fs2"        %% "fs2-core"    % "3.12.2",
    "co.fs2"        %% "fs2-io"      % "3.12.2",
)

lazy val `graph-tools` = (project in file("./graph-tools"))
  .settings(
    name := "neuro-evolution-graph-tools",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.4" % Test
    )
  )  

lazy val genome = (project in file("./genome"))
  .settings(
    name := "neuro-evolution-genome",
    Compile / scalacOptions ++= Seq(
      "-Ykind-projector:underscores"
    ),
    libraryDependencies ++= catsDependencies ++ pureConfigDependencies
  )

lazy val evaluator = (project in file("./evaluator"))
  .settings(
    name := "neuro-evolution-evaluator",
    Compile / scalacOptions ++= Seq(
      "-Ykind-projector:underscores"
    ),
    libraryDependencies ++= catsDependencies
  ).dependsOn(genome, `graph-tools`)

lazy val evolution = (project in file("./evolution"))
  .settings(
    name := "neuro-evolution",
    Compile / scalacOptions ++= Seq(
      "-Ykind-projector:underscores"
    ),
    libraryDependencies ++= catsDependencies
  ).dependsOn(genome, evaluator)

lazy val graphviz = (project in file("./graphviz"))
  .settings(
    name := "neuro-evolution-graphviz",
    libraryDependencies ++= catsDependencies ++ fs2Dependencies
  ).dependsOn(genome, evaluator)

lazy val example = (project in file("./example"))
  .settings(
    name := "neuro-evolution-example",
    Compile / scalacOptions ++= Seq(
      "-Ykind-projector:underscores"
    ),
    libraryDependencies ++= catsDependencies ++ pureConfigDependencies
  ).dependsOn(genome, evaluator, evolution, graphviz)

lazy val `the-game` = (project in file("./the-game"))
  .settings(
    name := "neuro-evolution-the-game",
    Compile / scalacOptions ++= Seq(
      "-Ykind-projector:underscores"
    ),
    libraryDependencies ++= catsDependencies ++ pureConfigDependencies ++ Seq(
      "org.scalafx" %% "scalafx" % "21.0.0-R32",
      "org.openjfx" % "javafx-base"     % "21.0.1" classifier osName,
      "org.openjfx" % "javafx-graphics" % "21.0.1" classifier osName,
      "org.openjfx" % "javafx-controls" % "21.0.1" classifier osName
    )
  ).dependsOn(genome, evaluator, evolution)
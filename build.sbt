
val commonSettings: Seq[Setting[_]] = Seq(
  // offline in ThisBuild := true,
  organization := "com.atheriault",
  version := "0.1-SNAPSHOT",
  scalacOptions ++= Seq(
    "-language:implicitConversions",
    "-language:reflectiveCalls",

    "-feature", "-deprecation",

    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused:imports",

    "-Xlint:infer-any",
    "-Xlint:inaccessible",
    "-Xlint:delayedinit-select"
  )
)


lazy val fgl = (project in file("."))
  .settings(commonSettings)
  .settings(
    name := "fgl",
    version := "0.1.0.0",
    scalaVersion := "2.13.1",

    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-collection-contrib" % "0.1.0",

      "org.scalatest"          %% "scalatest"                % "3.0.8"  % Test,
      "org.scalacheck"         %% "scalacheck"               % "1.14.0" % Test,
    )
  )


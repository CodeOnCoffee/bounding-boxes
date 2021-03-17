scalaVersion := "2.13.3"

name := "bounding-box"
organization := "org.codeoncoffee.scala"
version := "0.1"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.typelevel" %% "cats-core" % "2.1.1",
  "org.wvlet.airframe"  %% "airframe-launcher" % "19.11.1",
  "org.scalactic" %% "scalactic" % "3.2.5",
  "org.scalatest" %% "scalatest" % "3.2.5" % "test",
)

enablePlugins(PackPlugin)
packMain := Map("bounding-box" -> "org.codeoncoffee.boxer.BoxerApp")
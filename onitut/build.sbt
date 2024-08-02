name := "onitut"

version := "0.2"

scalaVersion := "3.4.2"


maxErrors := 10

watchTriggeredMessage := Watch.clearScreenOnTrigger

libraryDependencies ++= Seq(
   "org.specs2" %% "specs2-core"           % "4.20.8" % "test",
   "com.drewnoakes" % "metadata-extractor" % "2.15.0")

scalacOptions ++= Seq("-deprecation", "-feature")

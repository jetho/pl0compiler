scalaVersion := "2.10.0-M7"
 
resolvers += "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/"
 
libraryDependencies ++= Seq(
  "org.scalaz" % "scalaz-core" % "7.0.0-M3" cross CrossVersion.full,
  "org.scalatest" % "scalatest_2.10.0-M5" % "1.9-2.10.0-M5-B2"
)
 
scalacOptions += "-feature"
 
initialCommands in console := "import scalaz._, Scalaz._"

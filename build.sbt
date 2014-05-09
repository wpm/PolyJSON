name := "PolyJSON"

version := "1.0"

scalaVersion := "2.11.0"

resolvers += "spray" at "http://repo.spray.io/"

libraryDependencies ++= Seq(
  "io.spray" %%  "spray-json" % "1.2.6"
)

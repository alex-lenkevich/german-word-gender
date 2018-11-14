name := "german-word-gender"

version := "0.1"

scalaVersion := "2.12.7"

enablePlugins(JavaAppPackaging)

mainClass in (Compile, run) := Some("link.german.gender.telegram.Server")

libraryDependencies ++= Seq(
  "org.mongodb.scala" %% "mongo-scala-driver" % "2.4.2",
  "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.22",
  "com.typesafe.akka" %% "akka-http" % "10.1.5",
  "com.typesafe.akka" %% "akka-stream" % "2.5.12",
  "io.circe" %% "circe-core" % "0.10.0",
  "io.circe" %% "circe-parser" % "0.10.0",
  "io.circe" %% "circe-generic" % "0.10.0",
  "io.circe" %% "circe-generic-extras" % "0.10.0",
  "de.heikoseeberger" %% "akka-http-circe" % "1.22.0"
)

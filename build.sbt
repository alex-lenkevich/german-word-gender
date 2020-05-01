name := "german-word-gender"

version := "0.1"

scalaVersion := "2.12.7"

enablePlugins(JavaAppPackaging)

mainClass in (Compile, run) := Some("link.german.gender.trainer.Main")

libraryDependencies ++= Seq(
  "org.mongodb.scala" %% "mongo-scala-driver" % "2.4.2",
  "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.22",
  "com.typesafe.akka" %% "akka-http" % "10.1.5",
  "com.typesafe.akka" %% "akka-stream" % "2.5.12",
  "io.circe" %% "circe-core" % "0.10.0",
  "io.circe" %% "circe-parser" % "0.10.0",
  "io.circe" %% "circe-generic" % "0.10.0",
  "io.circe" %% "circe-generic-extras" % "0.10.0",
  "de.heikoseeberger" %% "akka-http-circe" % "1.22.0",
  "com.itextpdf" % "kernel" % "7.1.10",
  "org.seleniumhq.selenium" % "selenium-java" % "4.0.0-alpha-5",
  "dev.zio" %% "zio" % "1.0.0-RC18-2",
  "org.typelevel" %% "cats-core" % "2.1.1",
  "javazoom" % "jlayer" % "1.0.1",
  "org.apache.commons" % "commons-text" % "1.8"
)

scalacOptions += "-Ypartial-unification"

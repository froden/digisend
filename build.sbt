name := "digisend"

version := "1.0"

scalaVersion := "2.9.2"

resolvers ++= Seq(
  "Local maven repo" at Path.userHome.asFile.toURI.toURL + ".m2/repository",
  "JAnalyse Repository" at "http://www.janalyse.fr/repository/"
)

libraryDependencies ++= Seq(
  "fr.janalyse" %% "janalyse-ssh" % "0.9.5-b3",
  "no.digipost" % "digipost-api-client-java" % "2.1-SNAPSHOT",
  "com.sun.jersey" % "jersey-core" % "1.12"
)

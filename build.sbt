organization := "fr.splayce"

name := "REL"

version := "0.1.1"

scalaVersion := "2.9.1"

libraryDependencies += "org.specs2" %% "specs2" % "1.10" % "test"

publishTo <<= version { (v: String) =>
  val url = "http://nexus.imaginatio.fr/content/repositories/"
  val realm = "Nexus imaginatio "
  if (v.trim.endsWith("SNAPSHOT"))
    Some(realm + "releases" at url + "snapshots")
  else
    Some(realm + "snapshots" at url + "releases")
}

publishArtifact in Test := false

publishMavenStyle := true

pomIncludeRepository := { _ => false }

credentials += Credentials(Path.userHome / ".ivy2" / ".nexus.credentials")

unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))

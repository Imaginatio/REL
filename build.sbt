organization := "fr.splayce"

name := "REL"

version := "0.3.1"

scalaVersion := "2.9.1"

crossScalaVersions := Seq("2.9.1", "2.9.2", "2.10.0")

libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
	deps :+ (sv match {
		case "2.10.0" => "org.specs2" % "specs2_2.10" % "1.14" % "test"
		case _        => "org.specs2" %% "specs2" % "1.12.3" % "test"
	})
}

scalacOptions ++= Seq("-deprecation", "-unchecked", "-encoding", "UTF8")

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

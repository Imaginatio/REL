organization := "fr.splayce"

name := "REL"

version := "0.3.2-SNAPSHOT"

scalaVersion := "2.9.1"

crossScalaVersions := Seq("2.9.1", "2.9.2", "2.10.0")

libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
	deps :+ (sv match {
		case "2.10.0" => "org.specs2" % "specs2_2.10" % "1.14" % "test"
		case _        => "org.specs2" %% "specs2" % "1.12.3" % "test"
	})
}

scalacOptions <<= scalaVersion map { v: String =>
  val default = Seq("-deprecation", "-unchecked", "-encoding", "UTF8")
  if (v.startsWith("2.9."))
    default
  else
    default ++ Seq("-feature", "-language:postfixOps", "-language:implicitConversions")
}

publishTo <<= version { (v: String) =>
  val url = "http://integration.imaginatio.fr:2000/nexus/content/repositories/"
  val realm = "Sonatype Nexus Repository Manager"
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

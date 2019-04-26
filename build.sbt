ThisBuild / scalacOptions ++= Seq(
  "-encoding", "utf8",
  "-Xfatal-warnings", 
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-language:postfixOps"           // Allow postfix operator notation, such as `1 to 10 toList'
  //-language:dynamics             # Allow direct or indirect subclasses of scala.Dynamic
  //-language:existential          # Existential types (besides wildcard types) can be written and inferred -language:experimental.macros  # Allow macro defintion (besides implementation and application)
  //-language:higherKinds          # Allow higher-kinded types
  //-language:reflectiveCalls      # Allow reflective access to members of structural types
)

ThisBuild / scalacOptions in Test ++= Seq("-Yrangepos")

ThisBuild / scalaVersion := "2.12.7"
ThisBuild / organization := "fr.splayce"

lazy val rel = (project in file("."))
  .settings(
    name := "REL",
    libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.6" % Test
  )

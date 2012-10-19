version := "0.1"

organization := "Stanford_PPL"

retrieveManaged := true

resolvers += ScalaToolsSnapshots

resolvers += dropboxScalaTestRepo

scalaOrganization := "org.scala-lang"

scalaVersion := virtScala

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

scalacOptions += "-Yvirtualize"

//scalacOptions += "-Yvirtpatmat"

//scalacOptions in Compile ++= Seq(/*Unchecked, */Deprecation)

// needed for scala.tools, which is apparently not included in sbt's built in version
libraryDependencies += "org.scala-lang" % "scala-library" % virtScala

libraryDependencies += "org.scala-lang" % "scala-compiler" % virtScala

libraryDependencies += scalaTest

libraryDependencies += virtualization_lms_core

// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false 

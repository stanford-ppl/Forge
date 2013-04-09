version := "0.1"

organization := "Stanford_PPL"

retrieveManaged := true

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := virtScala

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

scalacOptions += "-Yvirtualize"

//scalacOptions += "-Yvirtpatmat"

scalacOptions in Compile ++= Seq("-unchecked", "-deprecation")

libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % virtScala

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % virtScala

libraryDependencies += virtualization_lms_core

libraryDependencies += "org.scala-lang" % "scala-actors" % virtScala // for ScalaTest

libraryDependencies += scalaTest

// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false 

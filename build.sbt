version := "0.1"

organization := "Stanford_PPL"

retrieveManaged := true

scalaOrganization := "org.scala-lang"

scalaVersion := virtScala

scalaHome := Some(file(sys.env("SCALA_VIRTUALIZED_HOME")))

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

scalacOptions += "-Yvirtualize"

//scalacOptions += "-Yvirtpatmat"

scalacOptions in Compile ++= Seq("-unchecked", "-deprecation")

libraryDependencies += "org.scala-lang" % "scala-library" % virtScala

libraryDependencies += "org.scala-lang" % "scala-compiler" % virtScala

libraryDependencies += scalaTest

// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false 

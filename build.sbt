version := "0.1"

organization := "Stanford_PPL"

retrieveManaged := true

//scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := virtScala

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

//scalacOptions += "-Yvirtualize"

//scalacOptions += "-Yvirtpatmat"

scalacOptions in Compile ++= Seq("-unchecked", "-deprecation")

libraryDependencies += "org.scala-lang" % "scala-library" % virtScala

libraryDependencies += "org.scala-lang" % "scala-compiler" % virtScala

libraryDependencies += scalaTest

val paradiseVersion = "2.0.1"

libraryDependencies ++= (
	if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
	else Nil
)

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)

// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

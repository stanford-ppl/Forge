

import sbt._
import Keys._

object OptiMLBuild extends Build {
  if (System.getProperty("showSuppressedErrors") == null) System.setProperty("showSuppressedErrors", "false")
  val virtScala = "2.11.2"
  val virtBuildSettingsBase = Project.defaultSettings ++ Seq(
    organization := "stanford-ppl",
    //scalaOrganization := "org.scala-lang.virtualized",
    scalaVersion := virtScala,

    publishArtifact in (Compile, packageDoc) := false,
    // needed for scala.tools, which is apparently not included in sbt's built in version
    libraryDependencies += "org.scala-lang" % "scala-library" % virtScala,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % virtScala,
    libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.2",
    libraryDependencies += "com.google.guava" % "guava" % "17.0",
    libraryDependencies += "org.apache.commons" % "commons-math" % "2.2",
    libraryDependencies += "commons-io" % "commons-io" % "2.4",
    // cluster deps
    libraryDependencies += "com.google.protobuf" % "protobuf-java" % "2.5.0",
    libraryDependencies += "org.apache.mesos" % "mesos" % "0.20.1",
    libraryDependencies += "org.apache.hadoop" % "hadoop-common" % "2.5.1",
    libraryDependencies += "org.apache.hadoop" % "hadoop-client" % "2.5.1",
    libraryDependencies += "org.apache.hadoop" % "hadoop-hdfs" % "2.5.1",
    // MACRO VIRTUALIZATION
    libraryDependencies += "org.scala-lang.virtualized" %% "scala-virtualized" % "0.0.1-SNAPSHOT",
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full),
    // used in delitec to access jars
    retrieveManaged := true,
    scalacOptions += "-Yno-generic-signatures",
    //scalacOptions += "-Yvirtualize",
    initialCommands in console += "import optiml.library._; val OptiML = new OptiMLREPL { def main() = {} }; import OptiML._"
  )

  val virtBuildSettings = virtBuildSettingsBase ++ Seq(
    scalaSource in Compile <<= baseDirectory(_ / "src")
  )


  lazy val LMS_HOME = sys.env.get("LMS_HOME").getOrElse(error("Please set the LMS_HOME environment variable."))
  lazy val DELITE_HOME = sys.env.get("DELITE_HOME").getOrElse(error("Please set the DELITE_HOME environment variable."))

  val scalacp = "/target/scala-2.11/classes/"
  lazy val lms = file(LMS_HOME + scalacp)
  lazy val deliteFramework = file(DELITE_HOME + "/framework" + scalacp)
  lazy val deliteRuntime = file(DELITE_HOME + "/runtime" + scalacp)
  lazy val deliteTest = file(DELITE_HOME + "/framework/delite-test" + scalacp)

  val deps = Seq(
    unmanagedClasspath in Compile <+= (baseDirectory) map { bd => Attributed.blank(lms) },
    unmanagedClasspath in Compile <+= (baseDirectory) map { bd => Attributed.blank(deliteFramework) },
    unmanagedClasspath in Compile <+= (baseDirectory) map { bd => Attributed.blank(deliteRuntime) },
    unmanagedClasspath in Compile <+= (baseDirectory) map { bd => Attributed.blank(deliteTest) },
    unmanagedClasspath in Test <+= (baseDirectory) map { bd => Attributed.blank(lms) },
    unmanagedClasspath in Test <+= (baseDirectory) map { bd => Attributed.blank(deliteFramework) },
    unmanagedClasspath in Test <+= (baseDirectory) map { bd => Attributed.blank(deliteRuntime) },
    unmanagedClasspath in Test <+= (baseDirectory) map { bd => Attributed.blank(deliteTest) }
  )


  // build targets
  lazy val OptiML = Project("OptiML", file("."), settings = virtBuildSettings ++ deps) dependsOn(OptiMLApps) // default
  lazy val OptiMLShared = Project("OptiML-shared", file("shared"), settings = virtBuildSettings ++ deps)
  lazy val OptiMLComp = Project("OptiML-comp", file("compiler"), settings = virtBuildSettings ++ deps) dependsOn(OptiMLShared)
  lazy val OptiMLLib = Project("OptiML-lib", file("library"), settings = virtBuildSettings ++ deps) dependsOn(OptiMLShared)
  lazy val OptiMLIdent = Project("OptiML-ident", file("ident"), settings = virtBuildSettings ++ deps) dependsOn(OptiMLShared)
  lazy val OptiMLDirect = Project("OptiML-direct", file("direct"), settings = virtBuildSettings ++ deps)
  lazy val OptiMLApps = Project("OptiML-apps", file("apps"), settings = virtBuildSettings ++ deps) dependsOn(OptiMLComp, OptiMLLib, OptiMLIdent, OptiMLDirect)
  lazy val OptiMLTests = Project("OptiML-tests", file("tests"), settings = virtBuildSettingsBase ++ deps ++ Seq(
    scalaSource in Test <<= baseDirectory(_ / "src"),
    parallelExecution in Test := false,
    concurrentRestrictions in Test += Tags.limitAll(1), // don't run anything in parallel
    // Required to use native libraries within tests
    // See http://stackoverflow.com/questions/19425613/unsatisfiedlinkerror-with-native-library-under-sbt
    // and http://www.scala-sbt.org/release/docs/Forking.html
    //fork := true,
    //javaOptions in Test := sys.env.getOrElse("SBT_OPTS", "").split(" ")
    fork := false
  )) dependsOn(OptiMLComp, OptiMLLib, OptiMLDirect)
}


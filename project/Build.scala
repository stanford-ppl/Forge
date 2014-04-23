import sbt._
import Keys._

import forge.preprocessor._

object ForgeBuild extends Build with ForgePreprocessor {
  val virtScala = Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.2")
  val virtualization_lms_core = "EPFL" % "lms_2.10" % "0.3-SNAPSHOT"
  val scalaTest = "org.scalatest" % "scalatest_2.10" % "2.1.2"

  System.setProperty("showSuppressedErrors", "false")

  // hook in Forge preprocessor
  val forgeSettings = Defaults.defaultSettings ++ Seq(
    organization := "stanford-ppl",
    libraryDependencies += virtualization_lms_core,
    sources in Compile <<= (sourceManaged in Compile, sources in Compile, streams) map { (dir,files,s) => files.map(preprocess(dir,_,s)) }
  )

  lazy val forge = Project("Forge", file("."), settings = forgeSettings)
}


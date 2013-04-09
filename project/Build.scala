import sbt._
import java.io.File

object ForgeBuild extends Build {
  val virtScala = Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.0")
  val virtualization_lms_core = "EPFL" % "lms_2.10.0" % "0.3-SNAPSHOT"
  val scalaTest = "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"
 
  // -DshowSuppressedErrors=false
    System.setProperty("showSuppressedErrors", "false")

  lazy val forge = Project("Forge", file("."))
}

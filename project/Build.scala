import sbt._
import java.io.File

object ForgeBuild extends Build {
  // FIXME: custom-built scalatest
  val dropboxScalaTestRepo = "Dropbox" at "http://dl.dropbox.com/u/12870350/scala-virtualized"
  val scalaTest = "org.scalatest" % "scalatest_2.10.0-virtualized-SNAPSHOT" % "1.6.1-SNAPSHOT" % "test"
  val virtScala = Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.0-M1-virtualized")
  val virtualization_lms_core = "EPFL" % "lms_2.10.0-M1-virtualized" % "0.2"
  
  lazy val metal = Project("Forge", file("."))
}

package LOWERCASE_DSL_NAME.library

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

import org.scalatest._
import java.io._
import scala.collection.mutable.ArrayBuffer

import LOWERCASE_DSL_NAME.shared.ForgeTestModule

trait TestsWrapper extends HUMAN_DSL_NAMEBase

trait ForgeTestRunnerInterpreter extends ForgeTestModule {
  this: TestsWrapper =>

  val collector: Rep[ArrayBuffer[Boolean]] = new ArrayBuffer[Boolean](0)

  def collect(s: Rep[Boolean]) = { collector += s }

  def mkReport(): Rep[Unit] = { () }
}

trait ForgeSuiteInterpreter extends Suite {
  val verbose = System.getProperty("tests.verbose", "false").toBoolean

  def runTest(app: ForgeTestRunnerInterpreter) {
    app.main()

    // ignore the report file and read the collector directly
    if (verbose) println("==== " + app.getClass.getSimpleName + " ====")
    val results = app.collector
    for (i <- 0 until results.length) {
      val b = results(i)
      if (verbose) print("  condition " + i + ": ")
      if (verbose)
        if (b) println("PASSED") else println("FAILED")
      assert(b)
    }
  }

  def runTests(apps: ForgeTestRunnerInterpreter*) = apps.foreach(runTest)
}

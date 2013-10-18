package LOWERCASE_DSL_NAME.compiler

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

import ppl.tests.scalatest._
import scala.collection.mutable.ArrayBuffer

import LOWERCASE_DSL_NAME.shared.ForgeTestModule

trait TestsOpsExp extends DeliteTestOpsExp

trait ScalaGenTestsOps extends ScalaGenDeliteTest { val IR: TestsOpsExp }
trait CudaGenTestsOps extends CudaGenSynchronizedArrayBufferOps with CudaGenStringOps with CudaGenIOOps { val IR: TestsOpsExp }
trait OpenCLGenTestsOps extends OpenCLGenSynchronizedArrayBufferOps with OpenCLGenStringOps with OpenCLGenIOOps { val IR: TestsOpsExp }
trait CGenTestsOps extends CGenSynchronizedArrayBufferOps with CGenStringOps with CGenIOOps { val IR: TestsOpsExp }

trait ForgeTestRunnerCompiler extends ForgeTestModule with TestsOpsExp with DeliteTestRunner {
  // everything inherited from DeliteTestRunner
}

trait ForgeSuiteCompiler extends DeliteSuite {
  def runTest(app: ForgeTestRunnerCompiler) { compileAndTest(app) }
}

package dadl.compiler.ops

import scala.virtualization.lms.common.BaseExp

import dadl.shared._
import dadl.shared.ops._
import dadl.compiler._
import dadl.compiler.ops._

trait ModuleIOOpsExp extends ModuleIOOps with BaseExp {
  this: DADLExp =>

  def input[T:Manifest]: Wire[T] = fresh[T]
}


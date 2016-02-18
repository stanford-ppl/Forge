package dadl.shared.ops

import dadl.shared._
import dadl.shared.ops._

import scala.virtualization.lms.common.Record

trait ModuleIOOps {
  this: DADL =>

  abstract class IO extends Record

  //def input[T:Manifest]: Rep[T]
}
trait ModuleIOCompilerOps extends ModuleIOOps { this: DADL => }
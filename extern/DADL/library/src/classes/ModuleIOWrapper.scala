package dadl.library.classes

import dadl.shared._
import dadl.shared.ops._
import dadl.library._
import dadl.library.classes._

trait ModuleIOWrapper {
  this: DADLBase with DADLClasses =>

  //def input[T:Manifest]: Rep[T] = { throw new Exception("Method 'input' is not supported in the library backend") }
}
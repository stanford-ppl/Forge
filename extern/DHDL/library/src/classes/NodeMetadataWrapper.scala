package dhdl.library.classes
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.library._
import dhdl.library.classes._


trait NodeMetadataWrapper {
  this: DHDLBase with DHDLClasses =>

  def parOf(e: Rep[Any]): Int = 1
}

package dhdl.shared.ops

import scala.virtualization.lms.common.Base
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._

trait NodeMetadataTypes extends Base {
  def parOf(e: Rep[Any]): Int
}

trait NodeMetadataOps extends Base {this: DHDL => }
trait NodeMetadataCompilerOps extends NodeMetadataOps with NodeMetadataTypes {
  this: DHDL =>
}

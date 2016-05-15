package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.transform.SinglePassTransformer

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait BRAMVectorLowering extends SinglePassTransformer {
  val IR: DHDLExp
  import IR._

  debugMode = true

  override def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = rhs match {
    case EatReflect(Bram_load_vector(bram,ofs,cchain,inds)) => None
    case EatReflect(Bram_store_vector(bram,ofs,vec,cchain,inds)) => None
    case _ => None
  }

}

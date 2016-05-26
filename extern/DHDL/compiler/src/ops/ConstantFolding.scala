package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.transform.SinglePassTransformer

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

import scala.collection.mutable.HashMap

// Replaces all fixed value statements with corresponding constant value
// Assumed to be either a fixed or floating point value
trait ConstantFolding extends SinglePassTransformer {
  val IR: DHDLExp
  import IR._

  debugMode = false

  def convertType[T:Manifest](x: Int)(implicit ctx: SourceContext) = x.as[T]
  def convertType[T:Manifest](x: Long)(implicit ctx: SourceContext) = x.as[T]
  def convertType[T:Manifest](x: Float)(implicit ctx: SourceContext) = x.as[T]
  def convertType[T:Manifest](x: Double)(implicit ctx: SourceContext) = x.as[T]

  override def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = lhs match {
    case Def(Reflect(_,_,_)) => None // Never replace effectful statements (for now)

    case Fixed(v) if isBits(lhs.tp) =>
      val c = if (v.toInt == v)         convertType(v.toInt)(lhs.tp, ctx)
              else if (v.toLong == v)   convertType(v.toLong)(lhs.tp, ctx)
              else if (v.toFloat == v)  convertType(v.toFloat)(lhs.tp, ctx)
              else                      convertType(v.toDouble)(lhs.tp, ctx)

      if (c != lhs) {
        val Def(cRhs) = c
        debug(s"Replacing $lhs = $rhs ")
        debug(s"with $c = $cRhs")
        Some(c)
      }
      else None

    case _ => None
  }
}

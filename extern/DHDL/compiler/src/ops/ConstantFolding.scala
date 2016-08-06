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

  override def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = {
    foldConstants(lhs,rhs) match {
      case Some(c) if lhs != c =>
        debug(s"Replacing $lhs = $rhs ")
        c match {
          case Def(cRhs) => debug(s"with $c = $cRhs")
          case _ => debug(s"with $c")
        }
        setProps(c, getProps(lhs))
        Some(c)
      case _ => None
    }
  }

  def foldConstants[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = lhs match {
    case Def(Reflect(_,_,_)) => None // Never replace effectful statements (for now)

    case Fixed(v) if isBits(lhs.tp) =>
      val c = if (v.toInt == v)         convertType(v.toInt)(lhs.tp, ctx)
              else if (v.toLong == v)   convertType(v.toLong)(lhs.tp, ctx)
              else if (v.toFloat == v)  convertType(v.toFloat)(lhs.tp, ctx)
              else                      convertType(v.toDouble)(lhs.tp, ctx)
      Some(c)

    case Deff(FixPt_Mul(a,Fixed(1))) => Some(a)
    case Deff(FixPt_Mul(Fixed(1),b)) => Some(b)
    case Deff(FixPt_Add(a,Fixed(0))) => Some(a)
    case Deff(FixPt_Add(Fixed(0),b)) => Some(b)
    case Deff(FixPt_Sub(a,Fixed(0))) => Some(a)
    case Deff(FixPt_Sub(Fixed(0),b)) => Some(b)
    case Deff(FixPt_Div(a,Fixed(1))) => Some(a)
    // Can't do 0/b, as b may be zero..

    case _ => None
  }
}

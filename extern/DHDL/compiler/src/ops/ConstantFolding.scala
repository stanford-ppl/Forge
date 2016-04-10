package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

import scala.collection.mutable.HashMap

// Replaces all fixed value statements with corresponding constant value
// Assumed to be either a fixed or floating point value
trait ConstantFolding extends TunnelingTransformer {
  val IR: DHDLExp
  import IR._

  override val debugMode = true

  override def transformTP[A](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = rhs match {
    case Fixed(v) =>
      val c = if (v.toInt == v)         v.toInt.as(lhs.tp)
              else if (v.toLong == v)   v.toLong.as(lhs.tp)
              else if (v.toFloat == v)  v.toFloat.as(lhs.tp)
              else                      v.as(lhs.tp)

      debug(s"Replacing $lhs with $c")
      Some(c)

    case _ => super.transformTP(lhs, rhs)
  }
}

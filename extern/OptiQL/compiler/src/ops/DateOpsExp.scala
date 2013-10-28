package optiql.compiler.ops

import scala.tools.nsc.io._
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common.{Base,BaseExp,EffectExp,BaseFatExp}
import scala.virtualization.lms.common.{ScalaGenBase,ScalaGenEffect,ScalaGenFat}
import scala.virtualization.lms.util._
import scala.virtualization.lms.internal._
import ppl.delite.framework.ops.DeliteCollection
import ppl.delite.framework.datastructures._
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.Util._
import optiql.shared._
import optiql.shared.ops._
import optiql.compiler._
import optiql.compiler.ops._

/**
 * IR Definitions
 */

trait DateOpsExp extends DateCompilerOps with BaseFatExp with DeliteStructsExp {
  this: OptiQLExp =>

  case class Date5Object_Apply(__arg0: Rep[String])(implicit val __pos: SourceContext) extends DeliteOpSingleTask[Date](reifyEffectsHere(date_object_apply_impl5(__arg0)(__pos)))

  def date_object_apply(__arg0: Rep[Int])(implicit __pos: SourceContext,__imp1: Overload4) = {
    __arg0.asInstanceOf[Rep[Date]]
  }
  def date_object_apply(__arg0: Rep[String])(implicit __pos: SourceContext,__imp1: Overload5) = {
    reflectPure(Date5Object_Apply(__arg0)(__pos))
  }
  def date_value(self: Rep[Date])(implicit __pos: SourceContext) = {
    self.asInstanceOf[Rep[Int]]
  }
  def date_lt(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext,__imp1: Overload1) = {
    date_lt_impl1(self, __arg0)
  }
  def date_lteq(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext,__imp1: Overload1) = {
    date_lteq_impl1(self, __arg0)
  }
  def date_gt(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext,__imp1: Overload1) = {
    date_gt_impl1(self, __arg0)
  }
  def date_gteq(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext,__imp1: Overload1) = {
    date_gteq_impl1(self, __arg0)
  }


  def date_object_apply_impl5(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[Date] = {
    val tokens = fstring_fsplit(__arg0,unit("-"))
    val year = fstring_toint(array_apply(tokens,unit(0)))
    val month = fstring_toint(array_apply(tokens,unit(1)))
    val day = fstring_toint(array_apply(tokens,unit(2)))
    Date(primitive2_forge_int_plus(primitive2_forge_int_shift_left(year, unit(9)), primitive2_forge_int_plus(primitive2_forge_int_shift_left(month, unit(5)), day)))
  }

  def date_lt_impl1(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext): Rep[Boolean] = {
    ordering2_lt(date_value(self), date_value(__arg0))
  }

  def date_lteq_impl1(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext): Rep[Boolean] = {
    ordering2_lteq(date_value(self), date_value(__arg0))
  }

  def date_gt_impl1(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext): Rep[Boolean] = {
    ordering2_gt(date_value(self), date_value(__arg0))
  }

  def date_gteq_impl1(self: Rep[Date],__arg0: Rep[Date])(implicit __pos: SourceContext): Rep[Boolean] = {
    ordering2_gteq(date_value(self), date_value(__arg0))
  }


  /**
   * Mirroring
   */
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case mn@Date5Object_Apply(__arg0) => reflectPure(new { override val original = Some(f,mn) } with Date5Object_Apply(f(__arg0))(mn.__pos))(mtype(manifest[A]), pos)
    case Reflect(mn@Date5Object_Apply(__arg0), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,mn) } with Date5Object_Apply(f(__arg0))(mn.__pos), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]

}

// these need to exist for externs, even though we don't need them
trait ScalaGenDateOps extends ScalaGenDeliteStruct {
  override def remap[A](m: Manifest[A]): String = m match {
    case m if m.erasure.getSimpleName == "Date" => "Int"
    case m if m.erasure.getSimpleName == "Tup2" => "Int" //HACK
    case m if m.toString.startsWith("scala.collection.immutable.Map") // HACK-ish, maybe use a DSL type instead
      && remap(m.typeArguments(0)) == "Int" => "generated.scala.container.HashMapImpl[" + remap(m.typeArguments(0)) + "]"
    case _ => super.remap(m)
  }

  //because we remapped object types to primitive types above
  override def isPrimitiveType[A](m: Manifest[A]) = remap(m) match {
    case "Boolean" | "Byte" | "Char" | "Short" | "Int" | "Long" | "Float" | "Double" => true
    case _ => false
  }

  override def emitNode(sym: IR.Sym[Any], rhs: IR.Def[Any]) = rhs match {
    case IR.Struct(tag, elems) if sym.tp.erasure.getSimpleName == "Tup2" =>
      emitValDef(sym, "("+ quote(elems(0)._2) + ".toInt << 16) + " + quote(elems(1)._2))
    case f@IR.FieldApply(s, index) if s.tp.erasure.getSimpleName == "Tup2" && index == "_1" =>
      emitValDef(sym, "((" + quote(s) + " & 0xffff0000) >>> 16).toChar")
    case f@IR.FieldApply(s, index) if s.tp.erasure.getSimpleName == "Tup2" && index == "_2" =>
      emitValDef(sym, "(" + quote(s) + " & 0xffff).toChar")
    case _ => super.emitNode(sym, rhs)
  }
}
trait CudaGenDateOps
trait OpenCLGenDateOps
trait CGenDateOps

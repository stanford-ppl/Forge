package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.transform.MultiPassTransformer

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

import scala.collection.mutable.HashMap

// Replaces stores and loads to/from registers of constant/global values with direct references
trait RegisterFolding extends MultiPassTransformer {
  val IR: DHDLExp
  import IR._

  override val name = "Register Folding"
  debugMode = true

  override def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = {
    self_mirror(lhs, rhs) match {
      case lhs2@Def(rhs2) => foldNode(lhs2.asInstanceOf[Sym[A]],rhs2.asInstanceOf[Def[A]])
      case lhs2 => Some(lhs2)
    }
  }

  def foldNode[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = rhs match {
    case EatReflect(Reg_write(reg, value@Exact(_))) =>
      debug(s"Found register write to $reg with value $value")
      foldRegister(reg, value, lhs)
    case EatReflect(Reg_write(reg, value)) if isGlobal(value) =>
      debug(s"Found register write to $reg with value $value")
      foldRegister(reg, value, lhs)
    case EatReflect(Reg_write(reg,value)) =>
      debug(s"Unfoldable register write $lhs = $rhs")
      Some(lhs)
    case _ => Some(lhs)
  }

  def foldRegister(reg: Exp[Reg[Any]], value: Exp[Any], write: Exp[Any]) = {
    if (!isAccum(reg)) {
      debug(s"Replacing references to register $reg with $value")
      readersOf(reg).foreach{case (_,_,read) => register(read -> value) }
      scrubSym(reg.asInstanceOf[Sym[Any]])
      scrubSym(write.asInstanceOf[Sym[Any]])
      Some(value)
    }
    Some(write)
  }
}

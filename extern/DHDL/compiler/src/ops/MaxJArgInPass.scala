package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.{MaxJCodegen}
import scala.virtualization.lms.internal.{Expressions, Traversal}
import ppl.delite.framework.transform.{DeliteTransform}
import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._
import scala.collection.mutable.HashMap

import ppl.delite.framework.DeliteApplication

trait MaxJArgInPass extends Traversal  {
  val IR: LoweredPipeOpsExp with ControllerTemplateOpsExp with TpesOpsExp with ParallelOpsExp
          with PipeOpsExp with OffChipMemOpsExp with RegOpsExp with ExternCounterOpsExp
          with ExternPrimitiveOpsExp with DHDLCodegenOps with NosynthOpsExp with DeliteTransform
	import IR._

  override val name = "MaxJArgInPass"

	val expToArg = HashMap[Exp[Any],Exp[Reg[Any]]]()
	val argToExp = HashMap[Exp[Reg[Any]], Exp[Any]]()

  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
		expToArg.clear
    argToExp.clear
		b
	}
  override def postprocess[A:Manifest](b: Block[A]): Block[A] = {
		b
	}

  override def traverseStm(stm: Stm): Unit = stm match { // override this to implement custom traversal
    case TP(sym, rhs) => {
			traverseNode(sym,rhs)
			super.traverseStm(stm)
		}
    case _ => super.traverseStm(stm)
	}

  def traverseNode(sym: Sym[Any], rhs: Def[Any]):Unit = rhs match {
    case n@Set_arg(reg, value) =>
      expToArg += value -> reg
      argToExp += reg -> value
    case n@Reflect(d,_,_) =>
      traverseNode(sym, d)
    case _ =>
	}
}

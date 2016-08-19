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

trait RegChainPass extends Traversal  {
  val IR: LoweredPipeOpsExp with ControllerTemplateOpsExp with TpesOpsExp with ParallelOpsExp
          with PipeOpsExp with OffChipMemOpsExp with RegOpsExp with ExternCounterOpsExp
          with ExternPrimitiveOpsExp with DHDLCodegenOps with NosynthOpsExp with DeliteTransform
	import IR._

  override val name = "RegChainPass"

	val quoteSuffix = HashMap[Sym[Any], HashMap[Sym[Any], String]]()

  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
		quoteSuffix.clear
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

  def handleQuoteSuffix(children: List[Exp[Any]], inds: List[Sym[Any]]) {
    children.zipWithIndex.foreach { case (c, i) =>
      val localSuffixMap = HashMap[Sym[Any], String]()
      if (i > 0) {
        inds.foreach { idx =>
          localSuffixMap += idx -> s"_chain[${i-1}].read()"
        }
      }
      quoteSuffix += c.asInstanceOf[Sym[Any]] -> localSuffixMap
    }
  }

  def processNode(sym: Sym[Any], inds: List[Sym[Any]]) {
    styleOf(sym.asInstanceOf[Rep[Pipeline]]) match {
      case CoarsePipe =>
        handleQuoteSuffix(childrenOf(sym), inds)
      case _ =>
    }
  }

  def processNodeList(sym: Sym[Any], inds: List[List[Sym[Any]]]) {
    styleOf(sym.asInstanceOf[Rep[Pipeline]]) match {
      case CoarsePipe =>
        inds.foreach { level =>
          handleQuoteSuffix(childrenOf(sym), level)
        }
      case _ =>
    }
  }

  def traverseNode(sym: Sym[Any], rhs: Def[Any]):Unit = rhs match {
    case e@Pipe_foreach(cchain, func, inds) =>
      processNode(sym, inds)

    case e@Pipe_fold(cchain, accum, zero, foldAccum, iFunc, ldFunc, stFunc, func, rFunc, inds, idx, acc, res, rV) =>
      processNode(sym, inds)

    case e@ParPipeForeach(cc, func, inds) =>
      processNodeList(sym, inds)

    case e@ParPipeReduce(cchain, accum, func, rFunc, inds, acc, rV) =>
      processNodeList(sym, inds)

    case n@Reflect(d,_,_) =>
      traverseNode(sym, d)
    case _ =>
	}
}

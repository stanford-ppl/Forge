package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

import ppl.delite.framework.DeliteApplication

trait StageAnalysisExp extends DHDLExp {
  this: DHDLCompiler with DHDLApplication with DeliteApplication =>

  def isOuterController(s: Exp[Any]): Boolean = s match {
    case Def(d) => (isPipeline(d) && styleOf(s) != Fine) || isParallel(d) || isTileTransfer(d)
    case _ => false
  }
  def isInnerController(s: Exp[Any]): Boolean = s match {
    case Def(d) => isPipeline(d) && styleOf(s) == Fine
    case _ => false
  }

  def isTileTransfer(d: Def[Any]): Boolean = d match {
    case _:TileTransfer[_] => true
    case Reflect(d,_,_) => isTileTransfer(d)
    case _ => false
  }
  def isParallel(d: Def[Any]): Boolean = d match {
    case _:Pipe_parallel => true
    case Reflect(d,_,_) => isParallel(d)
    case _ => false
  }
  def isPipeline(d: Def[Any]): Boolean = d match {
    case _:Pipe_foreach => true
    case _:Pipe_reduce[_,_] => true
    case _:Block_reduce[_] => true
    case Reflect(d,_,_) => isPipeline(d)
    case _ => false
  }
}

trait StageAnalyzer extends Traversal {
  val IR: StageAnalysisExp
  import IR._

  override val debugMode: Boolean = true
  override val name = "Stage Analyzer"
  // Bit of a hack here - use scheduling to return list of statements
  private def getStmsInScope(b: Block[Any]): List[Stm] = {
    var stms: List[Stm] = Nil
    focusBlock(b) {
      focusExactScope(b){ levelScope =>
        stms = levelScope
      }
    }
    stms
  }
  def listStages(s: Exp[Any], d: Def[Any])(blks: Block[Any]*): Unit = {
    val stms = blks.toList.flatMap(b => getStmsInScope(b))

    debug(s"$s = $d:")
    stms.zipWithIndex.foreach {
      case (TP(s, d), idx) =>
        if      (isOuterController(s)) debug(s"   $idx. [Ctrl] $s = $d")
        else if (isInnerController(s)) debug(s"   $idx. [Pipe] $s = $d")
        else                           debug(s"   $idx. [None] $s = $d")
    }
  }

  override def traverseStm(stm: Stm) = stm match {
    case TP(s, d) =>
      super.traverseStm(stm)
      traverseNode(s, d)
  }

  def traverseNode(lhs: Exp[Any], rhs: Def[Any]): Unit = rhs match {
    // Parallel
    case Pipe_parallel(func) => listStages(lhs,rhs)(func)

    // MetaPipes / Sequentials
    case Pipe_foreach(_,func,_) if styleOf(lhs) != Fine => listStages(lhs,rhs)(func)
    case Pipe_reduce(c,a,ld,st,func,rFunc,inds,acc,res,rV) if styleOf(lhs) != Fine => listStages(lhs,rhs)(ld,func,rFunc,st)
    case Block_reduce(cc1,cc2,ram,func,i,rFunc,rV1,rV2) => listStages(lhs,rhs)(func,rFunc)

    // Pipe
    case Pipe_foreach(_,func,_) if styleOf(lhs) == Fine => listStages(lhs,rhs)(func)
    case Pipe_reduce(c,a,ld,st,func,rFunc,inds,acc,res,rV) if styleOf(lhs) == Fine => listStages(lhs,rhs)(ld,func,rFunc,st)

    case Reflect(d, _, _) => traverseNode(lhs, d)
    case _ =>
  }
}

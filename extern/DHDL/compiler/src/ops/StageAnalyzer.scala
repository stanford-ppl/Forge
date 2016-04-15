package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.{Traversal,NestedBlockTraversal}
import scala.virtualization.lms.common.EffectExp

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

import ppl.delite.framework.DeliteApplication

trait StageAnalysisExp extends PipeStageToolsExp {
  this: DHDLExp =>
}

trait StageAnalyzer extends Traversal with PipeStageTools {
  val IR: DHDLExp with StageAnalysisExp
  import IR._

  override val debugMode: Boolean = true
  override val name = "Stage Analyzer"

  override def traverseStm(stm: Stm) = stm match {
    case TP(s, d) =>
      super.traverseStm(stm)
      traverseNode(s, d)
  }

  def traverseNode(lhs: Exp[Any], rhs: Def[Any]): Unit = rhs match {
    // Parallel
    case Pipe_parallel(func) =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(func)
      list( stages )
      nStages(lhs) = stages.length

    // MetaPipes / Sequentials
    case Unit_pipe(func) if styleOf(lhs) != Fine =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(func)
      list(stages)
      nStages(lhs) = stages.length

    case Pipe_foreach(_,func,_) if styleOf(lhs) != Fine =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(func)
      list( stages )
      nStages(lhs) = stages.length

    case Pipe_reduce(c,a,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV) if styleOf(lhs) != Fine =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(ld,func,rFunc,st)
      list( stages )
      nStages(lhs) = stages.length + 1  // Account for implicit reduction pipe

    case Block_reduce(c1,c2,a,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV) =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(func,rFunc)
      list( stages )
      nStages(lhs) = stages.length + 1  // Account for implicit reduction pipe

    // Pipe
    /*case Pipe_foreach(_,func,_) if styleOf(lhs) == Fine =>
      debug(s"$lhs = $rhs:")
      list( getControlNodes(func) )
    case Pipe_reduce(c,a,ld,st,func,rFunc,inds,acc,res,rV) if styleOf(lhs) == Fine =>
      debug(s"$lhs = $rhs:")
      list( getControlNodes(ld,func,rFunc,st) )*/

    case Reflect(d, _, _) => traverseNode(lhs, d)
    case _ =>
  }
}

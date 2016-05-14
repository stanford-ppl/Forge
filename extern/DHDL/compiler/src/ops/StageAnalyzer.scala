package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.analysis.HungryTraversal

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait StageAnalysisExp extends PipeStageToolsExp {this: DHDLExp => }

trait StageAnalyzer extends HungryTraversal with PipeStageTools {
  val IR: DHDLExp with StageAnalysisExp
  import IR._

  debugMode = true
  override val name = "Stage Analyzer"
  override val recurseAlways = true  // Always follow default traversal scheme
  override val recurseElse = false   // Follow default traversal scheme when node was not matched

  override def traverse(lhs: Exp[Any], rhs: Def[Any]) = rhs match {
    // Parallel
    case Pipe_parallel(func) =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(func)
      if (debugMode) list(stages)
      nStages(lhs) = stages.length

    // MetaPipes / Sequentials
    case Unit_pipe(func) =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(func)
      if (debugMode) list(stages)
      if (styleOf(lhs) == Fine && stages.nonEmpty) styleOf(lhs) = Coarse
      nStages(lhs) = stages.length

    case Pipe_foreach(_,func,_) =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(func)
      if (debugMode) list( stages )
      if (styleOf(lhs) == Fine && stages.nonEmpty) styleOf(lhs) = Coarse
      nStages(lhs) = stages.length

    case Pipe_fold(c,a,fA,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV) =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(ld,func,rFunc,st)
      if (debugMode) list( stages )
      if (styleOf(lhs) == Fine && stages.nonEmpty) styleOf(lhs) = Coarse
      nStages(lhs) = stages.length + 1  // Account for implicit reduction pipe

    case Accum_fold(c1,c2,a,fA,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV) =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(func,rFunc)
      if (debugMode) list( stages )
      if (styleOf(lhs) == Fine) styleOf(lhs) = Coarse
      nStages(lhs) = stages.length + 1  // Account for implicit reduction pipe

    case _ => super.traverse(lhs, rhs)
  }
}

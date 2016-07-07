package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait StageAnalysisExp extends PipeStageToolsExp {this: DHDLExp => }

trait StageAnalyzer extends Traversal with PipeStageTools {
  val IR: DHDLExp with StageAnalysisExp
  import IR._

  override val name = "Stage Analyzer"
  override val recurse = Always    // Always follow default traversal scheme
  override val eatReflect = true   // Ignore reflect wrappers
  debugMode = false

  override def preprocess[A:Manifest](b: Block[A]) = {
    val stages = getStages(b)
    if (debugMode) list(stages)
    super.preprocess(b)
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    // Accel block (equivalent to a Sequential unit pipe)
    case Hwblock(blk) =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(blk)
      if (debugMode) list(stages)
      nStages(lhs) = stages.length

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
      nStages(lhs) = stages.length

    case Pipe_foreach(_,func,_) =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(func)
      if (debugMode) list( stages )
      nStages(lhs) = stages.length

    case Pipe_fold(c,a,z,fA,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV) =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(ld,func,rFunc,st)
      if (debugMode) list( stages )
      nStages(lhs) = stages.length + 1  // Account for implicit reduction pipe

    case Accum_fold(c1,c2,a,z,fA,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV) =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(func,rFunc)
      if (debugMode) list( stages )
      nStages(lhs) = stages.length + 1  // Account for implicit reduction pipe

    case _ => super.traverse(lhs, rhs)
  }
}

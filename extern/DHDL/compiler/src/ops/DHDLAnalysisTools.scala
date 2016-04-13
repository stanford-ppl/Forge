package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.{Traversal,NestedBlockTraversal}
import scala.virtualization.lms.common.EffectExp

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

import ppl.delite.framework.DeliteApplication

trait PipeStageToolsExp extends EffectExp {
  this: DHDLExp =>

  def isWriter(s: Exp[Any]): Boolean = s match {
    case Def(d) => isWriter(d)
    case _ => false
  }
  def isWriter(d: Def[Any]): Boolean = d match {
    case EatReflect(_:Reg_write[_]) => true
    case EatReflect(_:Bram_store[_]) => true
    case _ => false
  }

  def isReader(s: Exp[Any]): Boolean = s match {
    case Def(d) => isReader(d)
    case _ => false
  }
  def isReader(d: Def[Any]): Boolean = d match {
    case EatReflect(_:Reg_read[_]) => true
    case EatReflect(_:Bram_load[_]) => true
    case _ => false
  }


  def isControlNode(s: Exp[Any]): Boolean = s match {
    case Def(d) => isOuterControl(s) || isInnerControl(s) || isTileTransfer(d)
    case _ => false
  }

  def isOuterControl(s: Exp[Any]): Boolean = s match {
    case Def(d) => (isPipeline(d) && styleOf(s) != Fine) || isParallel(d)
    case _ => false
  }
  def isInnerControl(s: Exp[Any]): Boolean = s match {
    case Def(d) => isPipeline(d) && styleOf(s) == Fine
    case _ => false
  }
  def isAllocation(s: Exp[Any]): Boolean = s match {
    case Def(d) => isAllocation(d)
    case _ => false
  }
  def isAllocation(d: Def[Any]): Boolean = d match {
    case EatReflect(_:Reg_new[_]) => true
    case EatReflect(_:Bram_new[_]) => true
    case EatReflect(_:Offchip_new[_]) => true
    case EatReflect(_:Counter_new) => true
    case EatReflect(_:Counterchain_new) => true
    case _ => false
  }
  def isTileTransfer(d: Def[Any]): Boolean = d match {
    case EatReflect(_:TileTransfer[_]) => true
    case _ => false
  }
  def isParallel(d: Def[Any]): Boolean = d match {
    case EatReflect(_:Pipe_parallel) => true
    case _ => false
  }
  def isPipeline(d: Def[Any]): Boolean = d match {
    case EatReflect(_:Pipe_foreach) => true
    case EatReflect(_:Pipe_reduce[_,_]) => true
    case EatReflect(_:Block_reduce[_]) => true
    case EatReflect(_:Unit_pipe) => true
    case _ => false
  }
}


// Uses scheduling to get all statements in a given block
trait PipeStageTools extends NestedBlockTraversal {
  val IR: PipeStageToolsExp
  import IR._

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

  def list(x: List[Exp[Any]]) = x.zipWithIndex.foreach{
    case (s@Def(d),i) if isControlNode(s)  => println(s"   $i. [Ctrl] $s = $d")
    case (s@Def(d),i) if isInnerControl(s) => println(s"   $i. [Pipe] $s = $d")
    case (s@Def(d),i) if isAllocation(s)   => println(s"   $i. [Allc] $s = $d")
    case (s@Def(d),i)                      => println(s"   $i. [None] $s = $d")
    case (s,i)                             => println(s"   $i. [None] $s")
  }

  def getStages(blks: Block[Any]*): List[Exp[Any]] = {
    blks.toList.flatMap(b => getStmsInScope(b)).map{case TP(s,d) => s}
  }
  def getControlNodes(blk: Block[Any]*) = getStages(blk:_*).filter{s => isControlNode(s) }
  def getAllocations(blk: Block[Any]*)  = getStages(blk:_*).filter{s => isAllocation(s) }

  def getLocalReaders(blk: Block[Any]*) = getStages(blk:_*).filter{s => isReader(s)}
  def getLocalWriters(blk: Block[Any]*) = getStages(blk:_*).filter{s => isWriter(s)}
}


trait CounterToolsExp extends EffectExp {
  this: DHDLExp =>

  def parOf(cc: Rep[CounterChain]): List[Int] = parParamsOf(cc).map(_.x)

  def parParamsOf(cc: Rep[CounterChain]): List[Param[Int]] = cc match {
    case Def(EatReflect(Counterchain_new(ctrs,nIter))) => ctrs.map{
      case Def(EatReflect(Counter_new(_,_,_,par))) => par
    }
  }

  def isUnitCounterChain(e: Exp[Any]): Boolean = e match {
    case Def(EatReflect(Counterchain_new(ctrs,_))) if ctrs.length == 1 => isUnitCounter(ctrs(0))
    case _ => false
  }

  def isUnitCounter(e: Exp[Any]): Boolean = e match {
    case Def(EatReflect(Counter_new(ConstFix(0),ConstFix(1),ConstFix(1),_))) => true
    case _ => false
  }

  // TODO: Default number of iterations if bound can't be computed?
  def nIters(x: Rep[CounterChain]): Long = x match {
    case Def(EatReflect(Counterchain_new(_,nIters))) => Math.ceil( bound(nIters.res).getOrElse(1.0) ).toLong
    case _ => 1L
  }
}



object ReductionTreeAnalysis {
  /*
    Calculate delay line costs:
    a. Determine time (in cycles) any given input or internal signal needs to be delayed
    b. Distinguish each delay line as a separate entity

    Is there a concise equation that can capture this? Haven't been able to come up with one.
    E.g.
      8 inputs => perfectly balanced binary tree, no delay paths
      9 inputs => 1 path of length 3
      85 inputs => 3 paths with lengths 2, 1, and 1
  */
  def reductionTreeDelays(nLeaves: Int): List[Long] = {
    if ( (nLeaves & (nLeaves - 1)) == 0) Nil // Specialize for powers of 2
    // Could also have 2^k + 1 case (delay = 1 path of length k)
    else {
      def reduceLevel(nNodes: Int, completePaths: List[Long], currentPath: Long): List[Long] = {
        if (nNodes <= 1) completePaths  // Stop when 1 node is remaining
        else if (nNodes % 2 == 0) {
          // For an even number of nodes, we don't need any delays - all current delay paths end
          val allPaths = completePaths ++ (if (currentPath > 0) List(currentPath) else Nil)
          reduceLevel(nNodes/2, allPaths, 0L)
        }
        // For odd number of nodes, always delay exactly one signal, and keep delaying that signal until it can be used
        else reduceLevel((nNodes-1)/2 + 1, completePaths, currentPath+1)
      }

      reduceLevel(nLeaves, Nil, 0L)
    }
  }

  def reductionTreeHeight(nLeaves: Int): Int = {
    def treeLevel(nNodes: Int, curHeight: Int): Int = {
      if (nNodes <= 1) curHeight + 1
      else if (nNodes % 2 == 0) treeLevel(nNodes/2, curHeight + 1)
      else treeLevel((nNodes - 1)/2 + 1, curHeight + 1)
    }
    treeLevel(nLeaves, 0)
  }


}

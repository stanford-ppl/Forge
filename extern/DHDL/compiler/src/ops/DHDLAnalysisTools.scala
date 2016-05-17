package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.{Traversal,NestedBlockTraversal}
import scala.virtualization.lms.common.EffectExp

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

import ppl.delite.framework.DeliteApplication

// TODO: Can these be derived automatically?
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
    case Def(d) => isOuterControl(s) || isInnerControl(s)
    case _ => false
  }
  def isOuterControl(s: Exp[Any]): Boolean = s match {
    case Def(d) => isOuterPipeline(s) || isParallel(d)
    case _ => false
  }
  def isInnerControl(s: Exp[Any]): Boolean = s match {
    case Def(d) => isInnerPipeline(s) || isBramTransfer(d) || isOffChipTransfer(d)
    case _ => false
  }

  def isOuterPipeline(s: Exp[Any]): Boolean = s match {
    case Def(d) => isPipeline(d) && styleOf(s) != Fine
    case _ => false
  }
  def isInnerPipeline(s: Exp[Any]): Boolean = s match {
    case Def(d) => isPipeline(d) && styleOf(s) == Fine
    case _ => false
  }

  def isOuterLoop(s: Exp[Any]): Boolean = s match {
    case Def(d) => isLoop(d) && styleOf(s) != Fine
    case _ => false
  }
  def isMetaPipe(s: Exp[Any]): Boolean = isOuterControl(s) && styleOf(s) == Coarse
  def isMetaPipe(s: (Exp[Any],Boolean)): Boolean = !s._2 && isMetaPipe(s._1)

  def isInnerLoop(s: Exp[Any]): Boolean = s match {
    case Def(d) => isLoop(d) && styleOf(s) == Fine
    case _ => false
  }

  def isAllocation(s: Exp[Any]): Boolean = s match {
    case Def(d) => isAllocation(d)
    case _ => false
  }
  def isAllocation(d: Def[Any]): Boolean = d match {
    case EatReflect(_:Reg_new[_])       => true
    case EatReflect(_:Bram_new[_])      => true
    case EatReflect(_:Offchip_new[_])   => true
    case EatReflect(_:Counter_new)      => true
    case EatReflect(_:Counterchain_new) => true
    case _ => false
  }
  def isBramTransfer(d: Def[Any]): Boolean = d match {
    case EatReflect(_:Bram_load_vector[_])  => true
    case EatReflect(_:Bram_store_vector[_]) => true
    case _ => false
  }
  def isOffChipTransfer(d: Def[Any]): Boolean = d match {
    case EatReflect(_:Offchip_load_vector[_])  => true
    case EatReflect(_:Offchip_store_vector[_]) => true
    case _ => false
  }
  def isParallel(d: Def[Any]): Boolean = d match {
    case EatReflect(_:Pipe_parallel) => true
    case _ => false
  }
  def isPipeline(d: Def[Any]): Boolean = d match {
    case EatReflect(_:Pipe_foreach)    => true
    case EatReflect(_:Pipe_fold[_,_])  => true
    case EatReflect(_:Accum_fold[_,_]) => true
    case EatReflect(_:Unit_pipe)       => true
    case _ => false
  }
  def isLoop(d: Def[Any]): Boolean = d match {
    case EatReflect(_:Pipe_foreach)    => true
    case EatReflect(_:Pipe_fold[_,_])  => true
    case EatReflect(_:Accum_fold[_,_]) => true
    case _ => false
  }
}


// Uses scheduling to get all statements in a given block
trait PipeStageTools extends NestedBlockTraversal {
  val IR: PipeStageToolsExp
  import IR._

  def list(x: List[Exp[Any]]) = x.zipWithIndex.foreach{
    case (s@Def(d),i) if isControlNode(s)  => println(s"   $i. [Ctrl] $s = $d")
    case (s@Def(d),i) if isInnerControl(s) => println(s"   $i. [Pipe] $s = $d")
    case (s@Def(d),i) if isAllocation(s)   => println(s"   $i. [Allc] $s = $d")
    case (s@Def(d),i)                      => println(s"   $i. [None] $s = $d")
    case (s,i)                             => println(s"   $i. [None] $s")
  }

  def getStages(blks: Block[Any]*): List[Exp[Any]] = {
    blks.toList.flatMap(b => getStmsInBlock(b)).map{case TP(s,d) => s}
  }
  def getControlNodes(blk: Block[Any]*) = getStages(blk:_*).filter{s => isControlNode(s) }
  def getAllocations(blk: Block[Any]*)  = getStages(blk:_*).filter{s => isAllocation(s) }

  def getLocalReaders(blk: Block[Any]*) = getStages(blk:_*).filter{s => isReader(s)}
  def getLocalWriters(blk: Block[Any]*) = getStages(blk:_*).filter{s => isWriter(s)}
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
      if (nNodes <= 1) curHeight
      else if (nNodes % 2 == 0) treeLevel(nNodes/2, curHeight + 1)
      else treeLevel((nNodes - 1)/2 + 1, curHeight + 1)
    }
    treeLevel(nLeaves, 0)
  }
}

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
    case _:Reg_new[_] => true
    case _:Bram_new[_] => true
    case _:Offchip_new[_] => true
    case _:Counter_new => true
    case _:Counterchain_new => true
    case Reflect(d,_,_) => isAllocation(d)
    case _ => false
  }
  def isTileTransfer(d: Def[Any]): Boolean = d match {
    case _:TileTransfer[_] => true
    case Reflect(_:TileTransfer[_],_,_) => true
    case _ => false
  }
  def isParallel(d: Def[Any]): Boolean = d match {
    case _:Pipe_parallel => true
    case Reflect(_:Pipe_parallel,_,_) => true
    case _ => false
  }
  def isPipeline(d: Def[Any]): Boolean = d match {
    case _:Pipe_foreach => true
    case _:Pipe_reduce[_,_] => true
    case _:Block_reduce[_] => true
    case _:Unit_pipe => true
    case Reflect(d,_,_) => isPipeline(d)
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

  def list(x: List[Exp[Any]]) = x.zipWithIndex.foreach{sym => sym match {
    case (Def(d),i) if isOuterControl(sym._1) => println(s"   $i. [Ctrl] ${sym._1} = $d")
    case (Def(d),i) if isInnerControl(sym._1) => println(s"   $i. [Pipe] ${sym._1} = $d")
    case (Def(d),i) if isAllocation(sym._1)   => println(s"   $i. [Allc] ${sym._1} = $d")
    case (Def(d),i)                           => println(s"   $i. [None] ${sym._1} = $d")
    case (_,i)                                => println(s"   $i. [None] ${sym._1}")
  }}

  def getStages(blks: Block[Any]*): List[Exp[Any]] = {
    blks.toList.flatMap(b => getStmsInScope(b)).map{case TP(s,d) => s}
  }
  def getControlNodes(blk: Block[Any]*) = getStages(blk:_*).filter{s => isControlNode(s) }
  def getAllocations(blk: Block[Any]*)  = getStages(blk:_*).filter{s => isAllocation(s) }
}


trait CounterToolsExp extends EffectExp {
  this: DHDLExp =>

  def parOf(x: Rep[CounterChain]) = x match {
    case Def(EatReflect(Counterchain_new(ctrs,nIter))) => ctrs.map{
      case Def(EatReflect(Counter_new(_,_,_,par))) => par.x
    }
  }

  def isUnitCounter(e: Def[Any]) = e match {
    case EatReflect(Counter_new(ConstFix(0),ConstFix(1),ConstFix(1))) => true
    case _ => false
  }
}

package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

import scala.collection.mutable.HashSet

trait ParameterAnalyzer extends Traversal {
  val IR: DHDLExp
  import IR._

  val tileSizes  = HashSet[Param[Int]]()  // Params used to calculate BRAM size
  val parFactors = HashSet[Param[Int]]()  // Params used as parallelization factors for counters
  val metapipes  = HashSet[Exp[Any]]()    // List of metapipes which can be sequentialized

  override def traverseStm(stm: Stm) = stm match {
    case TP(s, d) =>
      super.traverseStm(stm)
      traverseNode(s, d)
  }
  def traverseNode(lhs: Exp[Any], rhs: Def[Any]) = rhs match {
    case EatReflect(Bram_new(_,_)) =>
      dimsOf(lhs).foreach{case ParamFix(p) => tileSizes += p; case _ =>  }

    case EatReflect(Counter_new(start,end,step,par)) => parFactors += par

    case EatReflect(_:Pipe_foreach) if styleOf(lhs) == Coarse => metapipes += lhs
    case EatReflect(_:Pipe_reduce[_,_]) if styleOf(lhs) == Coarse => metapipes += lhs
    case EatReflect(_:Block_reduce[_]) if styleOf(lhs) == Coarse => metapipes += lhs

    case _ => //
  }
}


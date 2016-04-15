package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._


trait ParSetter extends Traversal {
  val IR: DHDLExp with CounterToolsExp
  import IR._

  var innerLoopPar: Option[Int] = None
  def traverseInner(p: Int)(b: Block[Any]) {
    innerLoopPar = Some(p)
    traverseBlock(b)
    innerLoopPar = None
  }

  override def traverseStm(stm: Stm) = stm match {
    case TP(s, d) => traverseNode(s, d)
  }

  def traverseNode(lhs: Exp[Any], rhs: Def[Any]): Unit = rhs match {
    case EatReflect(Pipe_foreach(cchain, func, _)) if styleOf(lhs) == Fine =>
      val P = parOf(cchain).reduce{_*_}
      traverseInner(P)(func)

    case EatReflect(Pipe_reduce(cchain,_,iFunc,ld,st,func,rFunc,_,_,_,_,_)) if styleOf(lhs) == Fine =>
      val P = parOf(cchain).reduce{_*_}
      traverseInner(P)(iFunc)
      traverseInner(P)(func)
      // Should rFunc have parallelization factor?

    case EatReflect(Block_reduce(ccOuter,ccInner,_,iFunc,func,ld1,ld2,rFunc,st,_,_,_,_,_,_,_)) =>
      val P = parOf(ccInner).reduce{_*_}
      traverseInner(P)(iFunc)
      traverseInner(P)(ld1)
      traverseInner(P)(ld2)
      traverseInner(P)(rFunc)
      traverseInner(P)(st)

    case _ if innerLoopPar.isDefined => par(lhs) = innerLoopPar.get
    case _ => blocks(rhs).foreach{blk => traverseBlock(blk)}
  }
}

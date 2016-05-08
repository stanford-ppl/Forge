package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.analysis.AnalyzerBase

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._


trait ParSetter extends AnalyzerBase {
  val IR: DHDLExp
  import IR._

  override def hasCompleted = runs > 0

  var innerLoopPar: Option[Int] = None
  def traverseInner(p: Int)(b: Block[Any]) {
    innerLoopPar = Some(p)
    traverseBlock(b)
    innerLoopPar = None
  }

  // Custom ordering for block traversal for some nodes
  override def traverseStm(stm: Stm) = analyzeStm(stm)

  override def analyze(lhs: Exp[Any], rhs: Def[Any]): Unit = rhs match {
    case EatReflect(Pipe_foreach(cchain, func, inds)) if styleOf(lhs) == Fine =>
      val P = parOf(cchain).reduce{_*_}
      inds.foreach{i => par(i) = P}
      traverseInner(P)(func)

    case EatReflect(Pipe_fold(cchain,accum,fA,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV)) if styleOf(lhs) == Fine =>
      val P = parOf(cchain).reduce{_*_}
      inds.foreach{i => par(i) = P}
      traverseInner(P)(iFunc)
      traverseInner(P)(func)

    case EatReflect(Accum_fold(ccOuter,ccInner,a,fA,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV)) =>
      val P = parOf(ccInner).reduce{_*_}
      inds2.foreach{i => par(i) = P}
      traverseInner(P)(iFunc)
      traverseInner(P)(ld1)
      traverseInner(P)(ld2)
      traverseInner(P)(rFunc)
      traverseInner(P)(st)

    case _ if innerLoopPar.isDefined => par(lhs) = innerLoopPar.get
    case _ => blocks(rhs).foreach{blk => traverseBlock(blk)}
  }
}

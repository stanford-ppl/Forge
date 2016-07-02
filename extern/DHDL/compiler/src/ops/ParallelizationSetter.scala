package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.analysis.DeliteAnalyzer

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait ParallelizationSetter extends DeliteAnalyzer {
  val IR: DHDLExp
  import IR._

  //debugMode = true
  override val name = "Parallelization Setter"
  override val eatReflect = true
  override def hasCompleted = runs > 0

  var innerLoopPar: Option[Int] = None
  def traverseInner(p: Int)(b: Block[Any]) {
    innerLoopPar = Some(p)
    traverseBlock(b)
    innerLoopPar = None
  }

  override def analyze(lhs: Exp[Any], rhs: Def[Any]): Unit = rhs match {
    case Pipe_foreach(cchain, func, inds) =>
      val Ps = parsOf(cchain)
      inds.zip(Ps).foreach{case (i,p) => parOf(i) = p}

      if (isInnerPipe(lhs)) {
        val P = Ps.reduce{_*_}
        traverseInner(P)(func)
      }
      else traverseBlock(func)

    case Pipe_fold(cchain,accum,zero,fA,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV) =>
      val Ps = parsOf(cchain)
      inds.zip(Ps).foreach{case (i,p) => parOf(i) = p}

      if (isInnerPipe(lhs)) {
        val P = Ps.reduce{_*_}
        traverseInner(P)(iFunc)
        traverseInner(P)(ld)
        traverseInner(P)(st)
        traverseInner(P)(func)
      }
      else blocks(rhs).foreach{blk => traverseBlock(blk)}

    case Accum_fold(ccOuter,ccInner,a,zero,fA,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV) =>
      val Pm = parsOf(ccOuter)
      val Ps = parsOf(ccInner)
      inds1.zip(Pm).foreach{case (i,p) => parOf(i) = p}
      inds2.zip(Ps).foreach{case (i,p) => parOf(i) = p}
      val P = Ps.reduce{_*_}
      traverseInner(P)(iFunc)
      traverseInner(P)(ld1)
      traverseInner(P)(ld2)
      traverseInner(P)(rFunc)
      traverseInner(P)(st)
      traverseBlock(func)

    case _ =>
      if (innerLoopPar.isDefined) parOf(lhs) = innerLoopPar.get
      blocks(rhs).foreach{blk => traverseBlock(blk)}
  }
}

package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

// TODO: Eventually this analyzer should be superceded by more generalized reduction code generation

trait ReductionAnalysisExp extends NodeMetadataOpsExp {
  this: DHDLExp =>

  sealed abstract class ReduceFunction
  case object FixPtSum extends ReduceFunction
  case object FltPtSum extends ReduceFunction
  case object FixPtMin extends ReduceFunction
  case object FixPtMax extends ReduceFunction

  case class MReduceType(func: ReduceFunction) extends Metadata

  object reduceType {
    def update(e: Exp[Any], func: ReduceFunction) = setMetadata(e, MReduceType(func))
    def apply(e: Exp[Any]) = meta[MReduceType](e).map(_.func)
  }

  def identifyReduceFunc(rFunc: Block[Any], a: Exp[Any], b: Exp[Any]): Option[ReduceFunction] = getBlockResult(rFunc) match {
    case Deff(FixPt_Add(`a`,`b`)) => Some(FixPtSum)
    case Deff(FltPt_Add(`a`,`b`)) => Some(FltPtSum)
    case Deff(Min2(`a`,`b`)) if isFixPtType(a.tp) => Some(FixPtMin)
    case Deff(Max2(`a`,`b`)) if isFixPtType(a.tp) => Some(FixPtMax)
    case _ => None
  }
}

trait ReductionAnalyzer extends Traversal with SpatialTraversalTools {
  val IR: DHDLExp with ReductionAnalysisExp
  import IR._

  override val name = "Reduction Analyzer"
  override val recurse = Always    // Always follow default traversal scheme
  override val eatReflect = true   // Ignore reflect wrappers

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case Pipe_fold(c,a,z,fA,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV) =>
      identifyReduceFunc(rFunc, rV._1, rV._2).foreach{funcType =>
        // Not sure where Raghu needs this info, so just sticking it everywhere
        getStages(rFunc).foreach{sym => reduceType(sym) = funcType}
        reduceType(a) = funcType
        reduceType(acc) = funcType
        reduceType(lhs) = funcType
      }

    // TODO: Necessary here?
    case Accum_fold(c1,c2,a,z,fA,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV) =>
      identifyReduceFunc(rFunc, rV._1, rV._2).foreach{funcType =>
        // Not sure where Raghu needs this info, so just sticking it everywhere
        getStages(rFunc).foreach{sym => reduceType(sym) = funcType}
        reduceType(a) = funcType
        reduceType(acc) = funcType
        reduceType(lhs) = funcType
      }

    case _ => super.traverse(lhs, rhs)
  }
}

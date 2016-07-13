package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import ppl.delite.framework.analysis.{AffineAnalysisExp, AffineAnalyzer}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait DHDLAffineAnalysisExp extends AffineAnalysisExp {
  this: DHDLExp =>

  def isIndexType(t: Manifest[_]) = isFixPtType(t) && sign(t) && nbits(t.typeArguments(1)) == 32 && nbits(t.typeArguments(2)) == 0

  // Pair of symbols for nodes used in address calculation addition nodes
  override def indexPlusUnapply(x: Exp[Index]): Option[(Exp[Index], Exp[Index])] = x match {
    case Deff(FixPt_Add(a,b)) => Some((a.asInstanceOf[Exp[Index]],b.asInstanceOf[Exp[Index]])) // annoying erasure here
    case _ => None
  }
  // Pair of symbols for nodes used in address calculation multiplication nodes
  override def indexTimesUnapply(x: Exp[Index]): Option[(Exp[Index], Exp[Index])] = x match {
    case Deff(FixPt_Mul(a,b)) => Some((a.asInstanceOf[Exp[Index]],b.asInstanceOf[Exp[Index]]))
    case _ => None
  }
  // List of loop scopes. Each scope contains a list of iterators and blocks to traverse for loop nodes
  override def loopUnapply(x: Exp[Any]): Option[List[(List[Sym[Index]], List[Block[Any]])]] = x match {
    case Deff(Pipe_foreach(cchain, func, inds)) =>
      Some( List(inds -> List(func)) )
    case Deff(Pipe_fold(cchain,accum,zero,fA,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV)) =>
      Some( List(inds -> List(iFunc,ld,st,func,rFunc)) )
    case Deff(Accum_fold(c1,c2,a,zero,fA,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV)) =>
      Some( List(inds1 -> List(func), (inds1 ++ inds2) -> List(iFunc,ld1,ld2,rFunc,st)) )
    case _ => None
  }
  // Memory being read + list of addresses (for N-D access)
  override def readUnapply(x: Exp[Any]): Option[(Exp[Any], List[Exp[Index]])] = x match {
    case Deff(Bram_load(bram,addr)) => Some((bram, accessIndicesOf(x)))
    case _ => None
  }
  // Memory being written + list of addresses (for N-D access)
  override def writeUnapply(x: Exp[Any]): Option[(Exp[Any], List[Exp[Index]])] = x match {
    case Deff(Bram_store(bram,addr,y)) => Some((bram, accessIndicesOf(x)))
    case _ => None
  }
  // Usually None, but allows other exceptions for symbols being loop invariant
  override def invariantUnapply(x: Exp[Index]): Option[Exp[Index]] = x match {
    case Exact(_) => Some(x)  // May not be constant yet but will be in future
    case _ => super.invariantUnapply(x)
  }
}

trait DHDLAffineAnalyzer extends AffineAnalyzer {
  val IR: DHDLAffineAnalysisExp with DHDLExp
  import IR._

  private def patternOfVectorizedOp(offsets: List[Exp[FixPt[Signed,B32,B0]]], inds: List[Sym[FixPt[Signed,B32,B0]]]) = {
    val trueOffsets = offsets.take(offsets.length - inds.length).map{x => InvariantAccess(x)}
    val indices = inds.zip(offsets.drop(trueOffsets.length)).map{
      case (i,Exact(0)) => LinearAccess(i)
      case (i,b) => OffsetAccess(i,b)
    }
    trueOffsets ++ indices
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Pipe_fold(cc,a,zero,fA,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV) =>
      super.traverse(lhs,rhs)
      accessIndicesOf(lhs) = inds
      accessPatternOf(lhs) = inds.map{i => LinearAccess(i)}

    case Accum_fold(cc1,cc2,a,zero,fA,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV) =>
      super.traverse(lhs,rhs)
      accessIndicesOf(lhs) = inds2
      accessPatternOf(lhs) = inds2.map{i => LinearAccess(i)}

    case _ => super.traverse(lhs,rhs)
  }
}

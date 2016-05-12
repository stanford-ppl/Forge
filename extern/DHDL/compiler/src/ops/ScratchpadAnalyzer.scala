package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import ppl.delite.framework.analysis.HungryTraversal
import scala.virtualization.lms.common.EffectExp

import scala.collection.mutable.HashMap

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait ScratchpadToolsExp extends DHDLAffineAnalysisExp {
  this: DHDLExp =>

  // stride - number of contiguous elements mapped to the same bank

  /**
    Metadata for various banking techniques for vectorization of reads/writes of
    a single scratchpad.

    Examples for an RxC scratchpad:
     Access      Vectorization
      (i)             (1)         Hierarchical(1, 1)
     (i,j)           (1,4)        Hierarchical(1, 4)
     (i,j)           (4,1)        Hierarchical(C, 4)
     (i,j)           (4,4)        Hierarchical((C,1), (4,4)) // Does order matter here?
      b(i)            (4)         Duplicated(4)
   (i,j)+(j,i)   (1,4) + (4,1)    MultiWay((C,1), 4)
  */

  sealed abstract class BankingFormat
  // Optimization for doing things like diagonal banking for a memory accessed
  // both row- and column-wise.
  case class MultiWayBanks(strides: List[Int], banks: Int, bankFormat: BankingFormat) extends BankingFormat
  // "Hierarchical" banking scheme (banks of banks of ...)
  // Includes simple, 1D case
  case class StridedBanks(stride: Int, banks: Int, bankFormat: BankingFormat) extends BankingFormat
  // "Banking" via duplication - used when the reader and writer access patterns
  // are either incompatible or unpredictable
  case class DuplicatedMems(banks: Int, dupFormat: BankingFormat) extends BankingFormat

  case object SingleBank extends BankingFormat

  object BankingFormat {
    def fromAccessPattern(pattern: List[IndexPattern], pars: List[Int], strides: List[Int]): BankingFormat = {
      if (pattern.isEmpty) SingleBank
      else pattern.head match {
        case AffineAccess(Exact(a),i,b) => StridedBanks(a*strides.head, pars.head, BankingFormat.fromAccessPattern(pattern.tail,pars.tail,strides.tail))

      }
    }
  }

  /**
    Metadata for duplicated instances of a single coherent scratchpad. When possible,
    address generators should be coalesced to a single instance. However, this is only
    possible when the addresses can be guaranteed to be in lockstep.
  */
  case class MemInstance(depth: Int, bankFormat: BankingFormat)

  def unpairedAccess(access: (Exp[Any],Boolean,Exp[Any])): MemInstance = accessPatternOf(access._3) match {
    case AffineAccess(a,i,b) =>
    case OffsetAccess(i,b) =>
    case StridedAccess(a,i) =>
    case LinearAccess()
  }
  def pairedAccess(write: (Exp[Any],Boolean,Exp[Any]), read: (Exp[Any],Boolean,Exp[Any])): MemInstance = {

  }

  def getMemoryInstances(mem: Exp[Any]) = (writerOf(mem),readersOf(mem)) match {
    case (None, Nil) => Nil
    case (Some(write), Nil) => List(unpairedAccess(write))
    case (None, reads) =>
      reads.map(read => unpairedAccess(read))
    case (Some(write), reads) =>
      reads.map(read => pairedAccess(write, read))
  }



  def inferDoubleBuffers(localMems: List[Exp[Any]]) = {
    // Heuristic - find memories which have a reader and a writer which are different
    // but whose nearest common parent is a metapipeline.
    localMems.flatMap{mem =>
      writerOf(mem).flatMap { writer =>
        val lcas = readersOf(mem).filter(_ != writer).flatMap{reader => leastCommonAncestor(reader, writer, parentOf).filter(isMetaPipe(_)) }
        if (lcas.isEmpty) None else Some(mem -> lcas.head) // HACK: This could actually be much more complicated..
      }
    }
  }

}

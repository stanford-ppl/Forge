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


  /**
    Metadata for various banking techniques for vectorization of reads/writes of
    a single scratchpad.

    stride - number of contiguous elements mapped to the same bank
    banks  - number of independent address generators corresponding to a single vectorized load
    depth  - number of writes that occur before one read (number of blocks needed to be saved)

    val bram = BRAM[Fix](16,32)
    Pipe(N until 1 par P){i =>    // domain of P should be (1,1,1) if this includes random *writes* to bram
      val a = Reg[Idx]
      Pipe { a := ... }
      Pipe(32 until 1 par Q){j => bram(j) = ... }
      Pipe(...)
    }

    Should be allowed to bank (but not duplicate) writes to outer BRAMs -> restriction on params, not banking
    Otherwise need some more complicated coherency protocol
    Also should have a multiFold in DHDL?

    Examples for an RxC scratchpad:
     Access      Vectorization
      (i)             (1)         Strided(1, 1)
     (i,j)           (1,4)        Strided(1, 4)
     (i,j)           (4,1)        Strided(C, 4)
     (i,j)           (4,4)        Strided((C,1), (4,4)) // Does order matter here?
      b(i)            (4)         Duplicated(4)
    (b(i), i)        (4,4)        Duplicated(4)
   (b(i), c(i))      (4,4)        Duplicated(4)
   (i,j)&(j,i)    (1,4)&(4,1)    MultiWay((C,1), 4)
  
    Data (2x2)        Stride 1        Stride 4        Diagonal
                B  0 | 1 | 2 | 3   0 | 1 | 2 | 3   0 | 1 | 2 | 3
                  --------------- --------------- ---------------
     0 1 2 3       0 | 1 | 2 | 3   0   4   8   C   0   1   2   3
     4 5 6 7       4 | 5 | 6 | 7   1   5   9   D   7   4   5   6
     8 9 A B       8 | 9 | A | B   2   6   A   E   A   B   8   9
     C D E F       C | D | E | F   3   7   B   F   D   E   F   C
                   B = addr%S      B = (addr/S)%S  B = (i + j)%S
                   A = addr/S      A = addr%S      A = 
  */

  sealed abstract class BankingFormat
  // Optimization for doing things like diagonal banking for a memory accessed
  // both row- and column-wise.
  case class MultiWayBanks(strides: List[Int], banks: Int) extends BankingFormat
  // Strided banking scheme. Includes simple, 1D case
  case class StridedBanks(stride: Int, banks: Int) extends BankingFormat
  // "Banking" via duplication - used when the reader and writer access patterns
  // are either incompatible or unpredictable
  case class DuplicatedMems(banks: Int) extends BankingFormat
  // Not a real banking scheme - used to indicate wildcard banking scheme with some par factor
  case class FlexibleBanks(banks: Int) extends BankingFormat

  object BankingFormat {
    def fromAccessPattern(mem: Exp[Any], indices: List[Exp[Any]], pattern: List[IndexPattern]): BankingFormat = {
      val strides = constDimsToStrides(dimsOf(mem).map{case Exact(d) => d})

      (pattern, indices, strides).zipped.map{ case (pattern, index, stride) => pattern match {
        case AffineAccess(Exact(a),i,b) => StridedBanks(a*stride, parOf(i))
        case StridedAccess(Exact(a),i) => StridedBanks(a*stride, parOf(i))
        case OffsetAccess(i,b) => StridedBanks(stride, parOf(i))
        case LinearAccess(i,b) => StridedBanks(stride, parOf(i))
        case InvariantAccess(b) => DuplicatedMems(parOf(index))
        case RandomAccess => DuplicatedMems(parOf(index))
        case FlexibleAccess => FlexibleBanks(parOf(index))
      }}
    }
  }

  /**
    Metadata for duplicated instances of a single coherent scratchpad. When possible,
    address generators should be coalesced to a single instance. However, this is only
    possible when the addresses can be guaranteed to be in lockstep.
  */
  case class MemInstance(depth: Int, bankFormat: BankingFormat)

  def unpairedAccess(access: (Exp[Any],Boolean,Exp[Any])): MemInstance = {


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

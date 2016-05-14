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
                   B = addr%N      B = (addr/S)%S  B = (i + j)%S
                   A = addr/N      A =             A =

     Stride 1  Stride 4     Diagonal
   B  0 | 1     0 | 1        0 | 1
     -------   -------      -------
      0   1     0   4        0   1
      2   3     1   5        2   3
      4   5     2   6        7   4
      6   7     3   7        5   6
      8   9     8   C        8   9
      A   B     9   D        A   B
      C   D     A   E        D   E
      E   F     B   F        F   C
  B:   j%N       i%N        (i+j)%N
  A:(i*S+j)/N  (i/N)*S+j      ???
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

  def bankingFromAccessPattern(mem: Exp[Any], indices: List[Exp[Any]], pattern: List[IndexPattern]) = {
    val strides = constDimsToStrides(dimsOf(mem).map{case Exact(d) => d})

    (pattern, indices, strides).zipped.map{ case (pattern, index, stride) => pattern match {
      case AffineAccess(Exact(a),i,b) => StridedBanks(a*stride, parOf(i))
      case StridedAccess(Exact(a),i) => StridedBanks(a*stride, parOf(i))
      case OffsetAccess(i,b) => StridedBanks(stride, parOf(i))
      case LinearAccess(i,b) => StridedBanks(stride, parOf(i))
      case InvariantAccess(b) => DuplicatedMems(parOf(index))
      case RandomAccess => DuplicatedMems(parOf(index))
    }}
  }

  /**
    Metadata for duplicated instances of a single coherent scratchpad. When possible,
    address generators should be coalesced to a single instance. However, this is only
    possible when the addresses can be guaranteed to be in lockstep.
  */
  case class MemInstance(depth: Int, bankFormat: BankingFormat)

  def unpairedAccess(mem: Exp[Any], access: (Exp[Any],Boolean,Exp[Any])): MemInstance = {
    val banking = bankingFromAccessPattern(mem, accessIndicesOf(access._3), accessPatternOf(access._3))
    MemInstance(1, banking)
  }
  def pairedAccess(mem: Exp[Any], write: (Exp[Any],Boolean,Exp[Any]), read: (Exp[Any],Boolean,Exp[Any])): MemInstance = {
    val bankWrite = bankingFromAccessPattern(mem, accessIndicesOf(write._3), accessPatternOf(write._3))
    val bankRead  = bankingFromAccessPattern(mem, accessIndicesOf(read._3), accessPatternOf(read._3))

  }

  // TODO: How to express "tapped" block FIFO?
  def coalesceDuplicates(dups: List[MemInstance]) = dups


  def getMemoryInstances(mem: Exp[Any]) = (writerOf(mem),readersOf(mem)) match {
    case (None, Nil) => Nil
    case (Some(write), Nil) => List(unpairedAccess(mem, write))
    case (None, reads) =>
      // What to do here? No writer, so current version will always read garbage...
      val dups = reads.map(read => unpairedAccess(mem, read))
      coalesceDuplicates(dups)
    case (Some(write), reads) =>
      val dups = reads.map(read => pairedAccess(mem, write, read))
      coalesceDuplicates(dups)
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

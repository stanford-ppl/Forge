package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import ppl.delite.framework.analysis.HungryTraversal
import scala.virtualization.lms.common.EffectExp
import scala.virtualization.lms.util.GraphUtil

import scala.collection.mutable.HashMap

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait ScratchpadToolsExp extends DHDLAffineAnalysisExp with PipeStageToolsExp {
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

  sealed abstract class Banking(val banks: Int)
  object Banking {
    def unapply(x: Banking): Option[Int] = Some(x.banks)
  }
  // Optimization for doing things like diagonal banking for a memory accessed
  // both row- and column-wise.
  case class MultiWayBanking(strides: List[Int], override val banks: Int) extends Banking(banks)
  // Strided banking scheme. Includes simple, 1D case
  case class StridedBanking(stride: Int, override val banks: Int) extends Banking(banks)
  // "Banking" via duplication - used when the reader and writer access patterns
  // are either incompatible or unpredictable
  case class DuplicatedBanking(override val banks: Int) extends Banking(banks)

  def bankingFromAccessPattern(mem: Exp[Any], indices: List[Exp[Any]], pattern: List[IndexPattern]): List[Banking] = {
    val strides = constDimsToStrides(dimsOf(mem).map{case Exact(d) => d.toInt})

    (pattern, indices, strides).zipped.map{ case (pattern, index, stride) => pattern match {
      case AffineAccess(Exact(a),i,b) => StridedBanking(a.toInt*stride, parOf(i))
      case StridedAccess(Exact(a),i) => StridedBanking(a.toInt*stride, parOf(i))
      case OffsetAccess(i,b) => StridedBanking(stride, parOf(i))
      case LinearAccess(i) => StridedBanking(stride, parOf(i))
      case InvariantAccess(b) => DuplicatedBanking(parOf(index))
      case RandomAccess => DuplicatedBanking(parOf(index))
    }}
  }

  /**
    Metadata for duplicated instances of a single coherent scratchpad. When possible,
    address generators should be coalesced to a single instance. However, this is only
    possible when the addresses can be guaranteed to be in lockstep.
  */
  case class MemInstance(depth: Int, banking: List[Banking])

  case class MemDuplicates(insts: List[MemInstance]) extends Metadata
  object duplicatesOf {
    def update(e: Exp[Any], m: List[MemInstance]) { setMetadata(e, MemDuplicates(m)) }
    def apply(e: Exp[Any]) = meta[MemDuplicates](e).map(_.insts).getOrElse(Nil)
  }

  def unpairedAccess(mem: Exp[Any], access: (Exp[Any],Boolean,Exp[Any])): MemInstance = {
    val banking = bankingFromAccessPattern(mem, accessIndicesOf(access._3), accessPatternOf(access._3))
    MemInstance(1, banking)
  }
  def pairedAccess(mem: Exp[Any], write: (Exp[Any],Boolean,Exp[Any]), read: (Exp[Any],Boolean,Exp[Any])): MemInstance = {
    val dims = dimsOf(mem)

    val bankWrite = bankingFromAccessPattern(mem, accessIndicesOf(write._3), accessPatternOf(write._3))
    val bankRead  = bankingFromAccessPattern(mem, accessIndicesOf(read._3), accessPatternOf(read._3))

    var banking: List[Banking] = Nil
    var i: Int = 0

    def matchSingle(write: Banking, read: Banking) = (write,read) match {
      case (StridedBanking(s1,p), StridedBanking(s2,q)) if s1 == s2 => StridedBanking(s1, lcm(p,q))
      case (Banking(p), Banking(q))                                 => DuplicatedBanking(lcm(p,q))
    }

    // TODO: Only properly defined for up to 2 dimensions right now?
    if (bankWrite.length == bankRead.length) {
      var i = 0
      while (i < bankWrite.length) {
        if (i < bankWrite.length - 1) {
          // Special case for creating diagonally banked memories
          (bankWrite(i),bankWrite(i+1),bankRead(i),bankRead(i+1)) match {
            case (Banking(1),StridedBanking(s1,p), StridedBanking(s2,q),Banking(1)) =>
              banking ::= MultiWayBanking(List(s2,s1), lcm(p,q))
              i += 2
            case (StridedBanking(s1,p),Banking(1), Banking(1),StridedBanking(s2,q)) =>
              banking ::= MultiWayBanking(List(s1,s2), lcm(p,q))
              i += 2

            case _ =>
              banking ::= matchSingle(bankWrite(i),bankRead(i))
              i += 1
          }
        }
        else {
          banking ::= matchSingle(bankWrite(i), bankRead(i))
          i += 1
        }
      }
    }
    else if (bankWrite.length == 1 || bankRead.length == 1) {
      // This one's a little easier to address, but not clear how arbitrary banking should be done here
      stageError("Memory " + nameOf(mem).getOrElse("") + " defined here is treated as both " + bankWrite.length + "D and " + bankRead.length + "D. This is currently unsupported.")
    }
    else stageError("Memory " + nameOf(mem).getOrElse("") + " defined here is treated as both " + bankWrite.length + "D and " + bankRead.length + "D. This is currently unsupported.")

    MemInstance(1 + distanceBetween((write._1,write._2), (read._1,read._2)), banking.reverse)
  }

  def lcm(a: Int, b: Int): Int = {
    val bigA = BigInt(a)
    val bigB = BigInt(b)
    (bigA*bigB / bigA.gcd(bigB)).intValue() // Unchecked overflow, but hey...
  }

  // Defined as the number of coarse-grained pipeline stages between the read and the write
  // In other words, the number of writes that can happen to a memory before the first read happens
  def distanceBetween(write: (Exp[Any], Boolean), read: (Exp[Any], Boolean)): Int = {
    if (write == read) 0
    else {
      val (lca, writePath, readPath) = GraphUtil.leastCommonAncestorWithPaths[(Exp[Any],Boolean)](write, read, {node => parentOf(node)})

      if (lca.isDefined) {
        if (isMetaPipe(lca.get)) {
          val children = childrenOf(lca.get._1)
          val wIdx = children.indexOf(writePath.head)
          val rIdx = children.indexOf(readPath.head)
          Math.abs(rIdx - wIdx) // write may happen after read in special cases - what to do there?
        }
        else 0
      }
      else stageError("No common controller found between read and write") // TODO: Would need better error here
    }
  }

  // TODO: How to express "tapped" block FIFO? What information is needed here?
  def coalesceDuplicates(dups: List[MemInstance]) = dups


  // TODO: Also need pointer for each read/write to refer to instance(s) it accesses
  def getMemoryInstances(mem: Exp[Any]): List[MemInstance] = {
    if (isBRAM(mem.tp)) {
      (writerOf(mem),readersOf(mem)) match {
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
    }
    else if (isRegister(mem.tp)) {    // Registers don't need banking, but they do need buffering
      (writerOf(mem), readersOf(mem)) match {
        case (None, Nil) => Nil
        case (Some(write), Nil) => List(MemInstance(1, List(DuplicatedBanking(1))))
        case (None, reads) => List(MemInstance(1, List(DuplicatedBanking(1))))
        case (Some(write), reads) =>
          val dups = reads.map{read => MemInstance(1 + distanceBetween( (write._1,write._2), (read._1,read._2) ), List(DuplicatedBanking(1))) }
          coalesceDuplicates(dups)
      }
    }
    else stageError("TODO: Don't yet know how to bank memory of type " + mem.tp)
  }



  def inferBuffers(localMems: List[Exp[Any]]) = {
    localMems.foreach{mem =>
      val instances = getMemoryInstances(mem)
      duplicatesOf(mem) = instances

      stageInfo("Inferred " + instances.length + " instances for memory " + nameOf(mem).getOrElse("") + " defined here: ")(mpos(mem.pos))
      instances.zip(readersOf(mem)).zipWithIndex.foreach { case ((inst,read), i) =>
        stageInfo("  Inst #" + i + ": Depth: " + inst.depth + ", Format: " + inst.banking.mkString(", "))(mpos(read._3.pos))
      }
    }
    // Heuristic - find memories which have a reader and a writer which are different
    // but whose nearest common parent is a metapipeline.
    /*localMems.flatMap{mem =>
      writerOf(mem).flatMap { writer =>
        val lcas = readersOf(mem).filter(_ != writer).flatMap{reader => leastCommonAncestor(reader, writer, parentOf).filter(isMetaPipe(_)) }
        if (lcas.isEmpty) None else Some(mem -> lcas.head) // HACK: This could actually be much more complicated..
      }
    }*/
  }

}

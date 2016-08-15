package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.Traversal
import scala.virtualization.lms.common.EffectExp
import scala.virtualization.lms.util.GraphUtil

import scala.collection.mutable.HashMap

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait MemoryAnalysisExp extends DHDLAffineAnalysisExp with ControlSignalAnalysisExp {
  this: DHDLExp =>

  var fold_in_out_accums = HashMap[Exp[Any],Exp[Any]]()

  // TODO
  def isDblBuf(e: Exp[Any]) = duplicatesOf(e).headOption match {
    case Some(meminst) => meminst.depth == 2
    case _ => false
  }
  def banks(e: Exp[Any]) = duplicatesOf(e).headOption match {
    case Some(meminst) => meminst.banking.head.banks
    case _ => 1
  }

  sealed abstract class Banking(val banks: Int)
  object Banking {
    def unapply(x: Banking): Option[Int] = Some(x.banks)
  }
  // Optimization for doing things like diagonal banking for a memory accessed
  // both row- and column-wise.
  case class DiagonalBanking(strides: List[Int], override val banks: Int) extends Banking(banks)
  // Strided banking scheme. Includes simple, 1D case
  case class StridedBanking(stride: Int, override val banks: Int) extends Banking(banks)
  // No banking
  case object NoBanking extends Banking(1)

  /**
    Metadata for duplicated instances of a single coherent scratchpad. When possible,
    address generators should be coalesced to a single instance. However, this is only
    possible when the addresses can be guaranteed to be in lockstep.
  */
  case class MemInstance(depth: Int, duplicates: Int, banking: List[Banking])

  def SimpleInstance = MemInstance(1, 1, List(NoBanking))

  case class MemDuplicates(insts: List[MemInstance]) extends Metadata
  object duplicatesOf {
    def update(e: Exp[Any], m: List[MemInstance]) { setMetadata(e, MemDuplicates(m)) }
    def apply(e: Exp[Any]) = meta[MemDuplicates](e).map(_.insts).getOrElse(Nil)
  }


  /**
    Metadata for determining which memory instance a reader should correspond to.
    Needed to preserve mapping after unrolling
  */
  case class MemInstanceIndex(mapping: Map[Exp[Any], Int]) extends Metadata

  object instanceIndexOf {
    def update(reader: Exp[Any], mem: Exp[Any], idx: Int) = instanceIndexOf.get(reader) match {
      case Some(map) =>
          val newMap = map.filterKeys(_ != mem) + (mem -> idx)
          setMetadata(reader, MemInstanceIndex(newMap))
      case None => 
          setMetadata(reader, MemInstanceIndex(Map(mem -> idx)))
    }
    def get(reader: Exp[Any]) = meta[MemInstanceIndex](reader).map(_.mapping)
    def get(reader: Exp[Any], mem: Exp[Any]) = meta[MemInstanceIndex](reader).flatMap(_.mapping.get(mem))

    def apply(reader: Exp[Any], mem: Exp[Any]) = meta[MemInstanceIndex](reader).get.mapping.apply(mem)
  }

  override def mirror[T<:Metadata](m: T, f: Transformer): T = m match {
    case MemInstanceIndex(map) =>
      MemInstanceIndex(map.map{case (mem,idx) => f(mem) -> idx }).asInstanceOf[T]

    case _ => super.mirror(m,f)
  }

}

// Technically doesn't need a real traversal, but nice to have debugging, etc.
trait BankingBase extends Traversal {
  val IR: DHDLExp with MemoryAnalysisExp
  import IR._

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
      debug("    write: " + write)
      debug("    read:  " + read)

      val (lca, writePath, readPath) = GraphUtil.leastCommonAncestorWithPaths[(Exp[Any],Boolean)](write, read, {node => parentOf(node)})

      debug("    lca: " + lca)
      debug("    write path: " + writePath.mkString(", "))
      debug("    read path: " + readPath.mkString(", "))

      if (lca.isDefined) {
        if (isMetaPipe(lca.get)) {
          val parent = lca.get._1
          // FIXME: Reads and writes owned by the parent are assumed to occur at controller setup time
          if (write == lca && read != write) stageWarn("The parent of the write node here is the LCA - likely a bug")
          if (read  == lca && read != write) stageWarn("The parent of the read node here is the LCA - likely a bug")

          val children = lca +: childrenOf(parent).map{x => (x,false)} :+ ((parent,true))

          // FIXME: (#2) Metapipe children assumed to be a linear sequence of stages, not an arbitrary graph
          debug("    lca children: " + children.mkString(", "))
          val wIdx = children.indexOf(writePath.head)
          val rIdx = children.indexOf(readPath.head)
          if (wIdx < 0 || rIdx < 0) stageError("FIXME: Bug in scratchpad analyzer")

          Math.abs(rIdx - wIdx) // write may happen after read in special cases
        }
        else 0
      }
      else stageError("No common controller found between read and write") // TODO: Would need better error here
    }
  }

  // TODO: How to handle non-matching strides? Just don't bank?
  def matchBanking(write: Banking, read: Banking) = (write,read) match {
    case (StridedBanking(s1,p), StridedBanking(s2,q)) if s1 == s2 => StridedBanking(s1, lcm(p,q))
    case (Banking(1), banking)                                    => banking
    case (banking, Banking(1))                                    => banking
    case _ => stageError("TODO: What to do for non-matching strided banking?")
  }

  // TODO: How to express "tapped" block FIFO? What information is needed here?
  def coalesceDuplicates(mem: Exp[Any], reads: List[Exp[Any]], insts: List[MemInstance]) = {
    reads.zipWithIndex.foreach{case (read, idx) => instanceIndexOf(read, mem) = idx }
    insts
  }

  def bank(mem: Exp[Any]): List[MemInstance] = stageError("Don't know how to bank memory of type " + mem.tp)
}

/**
    BRAM BANKING
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
      b(i)            (4)         Duplicate(4)
    (b(i), i)        (4,4)        Duplicate(4), Strided(1, 4)
   (b(i), c(i))      (4,4)        Duplicate(4), Duplicate(1)
   (i,j)&(j,i)    (1,4)&(4,1)     Diagonal((C,1), 4)

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
  A:(i*C+j)/N  (i/N)*C+j      ???
**/
trait BRAMBanking extends BankingBase {
  import IR._

  def bankingFromAccessPattern(mem: Exp[Any], access: Exp[Any]) = {
    val isWrite = isWriter(access)
    val indices = accessIndicesOf(access)
    val pattern = accessPatternOf(access)
    val allStrides = constDimsToStrides(dimsOf(mem).map{case Exact(d) => d.toInt})
    val strides = if (indices.length == 1) List(allStrides.last) else allStrides

    val Def(d) = access
    debug(s"  access:  $access = $d")
    debug(s"  indices: $indices")
    debug(s"  pattern: $pattern")

    val factors = unrollFactorsOf(access) diff unrollFactorsOf(mem) // Parallelization factors relative to this memory

    debug(s"  factors: ${factors}")

    var used: Map[Exp[Any], Boolean] = Map.empty
    factors.foreach{factor => used += factor -> false}

    def bankFactor(i: Exp[Any]): Int = {
      val factor = parFactorOf(i)
      if (used.contains(factor) && !used(factor)) {
        used += factor -> true
        parOf(i)
      }
      else 1
    }

    val banking = (pattern, indices, strides).zipped.map{ case (pattern, index, stride) => pattern match {
      case AffineAccess(Exact(a),i,b) => StridedBanking(a.toInt*stride, bankFactor(i))
      case StridedAccess(Exact(a),i)  => StridedBanking(a.toInt*stride, bankFactor(i))
      case OffsetAccess(i,b)          => StridedBanking(stride, bankFactor(i))
      case LinearAccess(i)            => StridedBanking(stride, bankFactor(i))
      case InvariantAccess(b)         => NoBanking // Single "bank" in this dimension
      case RandomAccess               => NoBanking // Single "bank" in this dimension
    }}

    val duplicates = factors.filter{factor => !used(factor)}.map{case Exact(p) => p.toInt}.fold(1){_*_}

    MemInstance(1, duplicates, banking)
  }

  def unpairedAccess(mem: Exp[Any], access: (Exp[Any],Boolean,Exp[Any])): MemInstance = {
    val memInstance = bankingFromAccessPattern(mem, access._3)
    debug(" - Unpaired access " + access + ", inferred banking " + memInstance)
    memInstance
  }

  def pairedAccess(mem: Exp[Any], write: (Exp[Any],Boolean,Exp[Any]), read: (Exp[Any],Boolean,Exp[Any])): MemInstance = {
    val dims = dimsOf(mem)

    debug("  Write " + write + ", read " + read)
    val writeInstances = bankingFromAccessPattern(mem, write._3)
    val readInstances  = bankingFromAccessPattern(mem, read._3)
    debug("    write banking: " + writeInstances)
    debug("    read banking:  " + readInstances)
    val bankWrite = writeInstances.banking
    val bankRead  = readInstances.banking

    var banking: List[Banking] = Nil
    var i: Int = 0

    // TODO: Should we try to detect diagonal banking for more than 2 dimensions?
    // TODO: Should we try to detect diagonal banking for non-contiguous dimensions?
    if (bankWrite.length == bankRead.length) {
      var i = 0
      while (i < bankWrite.length) {
        if (i < bankWrite.length - 1) {
          // Special case for creating diagonally banked memories
          (bankWrite(i),bankWrite(i+1),bankRead(i),bankRead(i+1)) match {
            case (Banking(1),StridedBanking(s1,p), StridedBanking(s2,q),Banking(1)) if p > 1 && q > 1 =>
              banking ::= DiagonalBanking(List(s2,s1), lcm(p,q))
              i += 2
            case (StridedBanking(s1,p),Banking(1), Banking(1),StridedBanking(s2,q)) if p > 1 && q > 1 =>
              banking ::= DiagonalBanking(List(s1,s2), lcm(p,q))
              i += 2

            case _ =>
              banking ::= matchBanking(bankWrite(i),bankRead(i))
              i += 1
          }
        }
        else {
          banking ::= matchBanking(bankWrite(i), bankRead(i))
          i += 1
        }
      }
    }
    else if (bankWrite.length == 1 || bankRead.length == 1) {
      // This one's a little easier to address, but not clear how arbitrary banking should be done here
      stageError("Memory " + nameOf(mem).getOrElse("") + " defined here is treated as both " + bankWrite.length + "D and " + bankRead.length + "D. This is currently unsupported.")
    }
    else stageError("Memory " + nameOf(mem).getOrElse("") + " defined here is treated as both " + bankWrite.length + "D and " + bankRead.length + "D. This is currently unsupported.")

    // Buffering factor
    val depth = 1 + distanceBetween((write._1,write._2), (read._1,read._2))

    // Duplication factor
    val duplicates = Math.max(writeInstances.duplicates, readInstances.duplicates)

    MemInstance(depth, duplicates, banking.reverse)
  }

  override def bank(mem: Exp[Any]) = if (isBRAM(mem.tp)) {
    (writersOf(mem).headOption,readersOf(mem)) match {
      case (None, Nil) => Nil
      case (Some(write), Nil) => List(unpairedAccess(mem, write))
      case (None, reads) =>
        val unallocatedReads = reads.filter{read => !memoryIndexOf.get(read._3).isDefined }
        // What to do here? No writer, so current version will always read garbage...
        val dups = unallocatedReads.map(read => unpairedAccess(mem, read))
        coalesceDuplicates(mem, unallocatedReads.map(_._3), dups)
      case (Some(write), reads) =>
        val allocatedReads = reads.filter{read => memoryIndexOf.get(read._3).isDefined }
        val unallocatedReads = reads.filter{read => !memoryIndexOf.get(read._3).isDefined }
        val allocReads = allocatedReads.map{read => s"$read (${memoryIndexOf(read._3)})"}
        debug(s"Write: $write, Reads: $unallocatedReads")
        debug(s"(Leaving out preallocated reads $allocReads)")
        allocatedReads.foreach{read => instanceIndexOf(read._3, mem) = memoryIndexOf(read._3) }
        val dups = unallocatedReads.map(read => pairedAccess(mem, write, read))
        coalesceDuplicates(mem, unallocatedReads.map(_._3), dups)
    }
  } else super.bank(mem)
}

trait RegisterBanking extends BankingBase {
  import IR._

  override def bank(mem: Exp[Any]) = if (isRegister(mem.tp)) {
    (writersOf(mem).headOption, readersOf(mem)) match {
      case (None, Nil)          => Nil
      case (Some(write), Nil)   => List(MemInstance(1, 1, List(NoBanking)))
      case (None, reads)        => List(MemInstance(1, 1, List(NoBanking)))
      case (Some(write), reads) =>
        debug(s"Write: $write, Reads: $reads")
        val dups = reads.map{read => MemInstance(1 + distanceBetween( (write._1,write._2), (read._1,read._2) ), 1, List(NoBanking)) }
        coalesceDuplicates(mem, reads.map(_._3), dups)
    }
  } else super.bank(mem)
}

trait FIFOBanking extends BankingBase {
  import IR._

  def accessPar(mem: Exp[Any], access: Exp[Any]) = {
    (unrollFactorsOf(access) diff unrollFactorsOf(mem)).map{case Exact(p) => p.toInt}.fold(1){_*_}
  }

  override def bank(mem: Exp[Any]) = if (isFIFO(mem.tp)) {
    (writersOf(mem).headOption, readersOf(mem).headOption) match {
      case (None,None)         => Nil
      case (Some(write), None) => List(MemInstance(1, 1, List(StridedBanking(1, accessPar(mem,write._3))) ))
      case (None, Some(read)) =>
        instanceIndexOf(read._3, mem) = 0
        List(MemInstance(1, 1, List(StridedBanking(1, accessPar(mem,read._3))) ))
      case (Some(write),Some(read)) =>
        debug(s"Write: $write, Read: $read")
        val banks = Math.max(accessPar(mem,write._3), accessPar(mem,read._3))
        instanceIndexOf(read._3, mem) = 0
        List(MemInstance(1, 1, List(StridedBanking(1, banks)) ))
    }
  } else super.bank(mem)
}


trait MemoryAnalyzer extends BRAMBanking with RegisterBanking with FIFOBanking {
  import IR._

  override val name = "Scratchpad Analyzer"
  override val eatReflect = true
  debugMode = SpatialConfig.verbose
  verboseMode = SpatialConfig.verbose

  // TODO: Also need pointer for each read/write to refer to instance(s) it accesses
  def analyzeMemory(mem: Exp[Any]) {
    debug("")
    debug(s"Inferring instances for memory $mem " + nameOf(mem).map{n => "(" + n + ")"}.getOrElse(""))
    val instances = bank(mem)
    duplicatesOf(mem) = instances

    debug("  Inferred " + instances.length + " instances: ")
    instances.zipWithIndex.foreach{ case (inst, i) =>
      debug("    #" + i + " Depth: " + inst.depth + ", Duplicates: " + inst.duplicates + ", Format: " + inst.banking.mkString(", "))
    }
  }

  def run(localMems: List[Exp[Any]]): Unit = localMems.foreach{mem => analyzeMemory(mem) }

  lazy val ctrlAnalyzer = new ControlSignalAnalyzer{val IR: MemoryAnalyzer.this.IR.type = MemoryAnalyzer.this.IR}

  override def runOnce[A:Manifest](b: Block[A]): Block[A] = {
    ctrlAnalyzer.run(b)
    run(ctrlAnalyzer.localMems)
    b
  }

}

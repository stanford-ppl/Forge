package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._
import scala.collection.mutable.{HashSet, HashMap}

// For bound symbols
trait SubstQuotingExp extends QuotingExp {
  import IR._
  val subst = HashMap[Exp[Any], Exp[Any]]()
  override def quote(x: Exp[Any]) = super.quote(subst.getOrElse(x, x))
}

trait PIRCommon extends SubstQuotingExp with Traversal {
  val IR: PIRScheduleAnalysisExp with DHDLExp
  import IR._

  val globals = HashSet[GlobalMem]()

  // Create a vector for communication to/from a given memory
  def allocateGlobal(mem: Exp[Any]) = {
    val name = quote(mem)
    val global = mem match {
      case Deff(Offchip_new(_)) => Offchip(name)
      case Deff(Argin_new(_))   => InputArg(name)
      case Deff(Argout_new(_))  => OutputArg(name)
      case Deff(Reg_new(_))     => ScalarMem(name)
      case mem if isArgIn(mem)  => InputArg(name)
      case mem if isArgOut(mem) => OutputArg(name)
      case mem if isRegister(mem.tp) => ScalarMem(name)
      case _                    => VectorMem(name)
    }
    debug(s"### Adding global for $mem: $global")
    globals += global
    global
  }

  private def allocateReg(reg: Exp[Any], pipe: Exp[Any], read: Option[Exp[Any]] = None, write: Option[Exp[Any]] = None) = {
    val isLocallyRead = isReadInPipe(reg, pipe, read)
    val isLocallyWritten = isWrittenInPipe(reg, pipe, write)
    debug(s"### Allocating register $reg in $pipe (localRead: $isLocallyRead, localWrite: $isLocallyWritten, accum: ${isAccum(reg)}")
    if (isLocallyRead && isLocallyWritten && isInnerAccum(reg)) {
      ReduceReg(reg)
    }
    else if (isLocallyRead && isLocallyWritten && isAccum(reg)) {
      val rst = allocateConst(resetValue(reg.asInstanceOf[Exp[Reg[Any]]]))
      AccumReg(reg, rst)
    }
    else if (!isLocallyRead) { // Always prefer the local register over ScalarOut, if applicable
      val global = allocateGlobal(reg)
      ScalarOut(reg, global)
    }
    else if (!isLocallyWritten) {
      val global = allocateGlobal(reg)
      ScalarIn(reg, global)
    }
    else {
      TempReg(reg)
    }
  }

  def allocateLocal(mem: Exp[Any], pipe: Exp[Any], read: Option[Exp[Any]] = None,  write: Option[Exp[Any]] = None): LocalMem = mem match {
    case Exact(c) => allocateConst(mem)
    case reg@Deff(Argin_new(init)) =>
      val global = allocateGlobal(reg)
      ScalarIn(reg, global)
    case reg@Deff(Argout_new(init)) => allocateReg(reg, pipe, read, write) // argOuts can be accumulators
    case reg@Deff(Reg_new(init))    => allocateReg(reg, pipe, read, write)

    case reader@Deff(Reg_read(reg)) =>
      debug(s"### Allocating reader $reader of $reg in $pipe")
      allocateLocal(reg, pipe, Some(reader), write)

    case _ => TempReg(mem)
  }

  def isBuffer(mem: Exp[Any]) = isBRAM(mem.tp) // || isFIFO(mem.tp)

  def allocateMem(mem: Exp[Any], reader: Exp[Any], cu: ComputeUnit) = {
    if (!isBuffer(mem))
      throw new Exception(s"Cannot allocate SRAM for non-buffer $mem")

    val name = s"${quote(mem)}_${quote(reader)}"
    val size = memSize(mem)
    debug(s"### Looking for mem $name in $cu")
    val sram = cu.getOrAddMem(name, size)
    debug(s"### ${sram.dumpString}")
    sram
  }
}

trait PIRScheduleAnalysisExp extends NodeMetadataOpsExp with ReductionAnalysisExp {
  this: DHDLExp =>

  sealed abstract class MemoryMode
  case object MemLoad extends MemoryMode { override def toString() = "TileLoad" }
  case object MemStore extends MemoryMode { override def toString() = "TileStore" }
  case object MemScatter extends MemoryMode { override def toString() = "Scatter" }
  case object MemGather extends MemoryMode { override def toString() = "Gather" }

  // Inter-CU communication
  sealed abstract class GlobalMem
  case class Offchip(name: String) extends GlobalMem
  case class MemCtrl(name: String, region: Offchip, mode: MemoryMode) extends GlobalMem
  case class InputArg(name: String) extends GlobalMem
  case class OutputArg(name: String) extends GlobalMem
  case class ScalarMem(name: String) extends GlobalMem
  case class VectorMem(name: String) extends GlobalMem
  case object LocalVector extends GlobalMem


  // Intra-CU communication
  sealed abstract class LocalMem {
    val id = {LocalMem.id += 1; LocalMem.id}
  }
  object LocalMem { var id = 0 }

  case class ConstReg(const: String) extends LocalMem
  case class CounterReg(cchain: CUCounterChain, idx: Int) extends LocalMem

  case class ReadAddrWire(mem: CUMemory) extends LocalMem
  case class WriteAddrWire(mem: CUMemory) extends LocalMem
  case class LocalWriteReg(mem: CUMemory) extends LocalMem

  case class ReduceReg(x: Exp[Any]) extends LocalMem
  case class AccumReg(x: Exp[Any], init: ConstReg) extends LocalMem
  case class TempReg(x: Exp[Any]) extends LocalMem

  case class ScalarIn(x: Exp[Any], mem: GlobalMem) extends LocalMem
  case class ScalarOut(x: Exp[Any], mem: GlobalMem) extends LocalMem

  case class VectorIn(mem: GlobalMem) extends LocalMem
  case class InputReg(mem: CUMemory) extends LocalMem
  case class VectorLocal(x: Exp[Any], mem: CUMemory) extends LocalMem
  case class VectorOut(x: Exp[Any], mem: GlobalMem) extends LocalMem

  def isReadable(mem: LocalMem) = mem match {
    case _:ReadAddrWire | _:WriteAddrWire | _:LocalWriteReg => false
    case _:ScalarOut | _:VectorLocal | _:VectorOut => false
    case _ => true
  }
  def isWritable(mem: LocalMem) = mem match {
    case _:ConstReg | _:CounterReg | _:ScalarIn => false
    case _:VectorIn | _:InputReg => false
    case _ => true
  }

  // Local memory references
  case class LocalRef(stage: Int, reg: LocalMem)

  def isReadOutsidePipe(x: Exp[Any], pipe: Exp[Any], reader: Option[Exp[Any]] = None) = {
    isArgOut(x) || readersOf(x).exists{read => reader.map{filt => read._3 == filt}.getOrElse(true) && read._1 != pipe }
  }
  // (A) reader exists in this pipe or there are no readers
  def isReadInPipe(x: Exp[Any], pipe: Exp[Any], reader: Option[Exp[Any]] = None) = {
    readersOf(x).isEmpty || readersOf(x).exists{read => reader.map{filt => read._3 == filt}.getOrElse(true) && read._1 == pipe }
  }
  // Not an input argument, (a) writer exists in this pipe or there are no writers
  def isWrittenInPipe(x: Exp[Any], pipe: Exp[Any], writer: Option[Exp[Any]] = None) = {
    !isArgIn(x) && (writersOf(x).isEmpty || writersOf(x).exists{write => writer.map{filt => write._3 == filt}.getOrElse(true) && write._1 == pipe })
  }

  // TODO: This is VERY redundant with PIR
  sealed abstract class PIROp
  case object ALUMux extends PIROp { override def toString() = "Mux" }
  case object Bypass extends PIROp
  case object FixAdd extends PIROp
  case object FixSub extends PIROp
  case object FixMul extends PIROp
  case object FixDiv extends PIROp
  case object FltAdd extends PIROp
  case object FltSub extends PIROp
  case object FltMul extends PIROp
  case object FltDiv extends PIROp
  case object BitAnd extends PIROp
  case object BitOr  extends PIROp
  case object FixLt  extends PIROp
  case object FixLeq extends PIROp
  case object FixEql extends PIROp
  case object FixNeq extends PIROp

  // --- Stages prior to scheduling
  sealed abstract class PseudoStage { def output: Exp[Any] }
  case class DefStage(op: Exp[Any], isReduce: Boolean = false) extends PseudoStage { def output = op }
  case class OpStage(op: PIROp, inputs: List[Exp[Any]], out: Exp[Any], isReduce: Boolean = false) extends PseudoStage { def output = out }
  case class WriteAddrStage(write: Exp[Any]) extends PseudoStage { def output = write }

  // --- Stages after scheduling
  sealed abstract class Stage {
    def outputMems: List[LocalMem]
    def inputMems: List[LocalMem]
  }
  case class MapStage(op: PIROp, var ins: List[LocalRef], var outs: List[LocalRef]) extends Stage {
    def outputMems = outs.map(_.reg)
    def inputMems = ins.map(_.reg)
  }
  case class ReduceStage(op: PIROp, init: LocalMem, acc: ReduceReg) extends Stage {
    def outputMems = List(acc)
    def inputMems = throw new Exception("Inputs on ReduceStage not available") // Should really be a reducereg and acc
  }

  // --- Compute units
  def allocateConst(x: Exp[Any]) = x match {
    case Exact(c) => ConstReg(s"${c.toLong}l") // FIXME: Not necessarily an integer
    case _ => throw new Exception(s"Cannot allocate constant value for $x")
  }

  sealed abstract class ComputeUnit(val name: String, val parent: Option[ComputeUnit], val deps: List[Exp[Any]]) {
    var cchains: Set[CUCounterChain] = Set.empty
    var srams: Set[CUMemory] = Set.empty
    var regs: Set[LocalMem] = Set.empty
    private val regTable = HashMap[Exp[Any], LocalMem]()
    private val expTable = HashMap[LocalMem, List[Exp[Any]]]()

    def addReg(exp: Exp[Any], reg: LocalMem) {
      regs += reg
      regTable += exp -> reg
      if (expTable.contains(reg)) expTable += reg -> (expTable(reg) :+ exp)
      else                        expTable += reg -> List(exp)
    }
    def iterators = regTable.flatMap{case (exp, reg: CounterReg) => Some((exp,reg)); case _ => None}.toList
    def get(x: Exp[Any]): Option[LocalMem] = x match {
      case Exact(_) => Some(getOrAddReg(x)(allocateConst(x)))
      case _ => regTable.get(x) match {
        case Some(reg) if regs.contains(reg) => Some(reg)
        case _ => None
      }
    }
    def getOrAddReg(x: Exp[Any])(func: => LocalMem) = regTable.get(x) match {
      case Some(reg) if regs.contains(reg) => reg // On return this mapping if it is valid
      case _ =>
        val reg = x match {case Exact(_) => allocateConst(x); case _ => func }
        addReg(x, reg)
        reg
    }

    def getOrAddMem(name: String, size: Int) = srams.find(_.name == name) match {
      case Some(sram) => sram
      case None =>
        val sram = CUMemory(name, size)
        srams += sram
        sram
    }

    var writePseudoStages = HashMap[List[CUMemory], List[PseudoStage]]()
    var computePseudoStages: List[PseudoStage] = Nil
    var writeStages = HashMap[List[CUMemory], List[Stage]]()
    var stages: List[Stage] = Nil

    def dumpString = s"""  cchains = ${cchains.mkString(", ")}
  regs    = ${regs.mkString(", ")}
  srams   = ${srams.mkString(", ")}
  stages  = ${if (stages.isEmpty) "" else stages.mkString("\n    ","\n    ","")}"""
  }

  case class BasicComputeUnit(
    override val name: String,
    override val parent: Option[ComputeUnit],
    override val deps: List[Exp[Any]],
    val tpe: ControlType
  ) extends ComputeUnit(name,parent,deps) {
    override def dumpString = s"""BasicComputeUnit($name, $parent, $tpe){
${super.dumpString}
}"""
    override def toString() = s"BasicComputeUnit($name, ${parent.map(_.name)})"

    def isUnitCompute = (stages.nonEmpty || writeStages.nonEmpty) && !cchains.exists(_.isInstanceOf[CounterChainInstance])
  }

  case class TileTransferUnit(
    override val name: String,
    override val parent: Option[ComputeUnit],
    override val deps: List[Exp[Any]],
    val ctrl: MemCtrl,
    var vec: VectorMem,
    val mode: MemoryMode
  ) extends ComputeUnit(name,parent,deps) {
    override def dumpString = s"""TileTransferUnit($name, $parent, $ctrl, $mode){
${super.dumpString}
}"""
    override def toString() = s"TileTransferUnit($name, ${parent.map(_.name)}, $ctrl, $mode)"
  }

  // TODO: Parallelism?
  case class CUCounter(name: String, start: LocalMem, end: LocalMem, stride: LocalMem)

  sealed abstract class CUCounterChain(val name: String)
  case class CounterChainCopy(override val name: String, owner: ComputeUnit) extends CUCounterChain(name)
  case class CounterChainInstance(override val name: String, ctrs: List[CUCounter]) extends CUCounterChain(name)

  def memSize(mem: Exp[Any]) = dimsOf(mem).map(dim => bound(dim).get.toInt).fold(1){_*_}

  case class CUMemory(name: String, size: Int) {
    // These can be recursive... e.g. readAddr = ReadAddrWire(this)
    // TODO: Does this need to be changed?
    var vector: Option[GlobalMem] = None
    var readAddr: Option[LocalMem] = None
    var writeAddr: Option[LocalMem] = None

    def dumpString = s"""CUMemory($name, $size) {
  vector = $vector
  readAddr = $readAddr
  writeAddr = $writeAddr
}"""
  }

}

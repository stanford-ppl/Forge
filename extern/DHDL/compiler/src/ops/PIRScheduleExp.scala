package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait PIRScheduleAnalysisExp extends NodeMetadataOpsExp with ReductionAnalysisExp {
  this: DHDLExp =>

  sealed abstract class MemoryMode
  case object MemLoad extends MemoryMode { override def toString() = "TileLoad" }
  case object MemStore extends MemoryMode { override def toString() = "TileStore" }
  case object MemScatter extends MemoryMode { override def toString() = "Scatter" }
  case object MemGather extends MemoryMode { override def toString() = "Gather" }

  // Inter-CU communication
  sealed abstract class GlobalMem(val name: String)
  case class Offchip(override val name: String) extends GlobalMem(name)
  case class MemCtrl(override val name: String, region: Offchip, mode: MemoryMode) extends GlobalMem(name)
  case class InputArg(override val name: String) extends GlobalMem(name)
  case class OutputArg(override val name: String) extends GlobalMem(name)
  case class ScalarMem(override val name: String) extends GlobalMem(name)
  case class VectorMem(override val name: String) extends GlobalMem(name)
  case class TileTxVector(override val name: String) extends GlobalMem(name)

  // Intra-CU communication
  sealed abstract class CUReg
  case class ReduceReg(name: String) extends CUReg
  case class AccumReg(name: String, init: LocalMem) extends CUReg
  case class TempReg(name: String) extends CUReg
  case class ScalarIn(name: String, mem: GlobalMem) extends CUReg
  case class ScalarOut(name: String, mem: GlobalMem) extends CUReg
  case class VectorIn(name: String, mem: GlobalMem) extends CUReg
  case class VectorOut(name: String, mem: GlobalMem) extends CUReg
  case class CounterReg(cchain: CUCounterChain, idx: Int) extends CUReg
  case class Constant(const: String) extends CUReg

  // Local memory references
  sealed abstract class LocalMem
  case class LocalRef(stage: Int, reg: CUReg) extends LocalMem
  case class InputRef(mem: PIRMemory) extends LocalMem

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

  sealed abstract class PseduoStage
  case class DefStage(op: Exp[Any], isReduce: Boolean = false, isWrite: Boolean = false) extends PIRStage
  case class OpStage(op: PIROp, inputs: List[Exp[Any]], isReduce: Boolean, isWrite: Boolean) extends PIRStage

  sealed abstract class Stage
  case class MapStage(op: PIROp, inputs: List[LocalMem], var out: LocalMem) extends Stage
  case class ReduceStage(op: PIROp, init: LocalMem, var out: AccumReg) extends Stage

  sealed abstract class ComputeUnit(val name: String, val parent: Option[ComputeUnit]) {
    var cchains: Set[CUCounterChain] = Set.empty
    var srams: Set[CUMemory] = Set.empty
    var regs: Map[Exp[Any], CUReg] = Map.empty

    var pstages: List[PseduoStage] = Nil
    var stages: List[Stage] = Nil

    var scalarIn: List[ScalarIn] = Nil   // locally read, remotely written registers
    var scalarOut: List[ScalarOut] = Nil // locally written, remotely read registers
    var vectorIn: List[VectorIn] = Nil   // locally read, remotely written buffers
    var vectorOut: List[VectorOut] = Nil // locally written, remotely read buffers
    var tempRegs: Set[TempReg] = Set.empty
    var accRegs: Set[AccumReg] = Set.empty
    var tempAccs: Set[TempAccumReg] = Set.empty

    def temp(stage: Int) = {
      val t = TempReg(stage, tempRegs.size)
      tempRegs += t
      t
    }
    def tempWithInit(stage: Int, init: ConstReg) = {
      val t = TempAccumReg(stage, tempAccs.size, init)
      tempAccs += t
      t
    }
    def acc() = {
      val a = AccumReg(accRegs.size)
      accRegs += a
      a
    }
    def unuseReg(x: LocalMem) = x match {
      case t: TempReg => tempRegs -= t
      case t: TempAccumReg => tempAccs -= t
      case _ =>
    }

    def dumpString = s"""  cchains = ${cchains.mkString(", ")}
  iters   = ${iterators.mkString(", ")}
  srams   = ${srams.mkString(", ")}
  stages  = ${if (stages.isEmpty) "" else stages.mkString("\n    ","\n    ","")}
  scIns   = ${scalarIn.mkString(", ")}
  scOuts  = ${scalarOut.mkString(", ")}
  vecIns  = ${vectorIn.mkString(", ")}
  vecOuts = ${vectorOut.mkString(", ")}"""
}

  case class BasicComputeUnit(
    override val name: String,
    override val parent: Option[ComputeUnit],
    val tpe: ControlType
  ) extends ComputeUnit(name,parent) {
    override def dumpString = s"""BasicComputeUnit($name, $parent, $tpe){
${super.dumpString}
}"""
  override def toString() = s"BasicComputeUnit($name, ${parent.map(_.name)})"
  }

  case class TileTransferUnit(
    override val name: String,
    override val parent: Option[ComputeUnit],
    val ctrl: MemCtrl,
    val mode: MemoryMode
  ) extends ComputeUnit(name,parent) {
    override def dumpString = s"""TileTransferUnit($name, $parent, $ctrl, $mode){
${super.dumpString}
}"""
    override def toString() = s"TileTransferUnit($name, ${parent.map(_.name)}, $ctrl, $mode)"
  }

  // TODO: Parallelism?
  case class CUCounter(name: String, start: Exp[Any], end: Exp[Any], stride: Exp[Any])

  sealed abstract class CUCounterChain(val name: String)
  case class CounterChainCopy(override val name: String, owner: ComputeUnit) extends CUCounterChain(name)
  case class CounterChainInstance(override val name: String, ctrs: List[CUCounter]) extends PIRCounterChain(name)

  case class CUMemory(
    name: String,
    size: Int,
    var vector: Option[GlobalMem] = None,
    var readAddr: Option[Exp[Any]] = None,
    var writeAddr: Option[Exp[Any]] = None
  )

}

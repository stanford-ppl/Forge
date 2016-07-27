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

  sealed abstract class CommMem(val name: String)
  case class Offchip(override val name: String) extends CommMem(name)
  case class MemCtrl(override val name: String, region: Offchip, mode: MemoryMode) extends CommMem(name)
  case class InputArg(override val name: String) extends CommMem(name)
  case class OutputArg(override val name: String) extends CommMem(name)
  case class ScalarMem(override val name: String) extends CommMem(name)
  case class VectorMem(override val name: String) extends CommMem(name)

  case class ScalarIn(name: String, mem: CommMem)
  case class ScalarOut(name: String, mem: CommMem)
  case class VectorIn(name: String, mem: CommMem)
  case class VectorOut(name: String, mem: CommMem)

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

  sealed abstract class PIRStage(val isReduce: Boolean = false, val isWrite: Boolean = false)
  case class DefStage(
    op: Exp[Any],
    override val isReduce: Boolean = false,
    override val isWrite: Boolean = false
  ) extends PIRStage(isReduce, isWrite)

  case class PseudoStage(
    op: PIROp,
    override val isReduce: Boolean = false,
    override val isWrite: Boolean = false
  ) extends PIRStage(isReduce, isWrite)

  sealed abstract class ComputeUnit(val name: String, val parent: Option[ComputeUnit]) {
    var cchains: List[PIRCounterChain] = Nil
    var iterators: Map[Exp[Any], (PIRCounterChain, Int)] = Map.empty
    var srams: List[PIRMemory] = Nil
    var stages: List[PIRStage] = Nil
    var scalarIn: List[ScalarIn] = Nil   // locally read, remotely written registers
    var scalarOut: List[ScalarOut] = Nil // locally written, remotely read registers
    var vectorIn: List[VectorIn] = Nil   // locally read, remotely written buffers
    var vectorOut: List[VectorOut] = Nil // locally written, remotely read buffers

    def dumpString = s"""  cchains = ${cchains.mkString(", ")}
  iters   = ${iterators.mkString(", ")}
  srams   = ${srams.mkString(", ")}
  stages  = ${stages.mkString(", ")}
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
  }

  // TODO: Parallelism?
  case class PIRCounter(name: String, start: Exp[Any], end: Exp[Any], stride: Exp[Any], par: Exp[Any])

  sealed abstract class PIRCounterChain(val name: String)
  case class CounterChainCopy(override val name: String, owner: ComputeUnit) extends PIRCounterChain(name)
  case class CounterChainInstance(override val name: String, ctrs: List[PIRCounter]) extends PIRCounterChain(name)

  case class PIRMemory(
    name: String,
    size: Int,
    var writer: Option[ComputeUnit] = None,
    var readAddr: Option[Exp[Any]] = None,
    var writeAddr: Option[Exp[Any]] = None
  )

}

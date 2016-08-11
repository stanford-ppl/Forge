package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.collection.mutable.{HashMap,HashSet}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait PIROptimization extends Traversal with PIRCommon {
  val IR: DHDLExp with PIRScheduleAnalysisExp
  import IR._

  override val name = "PIR Scheduler"
  override val recurse = Always
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  val cus = HashMap[Exp[Any], ComputeUnit]()\

  override def run[A:Manifest](b: Block[A]) = {
    for ((pipe,cu) <- cus) removeUnusedCUComponents(cu)
    for ((pipe,cu) <- cus) removeRouteThruCUs(cu)
    b
  }


  def removeUnusedCUComponents(cu: ComputeUnit) {
    val stages = cu.stages.flatMap{case stage: MapStage => Some(stage); case _ => None} ++
                 cu.writeStages.values.flatMap{stages => stages.flatMap{case stage: MapStage => Some(stage); case _ => None}}

    // Remove all temporary registers from outputs when they are not used in any input
    val tempIns = stages.flatMap{stage => stage.inputMems.filter(_.isInstanceOf[TempReg]) }
    val tempOuts = stages.flatMap{stage => stage.outputMems.filter(_.isInstanceOf[TempReg]) }
    val unusedTemps = tempOuts.filterNot(tempIns contains _)
    stages.foreach{stage => stage.outs = stage.outs.filterNot{ref => unusedTemps contains ref.reg} }
    // Also remove from set of registers
    cu.regs --= unusedTemps

    // Remove unused counterchain copies
    val usedCCs = stages.flatMap{stage => stage.inputMems.flatMap{case CounterReg(cchain,_) => Some(cchain); case _ => None}} ++
                  cu.srams.flatMap{sram => sram.readAddr match {case Some(CounterReg(cchain,_)) => Some(cchain); case _ => None}} ++
                  cu.srams.flatMap{sram => sram.writeAddr match {case Some(CounterReg(cchain,_)) => Some(cchain); case _ => None}}
    val unusedCopies = cu.cchains.filter{cc=> cc.isInstanceOf[CounterChainCopy] && !usedCCs.contains(cc) }
    cu.cchains --= unusedCopies
  }

  // Remove route-through CUs from the IR after scheduling
  // Rationale (for post scheduling): The Spatial IR has a number of nodes which are
  // effectively no-ops in PIR, which makes detecting route through cases difficult.
  // Once scheduled, a typical route-through case just looks like a CU with a single stage
  // which takes a vecIn and bypasses to a vecOut, which is easier to pattern match on
  def removeRouteThruCUs(cu: ComputeUnit) = cu match {
    case cu: BasicComputeUnit if cu.tpe == InnerPipe =>
      // This is a super-specific condition:
      // 1. We have exactly one compute stage and no write stages
      // 2. That compute stage is a map stage which bypasses a vector in to a vector out
      if (cu.writeStages.isEmpty && cu.stages.length == 1) {
        cu.stages.head match {
          case MapStage(Bypass, List(LocalRef(_,VectorIn(in))), List(LocalRef(_,VectorOut(_,out))))



          case _ =>
        }
      }
    case _ =>
  }

  def vectorOuts(cu: ComputeUnit): List[VectorMem] = cu match {
    case ttu: TileTransferUnit if ttu.mode == MemLoad => ttu.vec
    case cu: BasicComputeUnit =>
      cu.stages.map(_.outputMems).flatMap{case vec: VectorMem => Some(vec); case _ => None}

    case _ => Nil
  }

}

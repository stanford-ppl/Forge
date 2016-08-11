package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.collection.mutable.{HashMap,HashSet}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait PIROptimizer extends Traversal with PIRCommon {
  val IR: DHDLExp with PIRScheduleAnalysisExp
  import IR._

  override val name = "PIR optimization"
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  val cuMapping = HashMap[Exp[Any], ComputeUnit]()

  lazy val cus = cuMapping.values

  override def run[A:Manifest](b: Block[A]) = {
    for (cu <- cus) removeUnusedCUComponents(cu)
    for (cu <- cus) removeRouteThruStages(cu)
    b
  }

  def removeUnusedCUComponents(cu: ComputeUnit) {
    val stages = allMapStages(cu)
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
  def removeRouteThruStages(cu: ComputeUnit) = cu match {
    case cu: BasicComputeUnit if cu.tpe == InnerPipe && cu.parent.isDefined =>
      // 1. The compute stage is a map stage which bypasses a vector in to a vector out
      // 2. The vector input's producer's parent is the same as this CU's parent
      val bypassStages = cu.stages.flatMap{
        case bypass@MapStage(Bypass, List(LocalRef(_,VectorIn(in))), List(LocalRef(_,VectorOut(_,out)))) =>
          cus.find{cu => vectorOuts(cu) contains in} match {
            case Some(producer) if producer.parent == cu.parent =>

                swapGlobal(out, in) // Reroute all consumers of this CU to the producer
                Some(bypass)

            case _ => None
          }
        case _ => None
      }
      // Remove bypass stages from CU
      cu.stages = removeStages(cu.stages, bypassStages)

    case _ =>
  }

  // --- Utility functions for optimization

  def allMapStages(cu: ComputeUnit): List[MapStage] = {
    cu.stages.flatMap{case stage: MapStage => Some(stage); case _ => None} ++
    cu.writeStages.values.flatMap{stages => stages.flatMap{case stage: MapStage => Some(stage); case _ => None}}
  }

  def vectorOuts(cu: ComputeUnit): List[VectorMem] = cu match {
    case tu: TileTransferUnit if tu.mode == MemLoad => List(tu.vec)
    case cu: BasicComputeUnit =>
      cu.stages.map(_.outputMems).flatMap{case vec: VectorMem => Some(vec); case _ => None}

    case _ => Nil
  }

  // --- Stage removal
  // TODO
  def removeStages(stages: List[Stage], remove: List[Stage]) = stages


  // --- Swapping helper functions

  def swapGlobal(orig: GlobalMem, swap: GlobalMem) = cus.foreach { cu =>
    allMapStages(cu).foreach{stage => swapGlobal_Stage(stage, orig, swap)}
    cu.srams.foreach{sram => swapGlobal_SRAM(sram, orig, swap)}

    cu match {
      case tu@TileTransferUnit(name,parent,deps,ctrl,`orig`,mode) => tu.vec = swap
      case _ =>
    }
  }
  def swapGlobal_Stage(stage: Stage, orig: GlobalMem, swap: GlobalMem) = stage match {
    case stage@MapStage(_,ins,outs) =>
      stage.ins = ins.map{case LocalRef(i,reg) => LocalRef(i, swapGlobal_Reg(reg,orig,swap)) }
      stage.outs = outs.map{case LocalRef(i,reg) => LocalRef(i, swapGlobal_Reg(reg,orig,swap)) }
    case _ =>
  }
  def swapGlobal_Reg(reg: LocalMem, orig: GlobalMem, swap: GlobalMem) = reg match {
    case VectorIn(`orig`) => VectorIn(swap)
    case VectorOut(x, `orig`) => VectorOut(x, swap)
    case ScalarIn(x, `orig`) => ScalarIn(x, swap)
    case ScalarOut(x, `orig`) => ScalarOut(x, swap)
    case _ => reg
  }
  def swapGlobal_SRAM(sram: CUMemory, orig: GlobalMem, swap: GlobalMem) {
    sram.vector = sram.vector match { case Some(`orig`) => Some(swap); case vec => vec}
  }

}

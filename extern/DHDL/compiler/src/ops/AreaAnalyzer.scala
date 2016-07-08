package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait AreaAnalysisExp extends AreaModel with LatencyModel {
  this: DHDLExp =>

  // TODO: This shouldn't be hardcoded (or here at all really)
  val BaseDesign = FPGAResources(
    lut7 = 468,
    lut6 = 9200,
    lut5 = 12350,
    lut4 = 11140,
    lut3 = 22600,
    mem64 = 4619,
    mem32 = 519,
    mem16 = 559,
    regs = 75400,
    dsps = 0,
    bram = 340
  )

  val FPGATarget = FPGAResourceSummary(alms=262400,regs=524800,dsps=1963,bram=2567,streams=13)  // Stratix V on MAIA board
}

trait AreaAnalyzer extends ModelingTraversal {
  val IR: AreaAnalysisExp with DHDLExp
  import IR._
  import ReductionTreeAnalysis._

  override val name = "Area Analyzer"

  override def silence() {
    super.silence()
    IR.silenceLatencyModel()
    IR.silenceAreaModel()
  }

  var totalArea = FPGAResourceSummary()
  var areaScope: List[FPGAResources] = Nil
  var savedArea: FPGAResources = NoArea

  override def save(b: Block[Any]) {
    super.save(b)
    savedArea = areaScope.fold(NoArea){_+_}
  }

  override def resume() {
    areaScope ::= savedArea
    inHwScope = true
    areaScope ::= areaOfBlock(savedBlock.get, false, 1)
    inHwScope = false
  }


  def areaOf(e: Exp[Any]) = IR.areaOf(e, inReduce, inHwScope)

  def areaOfBlock(b: Block[Any], isInnerLoop: Boolean, par: Int, oos: Map[Exp[Any],Long] = Map.empty): FPGAResources = {
    val outerScope = areaScope
    areaScope = Nil
    traverseBlock(b)
    val area = areaScope.fold(NoArea){_+_}
    areaScope = outerScope

    if (isInnerLoop) {
      val delays = pipeDelays(b) // oos unused for now
      val delayLineArea = delays.map {
        case (sym,len) if isBits(sym.tp) => areaOfDelayLine(nbits(sym.tp), len, par)
        case _ => NoArea
      }
      area.replicated(par,true) + delayLineArea.fold(NoArea){_+_}
    }
    else
      area.replicated(par,false)
  }
  def areaOfCycle(b: Block[Any], par: Int, oos: Map[Exp[Any],Long] = Map.empty): FPGAResources = {
    val outerReduce = inReduce
    inReduce = true
    val out = areaOfBlock(b, true, par, oos)
    inReduce = outerReduce
    out
  }
  def areaOfPipe(b: Block[Any], par: Int, oos: Map[Exp[Any],Long] = Map.empty): FPGAResources = {
    areaOfBlock(b, true, par, oos)
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    val area = rhs match {
      case EatReflect(Hwblock(blk)) =>
        inHwScope = true
        val body = areaOfBlock(blk, false, 1)
        save(blk) // Save HW scope to resume to later
        inHwScope = false
        body

      case EatReflect(Pipe_parallel(func)) => areaOfBlock(func,false,1) + areaOf(lhs)
      case EatReflect(Unit_pipe(func)) =>
        val body = areaOfBlock(func, isInnerPipe(lhs), 1)
        debug(s"Pipe $lhs: ")
        debug(s"  body: $body")
        body + areaOf(lhs)

      case EatReflect(Pipe_foreach(cchain, func, _)) =>
        val P = parsOf(cchain).reduce(_*_)

        val body = areaOfBlock(func, isInnerPipe(lhs), P)

        if (isInnerPipe(lhs)) debug(s"Foreach $lhs (P = $P):") else debug(s"Outer Foreach $lhs (P = $P):")
        debug(s"  body: $body")
        body + areaOf(lhs)

      case EatReflect(e@Pipe_fold(cchain,accum,zero,fA,iFunc,ld,st,func,rFunc,_,idx,_,_,_)) =>
        val P = parsOf(cchain).reduce(_*_)

        val body = areaOfBlock(func, isInnerPipe(lhs), P) // map duplicated P times
        /*
          Some simple math:
          A full binary (reduction) tree is a tree in which every node is either
          a leaf or has exactly two children.
          The number of internal (non-leaf) nodes of a full tree with L leaves is L - 1
          In our case, L is the map's parallelization factor P
          and internal nodes represent duplicates of the reduction function
          The reduction function is therefore duplicated P - 1 times
          Plus the special, tightly cyclic reduction function to update the accumulator
        */
        val internal = areaOfBlock(rFunc, true, P - 1)
        val rFuncLatency = latencyOfPipe(rFunc)
        val internalDelays = reductionTreeDelays(P).map{delay => areaOfDelayLine(nbits(e.mT), rFuncLatency * delay, 1) }.fold(NoArea){_+_}
        val icalc = areaOfCycle(iFunc, 1) // Calculate index for load/store
        val load  = areaOfCycle(ld, 1)    // Load from accumulator (happens once)
        val cycle = areaOfCycle(rFunc, 1) // Special case area of accumulator
        val store = areaOfCycle(st, 1, Map(idx -> -(latencyOfCycle(ld) + latencyOfCycle(rFunc))) )    // Store to accumulator (happens once)

        if (isInnerPipe(lhs)) debug(s"Reduce $lhs (P = $P):") else debug(s"Outer Reduce $lhs (P = $P):")
        debug(s"  body: $body")
        debug(s"  tree: $internal")
        debug(s"  dlys: $internalDelays")
        debug(s"  cycle: ${icalc + load + cycle + store}")

        body + internal + internalDelays + icalc + load + cycle + store + areaOf(lhs)

      case EatReflect(e@Accum_fold(ccOuter,ccInner,accum,zero,fA,iFunc,func,ld1,ld2,rFunc,st,_,_,idx,_,_,_,_)) =>
        val Pm = parsOf(ccOuter).reduce(_*_) // Parallelization factor for map
        val Pr = parsOf(ccInner).reduce(_*_) // Parallelization factor for reduce

        val body = areaOfBlock(func,false, Pm)

        val internal = areaOfPipe(rFunc, 1).replicated(Pm - 1,false).replicated(Pr,true)
        val rFuncLatency = latencyOfPipe(rFunc)
        val internalDelays = reductionTreeDelays(Pm).map{delay => areaOfDelayLine(nbits(e.mT), rFuncLatency * delay, Pr) }.fold(NoArea){_+_}
        val icalc = areaOfPipe(iFunc, 1) //.replicated(Pr,true) // TODO: Shouldn't be duplicated? Taken care of by counter parallelization
        val load1 = areaOfPipe(ld1, 1).replicated(Pm,false).replicated(Pr,true)
        val load2 = areaOfPipe(ld2, Pr, Map(idx -> -latencyOfPipe(ld1) ))
        val cycle = areaOfPipe(rFunc, Pr)
        val store = areaOfPipe(st, Pr, Map(idx -> -(latencyOfPipe(ld2) + latencyOfPipe(rFunc))) )

        debug(s"BlockReduce $lhs (Pm = $Pm, Pr = $Pr)")
        debug(s"  body: $body (Pm)")
        debug(s"  tree: $internal (Pm)")
        debug(s"  dlys: $internalDelays (Pr)")
        debug(s"  load: $load1 (Pm*Pr)")
        debug(s"  indx: $icalc (Pr)")
        debug(s"  accum: ${load2+cycle+store} (Pr)")

        body + internal + internalDelays + icalc + load1 + load2 + cycle + store + areaOf(lhs)

      case _ =>
        blocks(rhs).map(blk => areaOfBlock(blk,false,1)).fold(NoArea){_+_} + areaOf(lhs)
    }
    areaScope ::= area
  }

  override def preprocess[A:Manifest](b: Block[A]) = {
    areaScope = Nil
    super.preprocess(b)
  }


  // Neural net models for routing, fanout, unavailable ALMs
  val lutRoutingModel  = new LUTRoutingModel()
  val regFanoutModel   = new RegFanoutModel()
  val unavailALMsModel = new UnavailALMsModel()


  override def postprocess[A:Manifest](b: Block[A]) = {
    // TODO: Could potentially have multiple accelerator designs in a single program
    // Eventually want to be able to support multiple accel scopes
    val design = areaScope.fold(NoArea){_+_} + IR.BaseDesign

    val routingLUTs = lutRoutingModel.evaluate(design)  // 21000
    val fanoutRegs  = regFanoutModel.evaluate(design)   // 3600
    val unavailable = unavailALMsModel.evaluate(design) // 400

    val recoverable = design.lut3/2 + design.lut4/2 + design.lut5/2 + design.lut6/10 + design.mem16/2  + routingLUTs/2

    val logicALMs = design.lut3 + design.lut4 + design.lut5 + design.lut6 + design.lut7 +
                    design.mem16 + design.mem32 + design.mem64 + routingLUTs - recoverable + unavailable

    val totalRegs = design.regs + fanoutRegs + design.mregs

    val regALMs = Math.max( ((totalRegs - (logicALMs*2.16))/3).toInt, 0)

    val totalALMs = logicALMs + regALMs

    val dupBRAMs = Math.max(0.02*routingLUTs - 500, 0.0).toInt

    val totalDSPs = design.dsps
    val totalBRAM = design.bram + dupBRAMs

    msg(s"Resource Estimate Breakdown: ")
    msg(s"-----------------------------")
    msg(s"Estimated unavailable ALMs: $unavailable")
    msg(s"LUT3: ${design.lut3}")
    msg(s"LUT4: ${design.lut4}")
    msg(s"LUT5: ${design.lut5}")
    msg(s"LUT6: ${design.lut6}")
    msg(s"LUT7: ${design.lut7}")
    msg(s"Estimated routing luts: $routingLUTs")
    msg(s"Logic+register ALMs: $logicALMs")
    msg(s"Register-only ALMS:  $regALMs")
    msg(s"Recovered ALMs:      $recoverable")
    msg(s"MEM64: ${design.mem64}")
    msg(s"MEM32: ${design.mem32}")
    msg(s"MEM16: ${design.mem16}")
    msg(s"Design registers: ${design.regs}")
    msg(s"Memory registers: ${design.mregs}")
    msg(s"Fanout registers: $fanoutRegs")
    msg(s"Design BRAMs: ${design.bram}")
    msg(s"Fanout BRAMs: $dupBRAMs")
    msg("")
    msg(s"Resource Estimate Summary: ")
    msg(s"---------------------------")
    msg(s"ALMs: $totalALMs / ${FPGATarget.alms} (" + "%.2f".format(100*totalALMs.toDouble/FPGATarget.alms) + "%)")
    msg(s"Regs: $totalRegs")
    msg(s"DSPs: $totalDSPs / ${FPGATarget.dsps} (" + "%.2f".format(100*totalDSPs.toDouble/FPGATarget.dsps) + "%)")
    msg(s"BRAM: $totalBRAM / ${FPGATarget.bram} (" + "%.2f".format(100*totalBRAM.toDouble/FPGATarget.bram) + "%)")
    msg("")

    totalArea = FPGAResourceSummary(totalALMs, totalRegs, totalDSPs, totalBRAM, design.streams)
    (b)
  }

}

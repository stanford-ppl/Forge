package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait OpsAnalysisExp extends LatencyAnalysisExp with OpsModel {
  this: DHDLExp =>
}

trait OpsAnalyzer extends ModelingTraversal {
  val IR: OpsAnalysisExp with DHDLExp
  import IR._

  override val name = "Ops Analyzer"
  debugMode = SpatialConfig.debugging || SpatialConfig.loudModels
  verboseMode = SpatialConfig.verbose

  var totalOps = AppStatistics()
  var opScope: List[AppStatistics] = Nil

  def opsIn(e: Exp[Any]) = IR.opsIn(e, inHwScope)

  def opsInBlock(b: Block[Any]) = {
    val outerScope = opScope
    opScope = Nil
    traverseBlock(b)
    val ops = opScope.fold(NoOps){_+_}
    opScope = outerScope
    ops
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    val ops = rhs match {
      case EatReflect(Hwblock(blk)) =>
        inHwScope = true
        val body = opsInBlock(blk)
        inHwScope = false
        body

      case EatReflect(Pipe_parallel(func)) =>
        opsInBlock(func)

      case EatReflect(Unit_pipe(func)) =>
        val body = opsInBlock(func)
        debug(s"Pipe $lhs: ")
        debug(s"  body: $body")
        body

      case EatReflect(Pipe_foreach(cchain, func, _)) =>
        val P = parsOf(cchain).reduce(_*_)
        val N = nIters(cchain)
        val body = opsInBlock(func)

        debug(s"Foreach $lhs (N = $N, P = $P):")
        debug(s"  body: $body")
        body * P * N

      case EatReflect(e@Pipe_fold(cchain,accum,zero,fA,iFunc,ld,st,func,rFunc,_,idx,_,_,_)) =>
        val P = parsOf(cchain).reduce(_*_)
        val N = nIters(cchain)
        val body = opsInBlock(func) * N * P
        val reduce = opsInBlock(rFunc) * N * P
        val icalc = opsInBlock(iFunc) * N * P
        val load = opsInBlock(ld) * N * P
        val store = opsInBlock(st) * N * P

        debug(s"Fold $lhs (N = $N, P = $P)")
        debug(s"  body: $body")
        debug(s"  reduce: $reduce")
        debug(s"  icalc: $icalc")
        debug(s"  load: $load")
        debug(s"  store: $store")
        body + reduce + icalc + load + store

      case EatReflect(e@Accum_fold(ccOuter,ccInner,accum,zero,fA,iFunc,func,ld1,ld2,rFunc,st,_,_,idx,_,_,_,_)) =>
        val Pm = parsOf(ccOuter).reduce(_*_)
        val Pr = parsOf(ccInner).reduce(_*_)
        val Nm = nIters(ccOuter)
        val Nr = nIters(ccInner)

        val body = opsInBlock(func) * Nm * Pm
        val reduce = opsInBlock(rFunc) * Nm * Pm * Nr * Pr
        val icalc = opsInBlock(iFunc) * Nm * Pm * Nr * Pr
        val load = opsInBlock(ld1) * Nm * Pm * Nr * Pr
        val store = opsInBlock(ld2) * Nr * Pr

        debug(s"AccumFold $lhs (Nm = $Nm, Pm = $Pm, Nr = $Nr, Pr = $Pr)")
        debug(s"  body: $body")
        debug(s"  reduce: $reduce")
        debug(s"  icalc: $icalc")
        debug(s"  load: $load")
        debug(s"  store: $store")
        body + reduce + icalc + load + store

      case _ =>
        blocks(rhs).map{blk => opsInBlock(blk)}.fold(NoOps){_+_} + opsIn(lhs)
    }
    opScope ::= ops
  }

  override def preprocess[A:Manifest](b: Block[A]) = {
    opScope = Nil
    super.preprocess(b)
  }

  override def postprocess[A:Manifest](b: Block[A]) = {
    val G = 1000f*1000f*1000f
    val GB = 1024f*1024f*1024f*8f

    val total = opScope.fold(NoOps){_+_}
    totalOps = total

    val totalInsts = total.insts.toFloat / G
    val totalFLOPs = total.flops.toFloat / G
    val totalOnChipIn = total.onChipIn.toFloat / GB
    val totalOnChipOut = total.onChipOut.toFloat / GB
    val totalOffChipIn = total.dataIn.toFloat / GB
    val totalOffChipOut = total.dataOut.toFloat / GB

    val totalOnChip = totalOnChipIn + totalOnChipOut
    val totalOffChip = totalOffChipIn + totalOffChipOut

    val cycleAnalyzer = new LatencyAnalyzer{val IR: OpsAnalyzer.this.IR.type = OpsAnalyzer.this.IR}
    cycleAnalyzer.run(b)

    msg("Instruction statistics:")
    msg(s"  Instructions: ${total.insts} " + "(%.3f GI)".format(totalInsts))
    msg(s"  FLOPs: ${total.flops} " + "(%.3f GFLOPs)".format(totalFLOPs))
    msg(s"  On-chip loads: ${total.onChipOut} bits " + "(%.3f GB)".format(totalOnChipOut))
    msg(s"  On-chip stores: ${total.onChipIn} bits " + "(%.3f GB)".format(totalOnChipIn))
    msg(s"  Off-chip loads: ${total.dataIn} bits " + "(%.3f GB)".format(totalOffChipIn))
    msg(s"  Off-chip stores: ${total.dataOut} bits " + "(%.3f GB)".format(totalOffChipOut))
    msg("--")
    msg("  Total on-chip transfers: " + "%.3f GB".format(totalOnChip))
    msg("  Total off-chip transfers: " + "%.3f GB".format(totalOffChip))

    val totalCycles = cycleAnalyzer.totalCycles
    val runtime = totalCycles/(IR.CLK*1000000f)

    msg("")
    msg("Performance statistics (based on estimated runtime):")
    val MIPS = totalInsts / runtime
    val FLOPS = totalFLOPs / runtime
    val onChipLdBW = totalOnChipOut / runtime
    val onChipStBW = totalOnChipIn / runtime
    val offChipLdBW = totalOffChipIn / runtime
    val offChipStBW = totalOffChipOut / runtime

    val totalOnChipBW = totalOnChip / runtime
    val totalOffChipBW = totalOffChip / runtime

    msg("  Instructions: %.3f GIPS".format(MIPS))
    msg("  Floating point: %.3f GFLOPS".format(FLOPS))
    msg("  On-chip load bandwidth: %.3f GB/s".format(onChipLdBW))
    msg("  On-chip store bandwidth: %.3f GB/s".format(onChipStBW))
    msg("  Off-chip load bandwidth: %.3f GB/s".format(offChipLdBW))
    msg("  Off-chip store bandwidth: %.3f GB/s".format(offChipStBW))
    msg("--")
    msg("  Total on-chip bandwidth: %.3f GB/s".format(totalOnChipBW))
    msg("  Total off-chip bandwidth: %.3f GB/s".format(totalOffChipBW))

    super.postprocess(b)
  }

}

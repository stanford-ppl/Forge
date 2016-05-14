package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait OpsAnalysisExp extends LatencyAnalysisExp with OpsModel {
  this: DHDLExp =>
}

trait OpsAnalyzer extends ModelingTools {
  val IR: OpsAnalysisExp with DHDLExp
  import IR._

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

  def traverseNode(lhs: Exp[Any], rhs: Def[Any]) {
    val ops = rhs match {
      case EatReflect(Hwblock(blk)) =>
        inHwScope = true
        val body = opsInBlock(blk)
        inHwScope = false
        body

      case EatReflect(Pipe_parallel(func)) =>
        opsInBlock(func)

      case EatReflect(Unit_pipe(func)) =>
        opsInBlock(func)

      case EatReflect(Pipe_foreach(cchain, func, _)) =>
        val P = parsOf(cchain).reduce(_*_)
        val N = nIters(cchain)
        val body = opsInBlock(func)
        body * P * N

      case EatReflect(e@Pipe_fold(cchain,_,_,iFunc,ld,st,func,rFunc,_,idx,_,_,_)) =>
        val P = parsOf(cchain).reduce(_*_)
        val N = nIters(cchain)
        val body = opsInBlock(func) * N * P
        val reduce = opsInBlock(rFunc) * N * P
        val icalc = opsInBlock(iFunc) * N * P
        val load = opsInBlock(ld) * N * P
        val store = opsInBlock(st) * N * P

        body + reduce + icalc + load + store

      case EatReflect(e@Accum_fold(ccOuter,ccInner,_,_,iFunc,func,ld1,ld2,rFunc,st,_,_,idx,_,_,_,_)) =>
        val Pm = parsOf(ccOuter).reduce(_*_)
        val Pr = parsOf(ccInner).reduce(_*_)
        val Nm = nIters(ccOuter)
        val Nr = nIters(ccInner)

        val body = opsInBlock(func) * Nm * Pm
        val reduce = opsInBlock(rFunc) * Nm * Pm * Nr * Pr
        val icalc = opsInBlock(iFunc) * Nm * Pm * Nr * Pr
        val load = opsInBlock(ld1) * Nm * Pm * Nr * Pr
        val store = opsInBlock(ld2) * Nr * Pr

        body + reduce + icalc + load + store

      case _ =>
        blocks(rhs).map{blk => opsInBlock(blk)}.fold(NoOps){_+_}
    }
    opScope ::= ops
  }

  override def preprocess[A:Manifest](b: Block[A]) = {
    opScope = Nil
    super.preprocess(b)
  }

  override def postprocess[A:Manifest](b: Block[A]) = {
    val total = opScope.fold(NoOps){_+_}
    totalOps = total

    val totalInsts = total.insts.toFloat / (1000 * 1000)
    val totalFLOPs = total.flops.toFloat / (1000 * 1000)
    val totalOnChipIn = total.onChipIn.toFloat / (1024 * 1024)
    val totalOnChipOut = total.onChipOut.toFloat / (1024 * 1024)
    val totalOffChipIn = total.dataIn.toFloat / (1024 * 1024)
    val totalOffChipOut = total.dataOut.toFloat / (1024 * 1024)

    val totalOnChip = totalOnChipIn + totalOnChipOut
    val totalOffChip = totalOffChipIn + totalOffChipOut

    val cycleAnalyzer = new LatencyAnalyzer{val IR: OpsAnalyzer.this.IR.type = OpsAnalyzer.this.IR}
    cycleAnalyzer.run(b)

    msg("Instruction statistics:")
    msg(s"  Instructions: ${total.insts} " + "(%.1f MI)".format(totalInsts))
    msg(s"  FLOPs: ${total.flops} " + "(%.1f MFLOPs)".format(totalFLOPs))
    msg(s"  On-chip loads: ${total.onChipOut} bits " + "(%.2f MB)".format(totalOnChipOut))
    msg(s"  On-chip stores: ${total.onChipIn} bits " + "(%.2f MB)".format(totalOnChipIn))
    msg(s"  Off-chip loads: ${total.dataIn} bits " + "(%.2f MB)".format(totalOffChipIn))
    msg(s"  Off-chip stores: ${total.dataOut} bits " + "(%.2f MB)".format(totalOffChipOut))
    msg("--")
    msg("  Total on-chip transfers: " + "%.3f MB".format(totalOnChip))
    msg("  Total off-chip transfers: " + "%.3f MB".format(totalOffChip))

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

    msg("  Instructions: %.2f MIPS".format(MIPS))
    msg("  Floating point: %.2f FLOPS".format(FLOPS))
    msg("  On-chip load bandwidth: %.2f GB/s".format(onChipLdBW/1024))
    msg("  On-chip store bandwidth: %.2f GB/s".format(onChipStBW/1024))
    msg("  Off-chip load bandwidth: %.2f GB/s".format(offChipLdBW/1024))
    msg("  Off-chip store bandwidth: %.2f GB/s".format(offChipStBW/1024))
    msg("--")
    msg("  Total on-chip bandwidth: %.2f GB/s".format(totalOnChipBW/1024))
    msg("  Total off-chip bandwidth: %.2f GB/s".format(totalOffChipBW/1024))

    super.postprocess(b)
  }

}

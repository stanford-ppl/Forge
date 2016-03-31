package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._


trait LatencyAnalysisExp extends DHDLExp with LatencyModel with CounterToolsExp {
  this: DHDLExp =>

  val interruptCycles = 96
  val flushCycles = 512
  val pcieCycles = 42500
  val baseCycles = flushCycles + interruptCycles + pcieCycles
}

trait LatencyTools extends Traversal {
  val IR: DHDLExp with LatencyModel
  import IR._
  import ReductionTreeAnalysis._

  var inHwScope = false // In hardware scope
  var inReduce = false  // In tight reduction cycle (accumulator update)
  var cycleScope: List[Long] = Nil

  def latencyOf(e: Exp[Any]): Long = if (inHwScope) IR.latencyOf(e, inReduce) else 0L

  def latencyOfBlock(b: Block[Any]): List[Long] = {
    val outerScope = cycleScope
    cycleScope = Nil
    traverseBlock(b)
    val cycles = cycleScope.fold(0L){_+_}
    cycleScope = outerScope
    (cycles)
  }
  def latencyOfReduce(b: Block[Any]): List[Long] = {
    val outerReduce = inReduce
    inReduce = true
    val out = latencyOfBlock(b)
    inReduce = outerReduce
    out
  }

  override def traverseStm(stm: Stm) = stm match {
    case TP(s, d) => traverseNode(s, d)
  }

  def traverseNode(lhs: Exp[Any], rhs: Def[Any])(implicit ctx: SourceContext) {
    val cycles = rhs match {
      case EatReflect(Hwblock(blk)) =>
        inHwScope = true
        latencyOfBlock(blk).sum
        inHwScope = false

      case EatReflect(Counterchain_new(ctrs,nIters)) =>
        latencyOf(lhs)

      case EatReflect(Pipe_parallel(func)) =>
        latencyOfBlock(func).max + latencyOf(lhs)

      // --- Pipe
      case EatReflect(Unit_pipe(func)) if styleOf(lhs) == Fine =>
        latencyOfBlock(func).sum + latencyOf(lhs)

      case EatReflect(Pipe_foreach(cchain, func, _)) if styleOf(lhs) == Fine =>
        val N = 1 // TODO
        latencyOfBlock(func).sum * N + latencyOf(lhs)

      case EatReflect(Pipe_reduce(cchain,_,ld,st,func,rFunc,_,_,_,_)) if styleOf(lhs) == Fine =>
        val N = 1 // TODO

      // --- Sequential
      case EatReflect(Unit_pipe(func)) if styleOf(lhs) == Disabled =>
        latencyOfBlock(func).sum + latencyOf(lhs)

      // --- Metapipeline and Sequential
      case EatReflect(Pipe_foreach(cchain, func, _)) =>
        val N = 1 // TODO
        val stages = latencyOfBlock(func)
        if (styleOf(lhs) == Coarse) { stages.max * (N - 1) + stages.sum + latencyOf(lhs) }
        else                        { stages.sum * N + latencyOf(lhs) }

      case EatReflect(Pipe_reduce(cchain,_,ld,st,func,rFunc,_,_,_,_)) =>
        val N = 1 // TODO
        val P = parOf(cchain).reduce(_*_)
        val mapStages = latencyOfBlock(func)
        val internal = latencyOfBlock(rFunc).sum * reductionTreeHeight(P)
        val cycle = latencyOfReduce(ld).sum + latencyOfReduce(rFunc).sum + latencyOfReduce(st).sum

        val reduceStage = internal + cycle
        val stages = mapStages :+ reduceStage
        if (styleOf(lhs) == Coarse) { stages.max * (N - 1) + stages.sum + latencyOf(lhs) }
        else                        { stages.sum * N + latencyOf(lhs) }

      case EatReflect(Block_reduce(ccOuter,ccInner,_,func,ld1,ld2,rFunc,st,_,_,_,_,_,_)) =>
        val Nm = 1 // TODO
        val Nr = 1 // TODO
        val Pm = parOf(ccOuter).reduce(_*_) // Parallelization factor for map
        val Pr = parOf(ccInner).reduce(_*_) // Parallelization factor for reduce

        val mapStages = latencyOfBlock(func)
        val internal = (latencyOfBlock(ld1).sum) + latencyOfBlock(rFunc).sum) * reductionTreeHeight(Pm)
        val cycle = latencyOfReduce(ld2).sum + latencyOfReduce(rFunc).sum + latencyOfReduce(st).sum

        val reduceStage = internal + Nr*cycle
        val stages = mapStages :+ reduceStage
        if (styleOf(lhs) == Coarse) { stages.max * (Nm - 1) + stages.sum + latencyOf(lhs) }
        else                        { stages.sum * Nm + latencyOf(lhs) }

      case _ =>
        // No general rule for combining blocks
        blocks(rhs).foreach{blk => traverseBlock(blk)}
        latencyOf(lhs)
    }
    cycleScope ::= cycles
  }
}

trait LatencyAnalyzer extends AnalyzerBase {
  val IR: LatencyAnalysisExp with DHDLExp
  import IR._

  var totalCycles: Long = 0L

  private var silentTraversal = false
  private def msg(x: => Any) { if (!silentTraversal) System.out.println(x) }
  def silenceTraversal {
    silentTraversal = true
    IR.silenceLatencyModel()
  }


  override def postprocess[A:Manifest](b: Block[A]): Block[A] = {

    totalCycles = cycleScope.sum + IR.baseCycles
    (b)
  }
}

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

  var inHwScope = false // In hardware scope
  var inReduce = false  // In tight reduction cycle (accumulator update)
  var cycleScope: List[Long] = Nil

  def latencyOf(e: Exp[Any]) = IR.latencyOf(e, inReduce)

  def latencyOfBlock(b: Block[Any]) = {
    val outerScope = cycleScope
    cycleScope = Nil
    traverseBlock(b)
    val cycles = cycleScope.fold(0L){_+_}
    cycleScope = outerScope
    (cycles)
  }

}

trait LatencyAnalyzer extends AnalyzerBase {
  val IR: LatencyAnalysisExp with DHDLExp
  import IR._

  var total

}

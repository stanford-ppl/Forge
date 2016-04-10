package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait DSE extends Traversal {
  val IR: DHDLCompiler
  import IR._

  val paramAnalyzer = new ParameterAnalyzer{val IR = DSE.this.IR}
  def tileSizes  = paramAnalyzer.tileSizes
  def parFactors = paramAnalyzer.parFactors
  def metapipes  = paramAnalyzer.metapipes

  val areaAnalyzer = new AreaAnalyzer{val IR = DSE.this.IR}
  def totalArea = areaAnalyzer.totalArea

  val cycleAnalyzer = new LatencyAnalyzer{val IR = DSE.this.IR}
  def totalCycles = cycleAnalyzer.totalCycles


  def run[A:Manifest](b: Block[A]): Block[A] = {
    val bb = preprocess(b)

    // --- Get lists of parameters
    paramAnalyzer.run(b)

    areaAnalyzer.run(b)
    cycleAnalyzer.run(b)

    // --- Finalize parameters
    tileSizes.foreach{p => p.finalize}
    parFactors.foreach{p => p.finalize}

    postprocess(bb)
  }
}

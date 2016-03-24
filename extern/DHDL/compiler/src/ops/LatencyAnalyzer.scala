package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

import ppl.delite.framework.analysis.AnalyzerBase


trait LatencyAnalysisExp extends DHDLExp with LatencyModel {
  this: DHDLCompiler with DeliteApplication =>
}

trait LatencyAnalyzer extends AnalyzerBase {
  val IR: LatencyAnalyzerExp
  import IR._


}

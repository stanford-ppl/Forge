package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

import ppl.delite.framework.analysis.AnalyzerBase


trait AreaAnalysisExp extends DHDLExp with AreaModel {
  this: DHDLCompiler with DeliteApplication =>
}

trait AreaAnalyzer extends AnalyzerBase {
  val IR: AreaAnalyzerExp
  import IR._


}

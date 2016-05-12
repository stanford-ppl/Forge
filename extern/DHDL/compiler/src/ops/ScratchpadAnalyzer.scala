package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import ppl.delite.framework.analysis.HungryTraversal
import scala.virtualization.lms.common.EffectExp

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait ScratchpadAnalysisExp extends DHDLAffineAnalysisExp {
  this: DHDLExp =>

}

trait ScratchpadAnalyzer extends HungryTraversal {
  val IR: DHDLExp with ScratchpadAnalysisExp
  import IR._

}

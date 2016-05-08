package dhdl.shared.ops

import scala.virtualization.lms.common.Base
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._

trait CounterExternOps extends Base {
  this: DHDL =>

  def counter_new(start: Rep[FixPt[Signed,B32,B0]],end: Rep[FixPt[Signed,B32,B0]],step: Rep[FixPt[Signed,B32,B0]], par: Rep[Int])(implicit ctx: SourceContext): Rep[Counter]
  def counterchain_new(counters: List[Rep[Counter]])(implicit ctx: SourceContext): Rep[CounterChain]
}

trait CounterExternCompilerOps extends CounterExternOps { this: DHDL => }

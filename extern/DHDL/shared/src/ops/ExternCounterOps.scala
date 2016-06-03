package dhdl.shared.ops

import scala.virtualization.lms.common.Base
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._

trait ExternCounterTypes {
  type Counter
  type CounterChain

  implicit def counterManifest: Manifest[Counter]
  implicit def counterChainManifest: Manifest[CounterChain]
}

trait ExternCounterOps extends ExternCounterTypes with Base {this: DHDL => }
trait ExternCounterCompilerOps extends ExternCounterOps {
  this: DHDL =>

  def counter_new(start: Rep[FixPt[Signed,B32,B0]],end: Rep[FixPt[Signed,B32,B0]],step: Rep[FixPt[Signed,B32,B0]], par: Rep[Int])(implicit ctx: SourceContext): Rep[Counter]
  def counterchain_new(counters: List[Rep[Counter]])(implicit ctx: SourceContext): Rep[CounterChain]
}

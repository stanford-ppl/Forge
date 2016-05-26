package dhdl.library.classes
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.library._
import dhdl.library.classes._


trait ExternCounterWrapper {
  this: DHDLBase with DHDLClasses =>

  type Counter = FixedPointRange[Signed,B32,B0]
  type CounterChain = Array[FixedPointRange[Signed,B32,B0]]

  def counterManifest: Manifest[Counter] = manifest[FixedPointRange[Signed,B32,B0]]
  def counterChainManifest: Manifest[CounterChain] = manifest[Array[FixedPointRange[Signed,B32,B0]]]


  def counter_new(start: Rep[FixPt[Signed,B32,B0]],end: Rep[FixPt[Signed,B32,B0]],step: Rep[FixPt[Signed,B32,B0]], par: Rep[Int])(implicit ctx: SourceContext) = {
    start until end by step
  }
  def counterchain_new(counters: List[Rep[Counter]])(implicit ctx: SourceContext): Rep[CounterChain] = {
    counters.toArray.asInstanceOf[Rep[CounterChain]]
  }

}

package dhdl.library.classes

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.library._
import dhdl.library.classes._

trait CounterExternWrapper {
  this: DHDLBase with DHDLClasses =>

  def counter_new(start: Rep[FixPt[Signed,B32,B0]],end: Rep[FixPt[Signed,B32,B0]],step: Rep[FixPt[Signed,B32,B0]], par: Rep[Int])(implicit ctx: SourceContext) = {
    start until end by step
  }
  def counterchain_new(counters: List[Rep[Counter]])(implicit ctx: SourceContext): Rep[CounterChain] = {
    counters.toArray.asInstanceOf[Rep[CounterChain]]
  }

}
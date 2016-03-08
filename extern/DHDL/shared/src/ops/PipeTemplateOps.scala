package dhdl.shared.ops

import scala.virtualization.lms.common.Base
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._

trait PipeTemplateOps extends Base {
  this: DHDL =>

  def pipe_foreach(cchain: Rep[CounterChain], func: Rep[Indices] => Rep[Unit])(implicit ctx: SourceContext): Rep[Pipeline]
  def pipe_reduce[T,C[T]](cchain: Rep[CounterChain], accum: Rep[C[T]], func: Rep[Indices] => Rep[T], rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext, __mem: Mem[T,C], __mT: Manifest[T], __mC: Manifest[C[T]]): Rep[Pipeline]

  def counterchain_new(counters: List[Rep[Counter]])(implicit ctx: SourceContext): Rep[CounterChain]
}
trait PipeTemplateCompilerOps extends PipeTemplateOps { this: DHDL => }

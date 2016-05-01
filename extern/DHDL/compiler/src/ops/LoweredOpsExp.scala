package dhdl.compiler.ops

import scala.virtualization.lms.common.{EffectExp, ScalaGenEffect, DotGenEffect, MaxJGenEffect}
import scala.reflect.{Manifest,SourceContext}

import dhdl.compiler._
import dhdl.compiler.ops._

trait LoweredOpsExp extends EffectExp {
  this: DHDLExp =>

  // --- Nodes
  case class BusSlice[T:Manifest](bus: Exp[T], idx: Int) extends Def[T] {
    val mT = manifest[T]
  }

  case class PipeAccum[T:Manifest,C[T]](cc: Exp[CounterChain], accum: Exp[C[T]], func: Block[Unit], inds: List[Sym[Idx]], acc: Sym[C[T]])(implicit __mC: Manifest[C[T]], ctx: SourceContext) extends Def[Pipeline] {
    val mT = manifest[T]
    val mC = __mC
  }


  // --- Internal API
  def slice[T:Manifest](bus: Exp[T], idx: Int): Rep[T] = {
    if (idx >= par(bus)) throw new Exception("Cannot index position " + idx + " of " + par(bus) " wide bus")
    val x = reflectPure(BusSlice(bus,idx))
    setProps(x, getProps(bus))
    par(x) = 1
    x
  }

  def expandBus[T:Manifest](bus: Exp[T]): List[Rep[T]] = {
    if (par(bus) == 1) List(bus) else List.tabulate(par(bus)){i => slice(bus, i) }
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case BusSlice(b,i) => slice(f(b),i)
    case Reflect(e@BusSlice(b,i), u, es) => refletMirrored(Reflect(BusSlice(f(b),i)(e.mT), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@PipeAccum(cc,a,b,i,acc) => reflectPure(PipeAccum(f(cc),f(a),f(b),i,acc)(e.mT,e.mC,e.ctx))
    case Reflect(e@PipeAccum(cc,a,b,i,acc), u, es) => refletMirrored(Reflect(PipeAccum(f(cc),f(a),f(b),i,acc)(e.mT,e.mC,e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case _ => super.mirror(e,f)
  }

}

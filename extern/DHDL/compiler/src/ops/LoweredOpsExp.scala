package dhdl.compiler.ops

import scala.virtualization.lms.common.{EffectExp, ScalaGenEffect, DotGenEffect, MaxJGenEffect}
import scala.reflect.{Manifest,SourceContext}

import dhdl.compiler._
import dhdl.compiler.ops._

trait LoweredOpsExp extends EffectExp {
  this: DHDLExp =>

  // --- Nodes
  case class PipeAccum[T,C[T]](cc: Exp[CounterChain], accum: Exp[C[T]], func: Block[Unit], inds: List[Sym[Idx]], acc: Sym[C[T]])(implicit val ctx: SourceContext, val mT: Manifest[T], val mC: Manifest[C[T]]) extends Def[Pipeline]


  // --- Internal API

  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    //case BusSlice(b,i) => slice(f(b),i)
    //case Reflect(e@BusSlice(b,i), u, es) => refletMirrored(Reflect(BusSlice(f(b),i)(e.mT), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@PipeAccum(cc,a,b,i,acc) => reflectPure(PipeAccum(f(cc),f(a),f(b),i,acc)(e.ctx,e.mT,e.mC))
    case Reflect(e@PipeAccum(cc,a,b,i,acc), u, es) => reflectMirrored(Reflect(PipeAccum(f(cc),f(a),f(b),i,acc)(e.ctx,e.mT,e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }

}

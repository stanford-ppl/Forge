package dhdl.compiler.ops

import scala.virtualization.lms.common.{EffectExp, ScalaGenEffect, DotGenEffect, MaxJGenEffect}
import scala.reflect.{Manifest,SourceContext}

import dhdl.compiler._
import dhdl.compiler.ops._

trait LoweredOpsExp extends EffectExp {
  this: DHDLExp =>

  // --- Nodes
  case class ParPipeForeach[T,C[T]](
    cc: Exp[CounterChain],
    func: Block[Unit],
    inds: List[List[Sym[Idx]]]
  )(implicit val ctx: SourceContext)

  case class ParPipeReduce[T,C[T]](
    cc: Exp[CounterChain],
    accum: Exp[C[T]],
    func: Block[Unit],
    inds: List[List[Sym[Idx]]],
    acc: Sym[C[T]]
  )(implicit val ctx: SourceContext, val mT: Manifest[T], val mC: Manifest[C[T]]) extends Def[Pipeline]

  // --- Internal API

  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@ParPipeForeach(cc,func,i) => reflectPure(ParPipeForeach(f(cc),f(func),i)(e.ctx))(mtype(manifest[A]),pos)
    case Reflect(e@ParPipeForeach(cc,func,i)) => reflectMirrored(Reflect(ParPipeForeach(f(cc),f(func),i)(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),pos)

    case e@ParPipeReduce(cc,a,b,i,acc) => reflectPure(ParPipeReduce(f(cc),f(a),f(b),f(i),acc)(e.ctx,e.mT,e.mC))(mtype(manifest[A]),pos)
    case Reflect(e@ParPipeReduce(cc,a,b,i,acc), u, es) => reflectMirrored(Reflect(ParPipeReduce(f(cc),f(a),f(b),f(i),acc)(e.ctx,e.mT,e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }

}

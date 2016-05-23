package dhdl.compiler.ops

import scala.virtualization.lms.common.{EffectExp, ScalaGenEffect, DotGenEffect, MaxJGenEffect}
import scala.reflect.{Manifest,SourceContext}

import dhdl.compiler._
import dhdl.compiler.ops._

trait LoweredPipeOpsExp extends EffectExp with ExternPrimitiveTypesExp {
  this: DHDLExp =>

  // --- Nodes
  case class ParPipeForeach(
    cc: Exp[CounterChain],
    func: Block[Unit],
    inds: List[List[Sym[FixPt[Signed,B32,B0]]]]
  )(implicit val ctx: SourceContext) extends Def[Pipeline]

  case class ParPipeReduce[T,C[T]](
    cc: Exp[CounterChain],
    accum: Exp[C[T]],
    func: Block[Unit],
    inds: List[List[Sym[FixPt[Signed,B32,B0]]]],
    acc: Sym[C[T]]
  )(implicit val ctx: SourceContext, val mT: Manifest[T], val mC: Manifest[C[T]]) extends Def[Pipeline]

  // --- Internal API

  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@ParPipeForeach(cc,func,i) => reflectPure(ParPipeForeach(f(cc),f(func),i)(e.ctx))(mtype(manifest[A]),pos)
    case Reflect(e@ParPipeForeach(cc,func,i), u, es) => reflectMirrored(Reflect(ParPipeForeach(f(cc),f(func),i)(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),pos)

    case e@ParPipeReduce(cc,a,b,i,acc) => reflectPure(ParPipeReduce(f(cc),f(a),f(b),i,acc)(e.ctx,e.mT,e.mC))(mtype(manifest[A]),pos)
    case Reflect(e@ParPipeReduce(cc,a,b,i,acc), u, es) => reflectMirrored(Reflect(ParPipeReduce(f(cc),f(a),f(b),i,acc)(e.ctx,e.mT,e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }

  // --- Dependencies
  override def syms(e: Any): List[Sym[Any]] = e match {
    case ParPipeForeach(cc,func,inds) => syms(cc) ::: syms(func)
    case ParPipeReduce(cc,accum,func,inds,acc) => syms(cc) ::: syms(accum) ::: syms(func)
    case _ => super.syms(e)
  }
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case ParPipeForeach(cc,func,inds) => readSyms(cc) ::: readSyms(func)
    case ParPipeReduce(cc,accum,func,inds,acc) => readSyms(cc) ::: readSyms(accum) ::: readSyms(func)
    case _ => super.readSyms(e)
  }
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ParPipeForeach(cc,func,inds) => freqNormal(cc) ::: freqNormal(func)
    case ParPipeReduce(cc,accum,func,inds,acc) => freqNormal(cc) ::: freqNormal(accum) ::: freqNormal(func)
    case _ => super.symsFreq(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ParPipeForeach(cc,func,inds) => inds.flatten ::: effectSyms(func)
    case ParPipeReduce(cc,accum,func,inds,acc) => inds.flatten ::: effectSyms(func) ::: List(acc)
    case _ => super.boundSyms(e)
  }
}

trait ScalaGenLoweredPipeOps extends ScalaGenEffect {
  val IR: LoweredPipeOpsExp with DHDLCodegenOps
  import IR._

  def emitParallelizedLoop(iters: List[List[Sym[FixPt[Signed,B32,B0]]]], cchain: Exp[CounterChain])(emitBlk: => Unit) = {
    iters.zipWithIndex.foreach{ case (is, i) =>
      stream.println("for( " + quote(cchain) + "_iter <- " + quote(cchain) + ".apply(" + i + ".toInt)) {")
      is.zipWithIndex.foreach{ case (iter, ofs) =>
        stream.println("  val "+quote(iter)+" = " + quote(cchain) + "_iter" + " + FixedPoint[Signed,B32,B0]("+ofs+")")
      }
    }
    emitBlk
    stream.println("}" * iters.length)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@ParPipeForeach(cchain, func, inds) =>
      emitParallelizedLoop(inds, cchain){ emitBlock(func) }
      emitValDef(sym, "()")

    case e@ParPipeReduce(cchain, accum, func, inds, acc) =>
      emitValDef(acc, quote(accum))
      emitParallelizedLoop(inds, cchain){ emitBlock(func) }
      emitValDef(sym, "()")

    case _ => super.emitNode(sym, rhs)
  }
}

package dhdl.compiler.ops

import scala.virtualization.lms.common.{EffectExp, ScalaGenEffect, DotGenEffect, MaxJGenEffect}
import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.transform.{DeliteTransform}

import dhdl.compiler._
import dhdl.compiler.ops._

trait LoweredPipeOpsExp extends EffectExp with ExternPrimitiveTypesExp with MemoryTemplateOpsExp {
  this: DHDLExp =>

  // --- Nodes
  // TODO: Can these two be combined? Do we still need the reduce abstraction here?
  case class ParPipeForeach(
    cc:   Exp[CounterChain],
    func: Block[Unit],
    inds: List[List[Sym[FixPt[Signed,B32,B0]]]]
  )(implicit val ctx: SourceContext) extends Def[Pipeline]

  case class ParPipeReduce[T,C[T]](
    cc:    Exp[CounterChain],
    accum: Exp[C[T]],
    func:  Block[Unit],
    rFunc: Block[T],
    inds:  List[List[Sym[FixPt[Signed,B32,B0]]]],
    acc:   Sym[C[T]],
    rV:    (Sym[T], Sym[T])
  )(implicit val ctx: SourceContext, val mT: Manifest[T], val mC: Manifest[C[T]]) extends Def[Pipeline]

  case class ParBramLoadVector[T](
    bram:   Exp[BRAM[T]],
    ofs:    Exp[FixPt[Signed,B32,B0]],
    cchain: Exp[CounterChain],
    inds:   List[List[Sym[FixPt[Signed,B32,B0]]]]
  )(implicit val mT: Manifest[T], val ctx: SourceContext) extends Def[Vector[T]]

  case class ParBramStoreVector[T](
    bram:   Exp[BRAM[T]],
    ofs:    Exp[FixPt[Signed,B32,B0]],
    vec:    Exp[Vector[T]],
    cchain: Exp[CounterChain],
    inds:   List[List[Sym[FixPt[Signed,B32,B0]]]]
  )(implicit val mT: Manifest[T], val ctx: SourceContext) extends Def[Unit]



  // --- Internal API

  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@ParPipeForeach(cc,func,i) => reflectPure(ParPipeForeach(f(cc),f(func),i)(e.ctx))(mtype(manifest[A]),pos)
    case Reflect(e@ParPipeForeach(cc,func,i), u, es) => reflectMirrored(Reflect(ParPipeForeach(f(cc),f(func),i)(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),pos)

    case e@ParPipeReduce(cc,a,b,r,i,acc,rV) => reflectPure(ParPipeReduce(f(cc),f(a),f(b),f(r),i,acc,rV)(e.ctx,e.mT,e.mC))(mtype(manifest[A]),pos)
    case Reflect(e@ParPipeReduce(cc,a,b,r,i,acc,rV), u, es) => reflectMirrored(Reflect(ParPipeReduce(f(cc),f(a),f(b),f(r),i,acc,rV)(e.ctx,e.mT,e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@ParBramLoadVector(b,o,c,i) => reflectPure(ParBramLoadVector(f(b),f(o),f(c),i)(e.mT,e.ctx))(mtype(manifest[A]),pos)
    case Reflect(e@ParBramLoadVector(b,o,c,i), u, es) => reflectMirrored(Reflect(ParBramLoadVector(f(b),f(o),f(c),i)(e.mT,e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@ParBramStoreVector(b,o,v,c,i) => reflectPure(ParBramStoreVector(f(b),f(o),f(v),f(c),i)(e.mT,e.ctx))(mtype(manifest[A]),pos)
    case Reflect(e@ParBramStoreVector(b,o,v,c,i), u, es) => reflectMirrored(Reflect(ParBramStoreVector(f(b),f(o),f(v),f(c),i)(e.mT,e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(e,f)
  }

  // --- Dependencies
  override def syms(e: Any): List[Sym[Any]] = e match {
    case ParPipeForeach(cc,func,inds) => syms(cc) ::: syms(func)
    case ParPipeReduce(cc,accum,func,rFunc,inds,acc,rV) => syms(cc) ::: syms(accum) ::: syms(func) ::: syms(rFunc)
    case ParBramLoadVector(bram,ofs,cc,inds) => syms(bram) ::: syms(ofs) ::: syms(cc)
    case ParBramStoreVector(bram,ofs,vec,cc,inds) => syms(bram) ::: syms(ofs) ::: syms(vec) ::: syms(cc)
    case _ => super.syms(e)
  }
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case ParPipeForeach(cc,func,inds) => readSyms(cc) ::: readSyms(func)
    case ParPipeReduce(cc,accum,func,rFunc,inds,acc,rV) => readSyms(cc) ::: readSyms(accum) ::: readSyms(func) ::: readSyms(rFunc)
    case ParBramLoadVector(bram,ofs,cc,inds) => readSyms(bram) ::: readSyms(ofs) ::: readSyms(cc)
    case ParBramStoreVector(bram,ofs,vec,cc,inds) => readSyms(bram) ::: readSyms(ofs) ::: readSyms(vec) ::: readSyms(cc)
    case _ => super.readSyms(e)
  }
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ParPipeForeach(cc,func,inds) => freqNormal(cc) ::: freqNormal(func)
    case ParPipeReduce(cc,accum,func,rFunc,inds,acc,rV) => freqNormal(cc) ::: freqNormal(accum) ::: freqNormal(func) ::: freqNormal(rFunc)
    case ParBramLoadVector(bram,ofs,cc,inds) => freqNormal(bram) ::: freqNormal(ofs) ::: freqNormal(cc)
    case ParBramStoreVector(bram,ofs,vec,cc,inds) => freqNormal(bram) ::: freqNormal(ofs) ::: freqNormal(vec) ::: freqNormal(cc)
    case _ => super.symsFreq(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ParPipeForeach(cc,func,inds) => inds.flatten ::: effectSyms(func)
    case ParPipeReduce(cc,accum,func,rFunc,inds,acc,rV) => inds.flatten ::: effectSyms(func) ::: effectSyms(rFunc) ::: List(acc, rV._1, rV._2)
    case ParBramLoadVector(bram,ofs,cc,inds) => inds.flatten
    case ParBramStoreVector(bram,ofs,vec,cc,inds) => inds.flatten
    case _ => super.boundSyms(e)
  }
}

trait ScalaGenLoweredPipeOps extends ScalaGenEffect {
  val IR: LoweredPipeOpsExp with DHDLCodegenOps
  import IR._

  def emitParallelizedLoop(iters: List[List[Sym[FixPt[Signed,B32,B0]]]], cchain: Exp[CounterChain])(emitBlk: => Unit) = {
    iters.zipWithIndex.foreach{ case (is, i) =>
      stream.println("for( " + quote(cchain) + "_vec" + i + " <- " + quote(cchain) + ".apply(" + i + ".toInt)) {")
      is.zipWithIndex.foreach{ case (iter, j) =>
        stream.println("  val "+quote(iter)+" = " + quote(cchain) + "_vec" + i + ".apply(" + j + ".toInt)")
      }
    }
    emitBlk
    stream.println("}" * iters.length)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@ParPipeForeach(cchain, func, inds) =>
      emitParallelizedLoop(inds, cchain){ emitBlock(func) }
      emitValDef(sym, "()")

    case e@ParPipeReduce(cchain, accum, func, rFunc, inds, acc, rV) =>
      emitValDef(acc, quote(accum))
      emitParallelizedLoop(inds, cchain){ emitBlock(func) }
      emitValDef(sym, "()")

    // TODO: Further unrolling for these two?
    case e@ParBramLoadVector(bram,ofs,cchain,inds) =>
      if (dimsOf(sym).length > 1) throw new Exception("Vectors above 1 dimension currently unsupported")
      val len = dimsOf(sym).map{case Exact(c) => c}.reduce(_*_).toInt
      stream.println("val " + quote(sym) + " = new Array[" + remap(e.mT) + "](" + len + ")")
      emitParallelizedLoop(inds, cchain){
        inds.head.foreach{i =>
          stream.println(quote(sym) + "(" + quote(i) + ".toInt) = " + quote(bram) + "( (" + quote(ofs) + " + " + quote(i) + ").toInt )")
        }
      }

    case e@ParBramStoreVector(bram,ofs,vec,cchain,inds) =>
      if (dimsOf(vec).length > 1) throw new Exception("Vectors above 1 dimension currently unsupported")
      emitParallelizedLoop(inds, cchain){
        inds.head.foreach{i =>
          stream.println(quote(bram) + "( (" + quote(ofs) + " + " + quote(i) + ").toInt ) = " + quote(vec) + "(" + quote(i) + ".toInt)")
        }
      }
      emitValDef(sym, "()")


    case _ => super.emitNode(sym, rhs)
  }
}

trait MaxJGenLoweredPipeOps extends MaxJGenControllerTemplateOps {
  val IR: LoweredPipeOpsExp with ControllerTemplateOpsExp with TpesOpsExp with ParallelOpsExp
          with PipeOpsExp with OffChipMemOpsExp with RegOpsExp with ExternCounterOpsExp
          with DHDLCodegenOps with NosynthOpsExp with DeliteTransform
  import IR._

  def emitParallelizedLoop(iters: List[List[Sym[FixPt[Signed,B32,B0]]]], cchain: Exp[CounterChain]) = {
    val Def(EatReflect(Counterchain_new(counters, nIter))) = cchain

    iters.zipWithIndex.foreach{ case (is, i) =>
      if (is.size == 1) { // This level is not parallelized, so assign the iter as-is
          emit("DFEVar " + quote(is(0)) + " = " + quote(counters(i)) + ";");
      } else { // This level IS parallelized, index into the counters correctly
        is.zipWithIndex.foreach{ case (iter, j) =>
          emit("DFEVar " + quote(iter) + " = " + quote(counters(i)) + "[" + j + "];")
        }
      }
    }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@ParPipeForeach(cchain, func, inds) =>
      controlNodeStack.push(sym)
      emitComment(s"""ParPipeForeach ${quote(sym)} = ParPipeForeach(${quote(cchain)}) {""")
      styleOf(sym) match {
        case Coarse => emitComment(s"""MPSM to be emitted""")
        case Fine => emitComment(s"""PipeSM to be emitted""")
        case Disabled => emitComment(s"""SeqSM to be emitted""")
        case _ => emitComment(s"""ParPipeForeach style: ${styleOf(sym)}""")
      }
      emitController(sym, Some(cchain))
      emitParallelizedLoop(inds, cchain)
      emitBlock(func)
      emitComment(s"""} ParPipeForeach ${quote(sym)}""")
      controlNodeStack.pop

    case e@ParPipeReduce(cchain, accum, func, rFunc, inds, acc, rV) =>
      controlNodeStack.push(sym)
      emitComment(s"""ParPipeReduce ${quote(sym)} = ParPipeReduce(${quote(cchain)}, ${quote(accum)}) {""")
      styleOf(sym) match {
        case Coarse => emitComment(s"""MPSM to be emitted""")
        case Fine => emitComment(s"""PipeSM to be emitted""")
        case Disabled => emitComment(s"""SeqSM to be emitted""")
        case _ => emitComment(s"""ParPipeForeach style: ${styleOf(sym)}""")
      }
      emit(s"""DFEVar ${quote(acc)}_delayed = ${tpstr(1)(acc.tp.typeArguments.head, implicitly[SourceContext])}.newInstance(this);""")
      emitController(sym, Some(cchain))
      emitParallelizedLoop(inds, cchain)
      emitBlock(func)
      emit(s"""${quote(accum)} <== ${quote(acc)};""")

      emitComment(s"""} ParPipeReduce ${quote(sym)}""")
      controlNodeStack.pop

    // TODO: Further unrolling for these two?
    case e@ParBramLoadVector(bram,ofs,cchain,inds) =>
      stream.println(s"""// ParBramLoadVector ${quote(sym)} = ParBramLoadVector(${quote(bram)}, ${quote(ofs)}, ${quote(cchain)})""")

    case e@ParBramStoreVector(bram,ofs,vec,cchain,inds) =>
      stream.println(s"""// ParBramStoreVector ${quote(sym)} = ParBramStoreVector(${quote(bram)}, ${quote(ofs)}, ${quote(vec)}})""")

    case _ => super.emitNode(sym, rhs)
  }
}

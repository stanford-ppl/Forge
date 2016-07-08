package dhdl.compiler.ops

import scala.virtualization.lms.common.{ScalaGenEffect, DotGenEffect, MaxJGenEffect}
import scala.reflect.{Manifest,SourceContext}

import dhdl.compiler._
import dhdl.compiler.ops._

trait LoweredPipeOpsExp extends PipeStageToolsExp with ExternPrimitiveTypesExp with MemoryTemplateOpsExp {
  this: DHDLExp =>

  // --- Nodes
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

  // --- Internal API

  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@ParPipeForeach(cc,func,i) => reflectPure(ParPipeForeach(f(cc),f(func),i)(e.ctx))(mtype(manifest[A]),pos)
    case Reflect(e@ParPipeForeach(cc,func,i), u, es) => reflectMirrored(Reflect(ParPipeForeach(f(cc),f(func),i)(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),pos)

    case e@ParPipeReduce(cc,a,b,r,i,acc,rV) => reflectPure(ParPipeReduce(f(cc),f(a),f(b),f(r),i,acc,rV)(e.ctx,e.mT,e.mC))(mtype(manifest[A]),pos)
    case Reflect(e@ParPipeReduce(cc,a,b,r,i,acc,rV), u, es) => reflectMirrored(Reflect(ParPipeReduce(f(cc),f(a),f(b),f(r),i,acc,rV)(e.ctx,e.mT,e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(e,f)
  }

  // --- Dependencies
  override def syms(e: Any): List[Sym[Any]] = e match {
    case ParPipeForeach(cc,func,inds) => syms(cc) ::: syms(func)
    case ParPipeReduce(cc,accum,func,rFunc,inds,acc,rV) => syms(cc) ::: syms(accum) ::: syms(func) ::: syms(rFunc)
    case _ => super.syms(e)
  }
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case ParPipeForeach(cc,func,inds) => readSyms(cc) ::: readSyms(func)
    case ParPipeReduce(cc,accum,func,rFunc,inds,acc,rV) => readSyms(cc) ::: readSyms(accum) ::: readSyms(func) ::: readSyms(rFunc)
    case _ => super.readSyms(e)
  }
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ParPipeForeach(cc,func,inds) => freqNormal(cc) ::: freqNormal(func)
    case ParPipeReduce(cc,accum,func,rFunc,inds,acc,rV) => freqNormal(cc) ::: freqNormal(accum) ::: freqNormal(func) ::: freqNormal(rFunc)
    case _ => super.symsFreq(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ParPipeForeach(cc,func,inds) => inds.flatten ::: effectSyms(func)
    case ParPipeReduce(cc,accum,func,rFunc,inds,acc,rV) => inds.flatten ::: effectSyms(func) ::: effectSyms(rFunc) ::: List(acc, rV._1, rV._2)
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

    case _ => super.emitNode(sym, rhs)
  }
}

trait MaxJGenLoweredPipeOps extends MaxJGenEffect {
  val IR: LoweredPipeOpsExp with DHDLCodegenOps with DHDLMetadataOpsExp with ExternCounterOpsExp
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

  def emitController_again(sym:Sym[Any], cchain:Option[Exp[CounterChain]]) {
    val style = styleOf(sym) match {
      case CoarsePipe => "MetaPipe"
      case InnerPipe => "Pipe"
      case StreamPipe => "StreamPipe"
      case SequentialPipe => "Sequential"
      case ForkJoin => "Parallel"
    }
    emitComment(s"""${style} ${quote(sym)} {""")

    /* Emit done signal */
    if (parentOf(sym).isEmpty) {
      emit(s"""DFEVar ${quote(sym)}_en = top_en;""")
      emit(s"""DFEVar ${quote(sym)}_done = dfeBool().newInstance(this);""")
      emit(s"""top_done <== ${quote(sym)}_done;""")
//      enDeclaredSet += sym
//      doneDeclaredSet += sym
    } else {
      emitComment(s"""parentOf(${quote(sym)}) = ${quote(parentOf(sym).get)}""")
    }

    /* State Machine Instatiation */
    emit(s"""SMIO ${quote(sym)}_sm = addStateMachine("${quote(sym)}_sm", new ${quote(sym)}_${style}(this));""")
    // IO
    emit(s"""DFEVar ${quote(sym)}_sm.connectInput("sm_en", ${quote(sym)}_en);""")
    emit(s"""DFEVar ${quote(sym)}_done <== stream.offset(${quote(sym)}_sm.getOutput("sm_done"),-1);""")
    styleOf(sym) match {
      case InnerPipe =>
        emit(s"""DFEVar ${quote(sym)}_rst_en = ${quote(sym)}_sm.getOutput("rst_en");""")
      case CoarsePipe =>
		    val Deff(Counterchain_new(counters, nIters)) = cchain.get
        emit(s"""${quote(sym)}_sm.connectInput("sm_numIter", ${quote(nIters)});""")
        emit(s"""DFEVar ${quote(sym)}_rst_en = ${quote(sym)}_sm.getOutput("rst_en");""")
      case SequentialPipe =>
        if (cchain.isDefined) {
          val Def(EatReflect(Counterchain_new(counters, nIters))) = cchain.get
          emit(s"""${quote(sym)}_sm.connectInput("sm_numIter", ${quote(nIters)});""")
        } else {
          emit(s"""${quote(sym)}_sm.connectInput("sm_numIter, constant.var(1);""")
        }
        emit(s"""DFEVar ${quote(sym)}_rst_en = ${quote(sym)}_sm.getOutput("rst_en");""")
      case ForkJoin =>
      case StreamPipe => // ???
    }

    /* Control Signals to Children Controllers */
    if (!isInnerPipe(sym)) {
		  childrenOf(sym).zipWithIndex.foreach { case (c, idx) =>
		  	emit(s"""DFEVar ${quote(c)}_done = dfeBool().newInstance(this);""")
		  	emit(s"""${quote(sym)}_sm.connectInput("s${idx}_done", ${quote(c)}_done);""")
        emit(s"""DFEVar ${quote(c)}_en = ${quote(sym)}_sm.getOutput("s${quote(idx)}_en");""")
//		  	enDeclaredSet += c
//		  	doneDeclaredSet += c
		  }
    }
    else {
      emit(s"""DFEVar ${quote(sym)}_rst_done = dfeBool().newInstance(this);""")
      emit(s"""${quote(sym)}_sm.connectInput("rst_done", ${quote(sym)}_rst_done);""")
      emit(s"""DFEVar ${quote(sym)}_rst_en = ${quote(sym)}_sm.getOutput("rst_en");""")

      emit(s"""OffsetExpr ${quote(sym)}_offset = stream.makeOffsetAutoLoop("${quote(sym)}_offset");""")
      emit(s"""${quote(sym)}_rst_done <== stream.offset(${quote(sym)}_rst_en, -${quote(sym)}_offset-1);""")
    }

    if (styleOf(sym)!=ForkJoin) {
//      if (cchain.isDefined) {
//        emitCChainCtrl(sym, cchain.get)
//      }
    }

    emitComment(s"""} ${style} ${quote(sym)}""")
  }


  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@ParPipeForeach(cchain, func, inds) =>
      emitComment(s"""ParPipeForeach ${quote(sym)} = ParPipeForeach(${quote(cchain)}) {""")
      styleOf(sym) match {
        case StreamPipe => emitComment(s"""StrmPipe to be emitted""")
        case CoarsePipe => emitComment(s"""MPSM to be emitted""")
        case InnerPipe => emitComment(s"""PipeSM to be emitted""")
        case SequentialPipe => emitComment(s"""SeqSM to be emitted""")
        case _ => emitComment(s"""ParPipeForeach style: ${styleOf(sym)}""")
      }
      emitController_again(sym, Some(cchain))
//      emitParallelizedLoop(inds, cchain){ emitBlock(func) }
//      emitValDef(sym, "()")
      emitBlock(func)
      emitComment(s"""} ParPipeForeach ${quote(sym)}""")

    case e@ParPipeReduce(cchain, accum, func, rFunc, inds, acc, rV) =>
      stream.println(s"""// ParPipeReduce ${quote(sym)} = ParPipeReduce(${quote(cchain)}, ${quote(accum)})""")

    case _ => super.emitNode(sym, rhs)
  }
}

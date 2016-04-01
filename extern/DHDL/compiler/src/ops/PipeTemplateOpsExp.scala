package dhdl.compiler.ops

import scala.virtualization.lms.common.{EffectExp, ScalaGenEffect, DotGenEffect, MaxJGenEffect}
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait PipeTemplateOpsExp extends PipeTemplateOps with MemoryTemplateOpsExp with EffectExp {
  this: DHDLExp =>

  // --- Nodes
  case class Pipe_foreach(cchain: Exp[CounterChain], func: Block[Unit], inds: List[Sym[FixPt[Signed,B32,B0]]])(implicit ctx: SourceContext) extends Def[Pipeline]
  case class Pipe_reduce[T,C[T]] (
    // - Inputs
    cchain: Exp[CounterChain],  // Loop counter chain
    accum:  Exp[C[T]],          // Reduction accumulator
    // - Reified blocks
    ldFunc: Block[T],           // Accumulator load function (reified with acc, inds)
    stFunc: Block[Unit],        // Accumulator store function (reified with acc, inds, res)
    func:   Block[T],           // Map function
    rFunc:  Block[T],           // Reduction function
    // - Bound args
    inds:   List[Sym[FixPt[Signed,B32,B0]]],   // Loop iterators
    acc:    Sym[C[T]],          // Reduction accumulator (bound argument, aliases with accum)
    res:    Sym[T],             // Reduction intermediate result (bound argument, aliases with rFunc.res)
    rV:    (Sym[T], Sym[T])     // Reduction function inputs (bound arguments, aliases with ldFunc.res and func.res)
  )(implicit ctx: SourceContext, __mem: Mem[T,C], __mT: Manifest[T], __mC: Manifest[C[T]]) extends Def[Pipeline] {
    val mT = __mT
    val mC = __mC
    val memC = __mem
  }

  case class Block_reduce[T:Manifest](
    // - Inputs
    ccOuter: Exp[Counterchain], // Counter chain for map (outer) loop
    ccInner: Exp[CounterChain], // Counter chain for reduce (inner) loop
    accum:  Exp[BRAM[T]],
    // - Reified blocks
    func: Block[Unit],          // Map function
    resLdFunc: Block[T],        // Partial result load function
    ldFunc: Block[T],           // Accumulator load function
    rFunc: Block[T],            // Reduction function
    stFunc: Block[Unit],        // Accumulator store function
    // - Bound args
    indsOuter: List[Sym[FixPt[Signed,B32,B0]]],  // Map (outer) loop iterators
    indsInner: List[Sym[FixPt[Signed,B32,B0]]],  // Reduce (inner) loop iterators
    part: Sym[BRAM[T]],
    acc: Sym[BRAM[T]],
    res: Sym[T],
    rV:  (Sym[T], Sym[T])
  )(implicit val ctx: SourceContext) {
    val mT = manifest[T]
  }

  case class Counter_new(start: Rep[FixPt[Signed,B32,B0]], end: Rep[FixPt[Signed,B32,B0]], step: Rep[FixPt[Signed,B32,B0]], par: Param[Int])(implicit val ctx: SourceContext) extends Def[Counter]
  case class Counterchain_new(counters: List[Rep[Counter]], nIter: Block[FixPt[Signed,B32,B0]])(implicit val ctx: SourceContext) extends Def[CounterChain]

  // --- Internals
  def pipe_foreach(cchain: Rep[CounterChain], func: Rep[Indices] => Rep[Unit])(implicit ctx: SourceContext): Rep[Pipeline] = {
    val inds = List.fill(sizeOf(cchain)){ fresh[FixPt[Signed,B32,B0]] } // Arbitrary number of bound args. Awww yeah.
    val blk = reifyEffects( func(indices_create(inds)) )
    reflectEffect(Pipe_foreach(cchain, blk, inds), summarizeEffects(blk).star andAlso Simple())
  }
  def pipe_reduce[T,C[T]](cchain: Rep[CounterChain], accum: Rep[C[T]], func: Rep[Indices] => Rep[T], rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext, __mem: Mem[T,C], __mT: Manifest[T], __mC: Manifest[C[T]]): Rep[Pipeline]  = {
    // Loop indices
    val is = List.fill(sizeOf(cchain)){ fresh[FixPt[Signed,B32,B0]] }
    val inds = indices_create(is)

    // Reified map function
    val mBlk = reifyEffects( func(inds) )

    // Reified load function
    val acc = reflectMutableSym( fresh[C[T]] )  // Has to be mutable since we write to "it"
    val ldBlk = reifyEffects(__mem.ld(acc, inds))

   // Reified reduction function
    val rV = (fresh[T], fresh[T])
    val rBlk = reifyEffects( rFunc(rV._1, rV._2) )

    // Reified store function
    val res = fresh[T]
    val stBlk = reifyEffects(__mem.st(acc, inds, res))

    val effects = summarizeEffects(mBlk) andAlso summarizeEffects(ldBlk) andAlso
                  summarizeEffects(rBlk) andAlso summarizeEffects(stBlk) andAlso Write(List(accum.asInstanceOf[Sym[C[T]]]))

    reflectEffect(Pipe_reduce(cchain, accum, ldBlk, stBlk, mBlk, rBlk, is, acc, res, rV), effects.star)
  }

  def block_reduce[T:Manifest](chain: Rep[CounterChain], accum: Rep[BRAM[T]], func: Rep[Indices] => Rep[BRAM[T]], rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[Pipeline] = {
    val is = List.fill(sizeOf(cchain)){ fresh[FixPt[Signed,B32,B0]] }   // Map loop indices
    val inds = indices_create(is)

    // Reified map function
    val mBlk = reifyEffects( func(inds) )

    // Reduce loop indices
    val ctrsRed = dimsOf(accum).map{dim => Counter(max = dim) }
    val cchainRed = CounterChain(ctrsred:_*)
    val isRed = List.fill(sizeOf(ccRed)){ fresh[FixPt[Signed,B32,B0]] } // Reduce loop indices
    val indsRed = indices_create(isRed)

    val part = fresh[BRAM[T]]
    // Partial result load
    val ldPartBlk = reifyEffects( part.ld(indsRed) )

    val acc = reflectMutableSym( fresh[BRAM[T]] )
    // Accumulator load
    val ldBlk = reifyEffects( acc.ld(indsRed) )

    val rV = (fresh[T],fresh[T])
    // Reified reduction function
    val rBlk = reifyEffects( rFunc(rV._1, rV._2) )

    val res = fresh[T]
    // Accumulator store function
    val stBlk = reifyEffects( acc.st(indsRed, res) )

    val effects = summarizeEffects(mBlk) andAlso summarizeEffects(ldPartBlk) andAlso
                  summarizeEffects(ldBlk) andAlso summarizeEffects(rBlk) andAlso summarizeEffects(stBlk) andAlso Write(List(accum.asInstanceOf[Sym[BRAM[T]]]))

    reflectEffect(Block_reduce(chain, cchainRed, accum, mBlk, ldPartBlk, ldBlk, rBlk, stBlk, inds, indsRed, part, acc, res, rV), effects.star)
  }

  def counter_new(start: Rep[FixPt[Signed,B32,B0]],end: Rep[FixPt[Signed,B32,B0]],step: Rep[FixPt[Signed,B32,B0]], par: Int)(implicit ctx: SourceContext) = {
    val p = param[Int](par)
    reflectEffect[Counter](Counter_new(start,end,step,par)(ctx))
  }

  private def counterSplit(x: Rep[Counter]) = x match {
    case Def(EatReflect(Counter_new(start,end,step,par))) => (start,end,step,par)
    case _ => throw new Exception("Could not find def for counter")
  }

  def counterchain_new(counters: List[Rep[Counter]])(implicit ctx: SourceContext): Rep[CounterChain] = {
    val ctrSplit = counters.map{ctr => counterSplit(ctr)}
    val starts = ctrSplit.map(_._1)
    val ends   = ctrSplit.map(_._2)
    val strides = ctrSplit.map(_._3)
    val pars = ctrSplit.map(_._4).map(int_to_fix(_)) // HACK: Convert to fixed point

    val nIter = reifyEffects[FixPt[Signed,B32,B0]] {
      val lens = starts.zip(ends).map{
        case (ConstFix(x),ConstFix(y)) => (y - x).as[FixPt[Signed,B32,B0]]
        case (ConstFix(0), end) => end
        case (start, end) => (end - start)
      }
      val total = (lens,strides,pars).zipped.map{
        case (len, ConstFix(1), par) => (len / par) // TODO: Should use ceiling here
        case (len, stride, par) => (len / (stride * par))
      }
      total.reduce{_*_}
    }

    // HACK: Not actually mutable, but isn't being scheduled properly otherwise
    reflectMutable(Counterchain_new(counters, nIter)(ctx))
  }

  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@Pipe_foreach(c,func,inds) => reflectPure(Pipe_foreach(f(c),f(func),inds)(pos))(mtype(manifest[A]),pos)
    case Reflect(e@Pipe_foreach(c,func,inds), u, es) => reflectMirrored(Reflect(Pipe_foreach(f(c),f(func),inds)(pos), mapOver(f,u), f(es)))(mtype(manifest[A]),pos)

    case e@Pipe_reduce(c,a,ld,st,func,rFunc,inds,acc,res,rV) => reflectPure(Pipe_reduce(f(c),f(a),f(ld),f(st),f(func),f(rFunc),inds,acc,res,rV)(pos, e.memC, e.mT, e.mC))(mtype(manifest[A]), pos)
    case Reflect(e@Pipe_reduce(c,a,ld,st,func,rFunc,inds,acc,res,rV), u, es) => reflectMirrored(Reflect(Pipe_reduce(f(c),f(a),f(ld),f(st),f(func),f(rFunc),inds,acc,res,rV)(pos, e.memC, e.mT, e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@Block_reduce(c1,c2,a,func,ld1,ld2,rFunc,st,inds1,inds2,part,acc,res,rV) => reflectPure(Block_reduce(f(c1),f(c2),f(a),f(func),f(ld1),f(ld2),f(rFunc),f(st),inds1,inds2,part,acc,res,rV)(e.mT,e.ctx))
    case Reflect(e@Block_reduce(c1,c2,a,func,ld1,ld2,rFunc,st,inds1,inds2,part,acc,res,rV)) => reflectMirrored(Reflect(Block_reduce(f(c1),f(c2),f(a),f(func),f(ld1),f(ld2),f(rFunc),f(st),inds1,inds2,part,acc,res,rV)(e.mT,e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A], pos))

    case e@Counter_new(s,e,t,p) => counter_new(f(s),f(e),f(t),p)(pos)
    case Reflect(e@Counter_new(s,e,t,p), u, es) => reflectMirrored(Reflect(Counter_new(f(s),f(e),f(t),p)(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@Counterchain_new(counters,nIter) => reflectPure(Counterchain_new(f(counters),f(nIter))(e.ctx))(mtype(manifest[A]), pos)
    case Reflect(e@Counterchain_new(counters), u, es) => reflectMirrored(Reflect(Counterchain_new(f(counters),f(nIter))(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(e, f)
  }

  // --- Dependencies
  override def syms(e: Any): List[Sym[Any]] = e match {
    case Pipe_foreach(chain, func, _) => syms(chain) ::: syms(func)
    case e: Pipe_reduce[_,_] => syms(e.cchain) ::: syms(e.accum) ::: syms(e.func) ::: syms(e.rFunc) ::: syms(e.ldFunc) ::: syms(e.stFunc)
    case e: Block_reduce[_] => syms(e.ccOuter) ::: syms(e.ccInner) ::: syms(e.accum) ::: syms(e.func) ::: syms(e.resLdFunc) ::: syms(e.ldFunc) ::: syms(e.rFunc) ::: syms(e.stFunc)
    case Counterchain_new(ctrs, nIters) => syms(ctrs) ::: syms(nIters)
    case _ => super.syms(e)
  }
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case Pipe_foreach(chain, func, _) => readSyms(chain) ::: readSyms(func)
    case e: Pipe_reduce[_,_] => readSyms(e.cchain) ::: readSyms(e.accum) ::: readSyms(e.func) ::: readSyms(e.rFunc) ::: readSyms(e.ldFunc) ::: readSyms(e.stFunc)
    case e: Block_reduce[_] => readSyms(e.ccOuter) ::: readSyms(e.ccInner) ::: readSyms(e.accum) ::: readSyms(e.func) ::: readSyms(e.resLdFunc) ::: readSyms(e.ldFunc) ::: readSyms(e.rFunc) ::: readSyms(e.stFunc)
    case Counterchain_new(ctrs, nIters) => readSyms(ctrs) ::: readSyms(nIters)
    case _ => super.readSyms(e)
  }
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case Pipe_foreach(chain, func, _) => freqCold(func) ::: freqCold(chain)
    case e: Pipe_reduce[_,_] => freqCold(e.func) ::: freqCold(e.rFunc) ::: freqCold(e.ldFunc) ::: freqCold(e.stFunc) ::: freqCold(e.cchain) ::: freqCold(e.accum)
    case e: Block_reduce[_] => freqNormal(e.ccOuter) ::: freqNormal(e.ccInner) ::: freqNormal(e.accum) ::: freqNormal(e.func) ::: freqNormal(e.resLdFunc) ::: freqNormal(e.ldFunc) ::: freqNormal(e.rFunc) ::: freqNormal(e.stFunc)
    case Counterchain_new(ctrs, nIters) => freqNormal(ctrs) ::: freqNormal(nIters)
    case _ => super.symsFreq(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Pipe_foreach(chain,func,inds) => inds ::: effectSyms(func) ::: effectSyms(chain)
    case e: Pipe_reduce[_,_] => e.inds ::: List(e.rV._1, e.rV._2, e.acc, e.res) ::: effectSyms(e.cchain) ::: effectSyms(e.func) ::: effectSyms(e.rFunc) ::: effectSyms(e.ldFunc) ::: effectSyms(e.stFunc) ::: effectSyms(e.accum)
    case e: Block_reduce[_] => e.indsOuter ::: e.indsInner ::: List(e.rV._1, e.rV._2, e.acc, e.res, e.part) ::: effectSyms(e.ccOuter) ::: effectSyms(e.ccInner) ::: effectSyms(e.accum) ::: effectSyms(e.func) ::: effectSyms(e.resLdFunc) ::: effectSyms(e.ldFunc) ::: effectSyms(e.rFunc) ::: effectSyms(e.stFunc)
    case Counterchain_new(ctrs, nIters) => effectSyms(nIters)
    case _ => super.boundSyms(e)
  }
}

trait ScalaGenPipeTemplateOps extends ScalaGenEffect {
  val IR: PipeTemplateOpsExp with DHDLIdentifiers
  import IR._

  def emitNestedLoop(iters: List[Sym[FixPt[Signed,B32,B0]]], cchain: Exp[CounterChain])(emitBlk: => Unit) = {
    iters.zipWithIndex.foreach{ case (iter,idx) =>
      stream.println("for( " + quote(iter) + " <- " + quote(cchain) + ".apply(" + idx + ".toInt)) {")
    }
    emitBlk
    stream.println("}" * iters.length)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Counter_new(start,end,step,_) =>
      stream.println("val "+quote(sym)+" = "+quote(start)+" until "+quote(end)+" by "+quote(step))

    case e@Counterchain_new(counters, nIter) =>
      emitValDef(sym, "Array(" + counters.map(quote).mkString(", ") + ")")

    case e@Pipe_foreach(cchain, func, inds) =>
      emitNestedLoop(inds, cchain){ emitBlock(func) }
      emitValDef(sym, "()")

    case e@Pipe_reduce(cchain, accum, ldFunc, stFunc, func, rFunc, inds, acc, res, rV) =>
      emitValDef(acc, quote(accum)) // Assign bound accumulator to accum
      emitNestedLoop(inds, cchain){
        emitBlock(func)             // Map function
        emitBlock(ldFunc)           // Load corresponding value from accumulator
        emitValDef(rV._1, quote(getBlockResult(ldFunc)))
        emitValDef(rV._2, quote(getBlockResult(func)))
        emitBlock(rFunc)            // Reduction function
        emitValDef(res, quote(getBlockResult(rFunc)))
        emitBlock(stFunc)           // Write back to accumulator
      }
      emitValDef(sym, "()")

    case e@Block_reduce(ccOuter, ccInner, accum, func, ldPart, ldFunc, rFunc, stFunc, indsOuter, indsInner, part, acc, res, rV) =>
      emitValDef(acc, quote(accum)) // Assign bound accumulator to accum
      emitNestedLoop(indsOuter, ccOuter){
        emitBlock(func)
        emitValDef(part, getBlockResult(func))

        emitNestedLoop(indsInner, ccInner){
          emitBlock(ldPart)
          emitBlock(ldFunc)
          emitValDef(rV._1, quote(getBlockResult(ldPart)))
          emitValDef(rV._2, quote(getBlockResult(ldFunc)))
          emitBlock(rFunc)
          emitValDef(res, quote(getBlockResult(rFunc)))
          emitBlock(stFunc)
        }
      }
      emitValDef(sym, "()")


    case _ => super.emitNode(sym, rhs)
  }
}

trait DotGenPipeTemplateOps extends DotGenEffect{
  val IR: PipeTemplateOpsExp with FixPtOpsExp with FltPtOpsExp with TpesOpsExp with NosynthOpsExp
	        with OffChipMemOpsExp with RegOpsExp with DHDLCodegenOps

  import IR.{Sym, Exp, Def}
  import IR.{ConstFix, ConstFlt, ConstBit, EatReflect}
  import IR.{Counterchain_new, Offchip_new, Reg_new, Set_arg, Set_mem, Pipe_foreach, Pipe_reduce}
  import IR.{CounterChain, FixPt, Signed, B32, B0}

	def emitNestedIdx(cchain:Exp[CounterChain], inds:List[Sym[FixPt[Signed,B32,B0]]]) = cchain match {
    case Def(EatReflect(Counterchain_new(counters))) =>
	     inds.zipWithIndex.foreach {case (iter, idx) => emitAlias(iter, counters(idx)) }
	}

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
	  case e@Counter_new(start,end,step,_) =>
      val cic = "\"" + quote(counterInnerColor) + "\""
      stream.println(""+quote(sym)+" [ label=\""+quote(sym)+"\" shape=\"box\" style=\"filled,rounded\"" + "")
      stream.println("color="+quote(cic)+" ]" + "")
      stream.println(""+quote(start)+" -> "+quote(sym)+" [ label=\"start\" ]" + "")
      stream.println(""+quote(end)+" -> "+quote(sym)+" [ label=\"end\" ]" + "")
      stream.println(""+quote(step)+" -> "+quote(sym)+" [ label=\"step\" ]" + "")

    case e@Counterchain_new(counters,nIter) =>
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s""" label=${quote(sym)} """)
      emit(s""" style="rounded, filled" """)
      emit(s""" fillcolor="${counterColor}" """)
      counters.foreach{ ctr =>
        emit(s"""   ${quote(ctr)}""")
      }
      emit("}")

    case e@Pipe_foreach(cchain, func, inds) =>
      emitNestedIdx(cchain, inds)
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""label=\"${quote(sym)}\"""")
      emit(s"""color=\"gray\"""")
      emitBlock(func)             // Map function
      emit("}")

    case e@Pipe_reduce(cchain, accum, ldFunc, stFunc, func, rFunc, inds, acc, res, rV) =>
      emitAlias(acc, accum)
      emitNestedIdx(cchain, inds)
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""label=\"${quote(sym)}\"""")
      emit(s"""color=\"gray\"""")
      emit(s"""define(`${quote(acc)}', `${quote(accum)}')""")
      emitBlock(func)
      emitBlock(ldFunc)
      emitAlias(rV._1, getBlockResult(ldFunc))
      emitAlias(rV._2, getBlockResult(func))
      emitBlock(rFunc)
      emitAlias(res, getBlockResult(rFunc))
      emitBlock(stFunc)
      emit("}")

    case e@Block_reduce(ccOuter, ccInner, accum, func, ldPart, ldFunc, rFunc, stFunc, indsOuter, indsInner, part, acc, res, rV) =>
      emit(s"""subgraph ${quote(sym)} {""")
      emit(s"""  label = quote(sym)""")
      emit(s"""  style = "filled" """)
      emit(s"""  fillcolor = "$quote{mpFillColor}" """)
      emit(s"""  color = "$quote{mpBorderColor}" """)
      val sym_ctrl = quote(sym) + "_ctrl"
      emit(s"""  ${sym_ctrl} [label="ctrl" height=0 style="filled" fillcolor="${mpBorderColor} "]""")
      emit(s"""}""")

    case _ => super.emitNode(sym,rhs)
	}

  override def quote(x: Exp[Any]) = x match {
		case s@Sym(n) => s.tp.erasure.getSimpleName() + "_x" + n
    case _ => super.quote(x)
  }
}

trait MaxJGenPipeTemplateOps extends MaxJGenEffect {
  val IR: PipeTemplateOpsExp with FixPtOpsExp with FltPtOpsExp with TpesOpsExp with NosynthOpsExp
	        with OffChipMemOpsExp with RegOpsExp with DHDLCodegenOps

  import IR.{Sym, Exp, Def}
  import IR.{ConstFix, ConstFlt, ConstBit, EatReflect}
  import IR.{Counterchain_new, Offchip_new, Reg_new, Set_arg, Set_mem, Pipe_foreach, Pipe_reduce}
  import IR.{CounterChain, FixPt, Signed, B32, B0}

  // TODO
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Counter_new(start,end,step,par) =>

    case e@Counterchain_new(counters, nIter) =>

    case e@Pipe_foreach(cchain, func, inds) =>

    case e@Pipe_reduce(cchain, accum, ldFunc, stFunc, func, rFunc, inds, acc, res, rV) =>

    case e@Block_reduce(ccOuter, ccInner, accum, func, ldPart, ldFunc, rFunc, stFunc, indsOuter, indsInner, part, acc, res, rV) =>

    case _ => super.emitNode(sym, rhs)
  }

  override def quote(x: Exp[Any]) = x match {
		case s@Sym(n) => s.tp.erasure.getSimpleName() + "_x" + n
    case _ => super.quote(x)
  }
}

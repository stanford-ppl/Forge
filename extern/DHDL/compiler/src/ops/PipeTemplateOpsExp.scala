package dhdl.compiler.ops

import scala.virtualization.lms.common.{EffectExp, ScalaGenEffect, DotGenEffect, MaxJGenEffect}
import scala.reflect.{Manifest,SourceContext}
import scala.collection.mutable.Set
import java.io.{File, FileWriter, PrintWriter}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait ControllerTemplateOpsExp extends ControllerTemplateOps with MemoryTemplateOpsExp with CounterToolsExp with EffectExp {
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
    ccOuter: Exp[CounterChain], // Counter chain for map (outer) loop
    ccInner: Exp[CounterChain], // Counter chain for reduce (inner) loop
    accum:  Exp[BRAM[T]],
    // - Reified blocks
    func: Block[BRAM[T]],       // Map function
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
  )(implicit val ctx: SourceContext) extends Def[Pipeline] {
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
    setProps(acc, getProps(accum))

    val ldBlk = reifyEffects(__mem.ld(acc, inds))

   // Reified reduction function
    val rV = (fresh[T], fresh[T])
    val rBlk = reifyEffects( rFunc(rV._1, rV._2) )

    // Reified store function
    val res = fresh[T]
    val stBlk = reifyEffects(__mem.st(acc, inds, res))

    val effects = summarizeEffects(mBlk) andAlso summarizeEffects(ldBlk) andAlso
                  summarizeEffects(rBlk) andAlso summarizeEffects(stBlk) andAlso Write(List(accum.asInstanceOf[Sym[C[T]]]))

    reflectEffect(Pipe_reduce[T,C](cchain, accum, ldBlk, stBlk, mBlk, rBlk, is, acc, res, rV), effects.star)
  }

  def block_reduce[T:Manifest](cchain: Rep[CounterChain], accum: Rep[BRAM[T]], func: Rep[Indices] => Rep[BRAM[T]], rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[Pipeline] = {
    val isMap = List.fill(sizeOf(cchain)){ fresh[FixPt[Signed,B32,B0]] }   // Map loop indices
    val indsMap = indices_create(isMap)

    // Reified map function
    val mBlk = reifyEffects( func(indsMap) )

    // Reduce loop indices
    val ctrsRed = dimsOf(accum).map{dim => Counter(max = dim.as[Index]) }
    val cchainRed = CounterChain(ctrsRed:_*)
    val isRed = List.fill(sizeOf(cchainRed)){ fresh[FixPt[Signed,B32,B0]] } // Reduce loop indices
    val indsRed = indices_create(isRed)

    val part = fresh[BRAM[T]]
    setProps(part, getProps(mBlk))
    // Partial result load
    val ldPartBlk = reifyEffects( canBramMem[T].ld(part, indsRed) )

    val acc = reflectMutableSym( fresh[BRAM[T]] )
    setProps(acc, getProps(accum))
    // Accumulator load
    val ldBlk = reifyEffects( canBramMem[T].ld(acc, indsRed) )

    val rV = (fresh[T],fresh[T])
    // Reified reduction function
    val rBlk = reifyEffects( rFunc(rV._1, rV._2) )

    val res = fresh[T]
    // Accumulator store function
    val stBlk = reifyEffects( canBramMem[T].st(acc, indsRed, res) )

    val effects = summarizeEffects(mBlk) andAlso summarizeEffects(ldPartBlk) andAlso
                  summarizeEffects(ldBlk) andAlso summarizeEffects(rBlk) andAlso summarizeEffects(stBlk) andAlso Write(List(accum.asInstanceOf[Sym[BRAM[T]]]))

    reflectEffect(Block_reduce[T](cchain, cchainRed, accum, mBlk, ldPartBlk, ldBlk, rBlk, stBlk, isMap, isRed, part, acc, res, rV), effects.star)
  }

  def counter_new(start: Rep[FixPt[Signed,B32,B0]],end: Rep[FixPt[Signed,B32,B0]],step: Rep[FixPt[Signed,B32,B0]], par: Int)(implicit ctx: SourceContext) = {
    val p = param[Int](par)
    reflectEffect[Counter](Counter_new(start,end,step,p)(ctx))
  }

  private def counterSplit(x: Rep[Counter]): (Rep[Index],Rep[Index],Rep[Index],Param[Int]) = x match {
    case Def(EatReflect(Counter_new(start,end,step,par))) => (start,end,step,par)
    case _ => throw new Exception("Could not find def for counter")
  }

  def counterchain_new(counters: List[Rep[Counter]])(implicit ctx: SourceContext): Rep[CounterChain] = {
    val ctrSplit = counters.map{ctr => counterSplit(ctr)}
    val starts: List[Rep[Index]] = ctrSplit.map(_._1)
    val ends:   List[Rep[Index]] = ctrSplit.map(_._2)
    val steps:  List[Rep[Index]] = ctrSplit.map(_._3)
    val pars:   List[Rep[Index]] = ctrSplit.map(_._4).map(int_to_fix[Signed,B32](_)) // HACK: Convert Int param to fixed point

    val nIter: Block[Index] = reifyEffects {
      val lens: List[Rep[Index]] = starts.zip(ends).map{
        case (ConstFix(x: Int),ConstFix(y: Int)) => (y - x).as[FixPt[Signed,B32,B0]]
        case (ConstFix(0), end) => end
        case (start, end) => sub_fix(end, start)
      }
      val total: List[Rep[Index]] = (lens,steps,pars).zipped.map {
        case (len, ConstFix(1), par) => div_fix(len, par) // Should round correctly here
        case (len, step, par) => div_fix(len, mul_fix(step, par))
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

    case e@Block_reduce(c1,c2,a,func,ld1,ld2,rFunc,st,inds1,inds2,part,acc,res,rV) => reflectPure(Block_reduce(f(c1),f(c2),f(a),f(func),f(ld1),f(ld2),f(rFunc),f(st),inds1,inds2,part,acc,res,rV)(e.mT,e.ctx))(mtype(manifest[A]), pos)
    case Reflect(e@Block_reduce(c1,c2,a,func,ld1,ld2,rFunc,st,inds1,inds2,part,acc,res,rV), u, es) => reflectMirrored(Reflect(Block_reduce(f(c1),f(c2),f(a),f(func),f(ld1),f(ld2),f(rFunc),f(st),inds1,inds2,part,acc,res,rV)(e.mT,e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@Counter_new(s,end,t,p) => reflectPure( Counter_new(f(s),f(end),f(t),p)(pos) )(mtype(manifest[A]), pos)
    case Reflect(e@Counter_new(s,end,t,p), u, es) => reflectMirrored(Reflect(Counter_new(f(s),f(end),f(t),p)(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@Counterchain_new(counters,nIter) => reflectPure(Counterchain_new(f(counters),f(nIter))(e.ctx))(mtype(manifest[A]), pos)
    case Reflect(e@Counterchain_new(counters,nIter), u, es) => reflectMirrored(Reflect(Counterchain_new(f(counters),f(nIter))(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(e, f)
  }

  override def propagate(lhs: Exp[Any], rhs: Def[Any]) = rhs match {
    case Pipe_reduce(c,a,ld,st,func,rFunc,inds,acc,res,rV) =>
      setProps(acc, getProps(a))
      setProps(res, getProps(rFunc))
      setProps(rV._1, getProps(func))
      setProps(rV._2, getProps(func))
    case Block_reduce(c1,c2,a,func,ld1,ld2,rFunc,st,inds1,inds2,part,acc,res,rV) =>
      setProps(acc, getProps(a))
      setProps(part, getProps(func))
      setProps(res, getProps(rFunc))
      setProps(rV._1, getProps(ld1))
      setProps(rV._2, getProps(ld2))

    case _ => super.propagate(lhs, rhs)
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

trait ScalaGenControllerTemplateOps extends ScalaGenEffect {
  val IR: ControllerTemplateOpsExp with DHDLIdentifiers
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
        emitValDef(part, quote(getBlockResult(func)))

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

trait DotGenControllerTemplateOps extends DotGenEffect{
  val IR: ControllerTemplateOpsExp with TpesOpsExp // with NosynthOpsExp
          with OffChipMemOpsExp with RegOpsExp with DHDLCodegenOps

  import IR._ //{__ifThenElse => _, Nosynth___ifThenElse => _, __whileDo => _,
              // Forloop => _, println => _ , _}

	val emittedCtrChain = Set.empty[Exp[Any]]

  override def initializeGenerator(buildDir:String): Unit = {
		emittedCtrChain.clear
		super.initializeGenerator(buildDir)
	}

	def emitCtrChain(cchain: Exp[CounterChain]):Unit = {
		val Def(EatReflect(d)) = cchain
		emitCtrChain(cchain.asInstanceOf[Sym[CounterChain]],
									 d.asInstanceOf[Def[Any]])
	}

	def emitCtrChain(sym: Sym[Any], rhs: Def[Any]):Unit = rhs match {
	  case e@Counterchain_new(counters, nIter) =>
			if (!emittedCtrChain.contains(sym)) {
				emittedCtrChain += sym
    		emit(s"""subgraph cluster_${quote(sym)} {""")
    		emit(s""" label=${quote(sym)} """)
    		emit(s""" style="rounded, filled" """)
    		emit(s""" fillcolor=$counterColor""")
    		counters.foreach{ ctr =>
    		  emit(s"""   ${quote(ctr)}""")
    		}
    		emit("}")
			}
		case _ => 
	}

	def emitNestedIdx(cchain:Exp[CounterChain], inds:List[Sym[FixPt[Signed,B32,B0]]]) = cchain match {
    case Def(EatReflect(Counterchain_new(counters, nIter))) =>
	     inds.zipWithIndex.foreach {case (iter, idx) => emitAlias(iter, counters(idx)) }
	}
	
  override def emitFileHeader() {
		super.emitFileHeader()
    emit(s"compound=true")
    emit(s"""graph [splines="ortho" clusterrank="local" rankdir = "LR"]""")
    emit(s"edge [arrowsize=$arrowSize penwidth=$edgeThickness]")
    emit(s"""node [fontsize=$fontsize shape=$defaultShape style="filled" fillcolor=$bgcolor ]""")
    emit(s"fontsize=$fontsize")
	}

  def emitBlock(y: Block[Any], name:String, label:String, color:String): Unit = { 
      emit(s"""subgraph cluster_${name} {""")
      emit(s"""label="${name}" """)
      emit(s"""style="filled" """)
			emit(s"""fillcolor=$color""")
			emit(s"""color=none""")
			emitBlock(y)
			emit(s"""}""")
	}

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Counter_new(start,end,step,_) =>
			var l = s""""${quote(sym)}"""
			if (quote(start).forall(_.isDigit)) {
				l += "|start=" + quote(start)
			} else {
				emitEdge(start, sym, "start")
			}
			if (quote(end).forall(_.isDigit)) {
				l += "|end=" + quote(end)
			} else {
				emitEdge(end, sym, "end")
			}
			if (quote(step).forall(_.isDigit)) {
				l += "|step=" + quote(step)
			} else {
				emitEdge(step, sym, "step")
			}
			l += "\"" 
      emit(s"""${quote(sym)} [ label=$l shape="record" style="filled,rounded"
						color=$counterInnerColor ]""")

	  case e@Counterchain_new(counters, nIter) =>
			//TODO: check whether parent of cchain is empty, if is emit ctrchain
			//Uncomment after analysis complete
			//if (parentOf(sym).isEmpty) {
			//	emitCtrChain(sym, rhs)
			//}

    case e@Pipe_foreach(cchain, func, inds) =>
			var label = quote(sym)
			styleOf(sym.asInstanceOf[Rep[Pipeline]]) match {
				case Coarse => label = label.replace("DHDLPipeline", "MetaPipe")
				case Fine => label = label.replace("DHDLPipeline", "Pipe")
				case Disabled => label = label.replace("DHDLPipeline", "Sequential")
			}
      emitNestedIdx(cchain, inds)
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""label="$label"""")
      emit(s"""color=$pipeBorderColor""")
      emit(s"""style="bold, filled" """)
			emit(s"""fillcolor=$pipeFillColor""")
			emitCtrChain(cchain)
      emitBlock(func, quote(sym) + "_foreachFunc", "foreachFunc", foreachFillColor)             // Map function
      emit("}")

    case e@Pipe_reduce(cchain, accum, ldFunc, stFunc, func, rFunc, inds, acc, res, rV) =>
			var label = quote(sym)
			styleOf(sym.asInstanceOf[Rep[Pipeline]]) match {
				case Coarse => label = label.replace("DHDLPipeline", "MetaPipe")
				case Fine => label = label.replace("DHDLPipeline", "Pipe")
				case Disabled => label = label.replace("DHDLPipeline", "Sequential")
			}
      emitAlias(acc, accum)
      emitNestedIdx(cchain, inds)
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""label="${quote(sym)}"""")
      emit(s"""color=$pipeBorderColor""")
      emit(s"""style="bold, filled" """)
			emit(s"""fillcolor=$pipeFillColor""")
      emit(s"""define(`${quote(acc)}', `${quote(accum)}')""")
			val Def(EatReflect(d)) = cchain
			emitCtrChain(cchain)
      emitBlock(func, quote(sym) + "_mapFunc", "mapFunc", mapFillColor)             // Map function
      emitBlock(ldFunc, quote(sym) + "_ldFunc", "ldFunc", ldFillColor)             // Map function
      emitAlias(rV._1, getBlockResult(ldFunc))
      emitAlias(rV._2, getBlockResult(func))
      emitBlock(rFunc, quote(sym) + "_reduceFunc", "reduceFunc", reduceFillColor)             // Map function
      emitAlias(res, getBlockResult(rFunc))
      emitBlock(stFunc, quote(sym) + "_stFunc", "stFunc" , stFillColor)             // Map function
      emit("}")

    case e@Block_reduce(ccOuter, ccInner, accum, func, ldPart, ldFunc, rFunc, stFunc, indsOuter, indsInner, part, acc, res, rV) =>
      emit(s"""subgraph ${quote(sym)} {""")
      emit(s"""  label = quote(sym)""")
      emit(s"""  style = "filled" """)
      emit(s"""  fillcolor = "${quote(mpFillColor)}" """)
      emit(s"""  color = "${quote(mpBorderColor)}" """)
      val sym_ctrl = quote(sym) + "_ctrl"
      emit(s"""  ${sym_ctrl} [label="ctrl" height=0 style="filled" fillcolor="${mpBorderColor} "]""")
      emit(s"""}""")

    case _ => super.emitNode(sym,rhs)
  }

  override def quote(x: Exp[Any]) = x match {
		case s@Sym(n) => s match { 
				case Def(ConstFix(n)) => n.toString
				case Def(ConstFlt(n)) => n.toString
				case _ => {
					var tstr = s.tp.erasure.getSimpleName() 
					tstr = tstr.replace("DHDL","") 
//					tstr = tstr.replace("Register", regType(s) match {
//						case Regular => "Reg"
//						case ArgumentIn => "ArgIn"
//						case ArgumentOut => "ArgOut"
//					}) 
					tstr = tstr.replace("BlockRAM", "BRAM")
					tstr + (if (nameOf(s)!="") "_" else "") + nameOf(s) + n
				}
			}
    case _ => super.quote(x)
  }
}

trait MaxJGenControllerTemplateOps extends MaxJGenEffect {
  val IR: ControllerTemplateOpsExp with TpesOpsExp //with NosynthOpsExp
          with OffChipMemOpsExp with RegOpsExp with CounterOpsExp with MetaPipeOpsExp with
					DHDLPrimOpsExp with DHDLCodegenOps with CounterToolsExp

 import IR._ //{__ifThenElse => _, Nosynth___ifThenElse => _, __whileDo => _,
             // Forloop => _, println => _ , _}

  override def quote(x: Exp[Any]) = x match {
		case s@Sym(n) => {
			var tstr = s.tp.erasure.getSimpleName() 
			tstr = tstr.replace("DHDL","") 
//			tstr = tstr.replace("Register", regType(s) match {
//				case Regular => "Reg"
//				case ArgumentIn => "ArgIn"
//				case ArgumentOut => "ArgOut"
//			}) 
			tstr = tstr.replace("BlockRAM", "BRAM")
			tstr + (if (nameOf(s)!="") "_" else "") + nameOf(s) + n
		}
    case _ => super.quote(x)
  }

  /* Set of control nodes which already have their enable signal emitted */
  val enDeclaredSet = Set.empty[Exp[Any]]

  /* Set of control nodes which already have their done signal emitted */
  val doneDeclaredSet = Set.empty[Exp[Any]]

  override def initializeGenerator(buildDir:String): Unit = {
		enDeclaredSet.clear
		doneDeclaredSet.clear
		super.initializeGenerator(buildDir)
	}

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Counterchain_new(counters,nIters) =>

			//TODO: this seems allow a pipe to be a sequential, which wouldn't work
    case e@Pipe_foreach(cchain, func, inds) =>
			styleOf(sym.asInstanceOf[Rep[Pipeline]]) match {
				case Coarse =>
				case Fine =>
					//TODO: assume bram not write to accum
			styleOf(sym.asInstanceOf[Rep[Pipeline]]) match {
				case Coarse => 
				case Fine => 
				case Disabled => 
			}
    			//val writesToAccumRam = mapNode.nodes.filter { _.isInstanceOf[St] }.exists { _.asInstanceOf[St].mem.isAccum }
					val writesToAccumRam = false
					emitPipeProlog(sym, cchain, inds, writesToAccumRam)
					emitPipeForEachEpilog(sym, writesToAccumRam, cchain, inds(0))
				case Disabled =>
					val Def(EatReflect(Counterchain_new(counters, nIters))) = cchain
					emitSequential(sym, cchain, getBlockResult(nIters))
			}
      emitBlock(func)             // Map function

    case e@Pipe_reduce(cchain, accum, ldFunc, stFunc, func, rFunc, inds, acc, res, rV) =>
			styleOf(sym.asInstanceOf[Rep[Pipeline]]) match {
				case Coarse =>
				case Fine =>
					//TODO
					val writesToAccumRam = false
					emitPipeProlog(sym, cchain, inds, writesToAccumRam)
					//TODO
					val specializeReduce = false;
					emitPipeReduceEpilog(sym, specializeReduce, cchain)
				case Disabled =>
					val Def(EatReflect(Counterchain_new(counters, nIters))) = cchain
					emitSequential(sym, cchain, getBlockResult(nIters))
			}
      emitBlock(func)
      emitBlock(ldFunc)
      emitBlock(rFunc)
      emitBlock(stFunc)

		case e@Pipe_parallel(func: Block[Unit]) => 

			// If control signals have not yet been defined, define them here
			if (parentOf(sym).isEmpty) {
				emit(s"""DFEVar ${quote(sym)}_en = top_en;""")
				emit(s"""DFEVar ${quote(sym)}_done = dfeBool().newInstance(this);""")
				emit(s"""top_done <== ${quote(sym)}_done;""")
				enDeclaredSet += sym
				doneDeclaredSet += sym
			}

			emitComment(s"""Parallel ${quote(sym)} { """)
			emit(s"""
				SMIO ${quote(sym)}_sm = addStateMachine("${quote(sym)}_sm", new ${quote(sym)}_ParSM(this));
				${quote(sym)}_sm.connectInput("sm_en", ${quote(sym)}_en);
				${quote(sym)}_done <== stream.offset(${quote(sym)}_sm.getOutput("sm_done"),-1);
				""")

			childrenOf(sym).zipWithIndex.foreach { case (c, idx) =>
				emit(s"""DFEVar ${quote(c)}_done = dfeBool().newInstance(this);
					${quote(sym)}_sm.connectInput("s${idx}_done", ${quote(c)}_done);
					DFEVar ${quote(c)}_en = ${quote(sym)}_sm.getOutput("s${quote(idx)}_en");""")
				enDeclaredSet += c
				doneDeclaredSet += c
			}

			emitComment(s"""} Parallel ${quote(sym)} """)

    case _ => super.emitNode(sym,rhs)
  }

	def emitSequential(sym:Sym[Any], cchain: Exp[CounterChain], totIter: Rep[FixPt[Signed,B32,B0]]) = {

		// If control signals have not yet been defined, define them here
		if (parentOf(sym).isEmpty) {
			emit(s"""DFEVar ${quote(sym)}_en = top_en;""")
			emit(s"""DFEVar ${quote(sym)}_done = dfeBool().newInstance(this);""")
			emit(s"""top_done <== ${quote(sym)}_done;""")
			enDeclaredSet += sym
			doneDeclaredSet += sym
		}

		emit(s"""DFEVar ${quote(sym)}_numIter = ${quote(totIter)};""")
		emitComment(s"""Sequential ${quote(sym)} {""")
		emit(s"""
			SMIO ${quote(sym)}_sm = addStateMachine("${quote(sym)}_sm", new ${quote(sym)}_SeqSM(this));
			${quote(sym)}_sm.connectInput("sm_en", ${quote(sym)}_en);
			${quote(sym)}_sm.connectInput("sm_numIter", ${quote(sym)}_numIter);
			${quote(sym)}_done <== stream.offset(${quote(sym)}_sm.getOutput("sm_done"),-1);
			""")

		emit(s"""DFEVar ${quote(sym)}_rst_en = ${quote(sym)}_sm.getOutput("rst_en");""")

		childrenOf(sym).zipWithIndex.foreach {case (c, idx) =>
			emit(s"""DFEVar ${c}_done = dfeBool().newInstance(this);
				${quote(sym)}_sm.connectInput("s${idx}_done", ${quote(c)}_done);
				DFEVar ${quote(c)}_en = ${quote(sym)}_sm.getOutput("s${quote(idx)}_en");""")
			enDeclaredSet += c
			doneDeclaredSet += c
		}

		emit(s"""DFEVar ${quote(cchain)}_done = dfeBool().newInstance(this);""")
		doneDeclaredSet += cchain
		emitMaxJCounterChain(cchain, Some(s"${quote(childrenOf(sym).last)}_done"))

		emit(s"""} Sequential ${quote(sym)} """)
	}

	def emitPipeProlog(sym: Sym[Any], cchain: Exp[CounterChain], inds:List[Sym[FixPt[Signed,B32,B0]]],
		writesToAccumRam:Boolean) = {
		val Def(EatReflect(Counterchain_new(counters, nIter))) = cchain
    if (parentOf(sym).isEmpty) {
      emit(s"""DFEVar ${quote(sym)}_en = top_en;""")
      emit(s"""DFEVar ${quote(sym)}_done = dfeBool().newInstance(this);""")
      emit(s"""top_done <== ${quote(sym)}_done;""")
      enDeclaredSet += sym
      doneDeclaredSet += sym
    }

    emit(s"""SMIO ${quote(sym)}_sm = addStateMachine("${quote(sym)}_sm", new PipeSM(this, ${counters.size}));""")
    emit(s"""${quote(sym)}_sm.connectInput("sm_en", ${quote(sym)}_en);""")
    emit(s"""${quote(sym)}_done <== stream.offset(${quote(sym)}_sm.getOutput("sm_done"),-1);""")

    counters.zipWithIndex.map {case (ctr,i) =>
      val Def(EatReflect(Counter_new(start, end, step, par))) = ctr
      emit(s"""${quote(sym)}_sm.connectInput("sm_maxIn_$i", ${quote(end)});""")
      emit(s"""DFEVar ${quote(ctr)}_max = ${quote(sym)}_sm.getOutput("ctr_maxOut");""")
    }

    emit(s"""DFEVar ${quote(cchain)}_done = dfeBool().newInstance(this);""")
    emit(s"""${quote(sym)}_sm.connectInput("ctr_done", ${quote(cchain)}_done);""")
    emit(s"""DFEVar ${quote(cchain)}_en_from_pipesm = ${quote(sym)}_sm.getOutput("ctr_en");""")
    doneDeclaredSet += cchain

    emit(s"""DFEVar ${quote(sym)}_rst_done = dfeBool().newInstance(this);""")
    emit(s"""${quote(sym)}_sm.connectInput("rst_done", ${quote(sym)}_rst_done);""")
    emit(s"""DFEVar ${quote(sym)}_rst_en = ${quote(sym)}_sm.getOutput("rst_en");""")

    // TODO: Unit counter not as important with addition of unit pipe
    if (isUnitCounterChain(cchain) && !writesToAccumRam) {
      emit(s"""${quote(sym)}_rst_done <== constant.var(true);""")
    } else {
      emit(s"""OffsetExpr ${quote(sym)}_offset = stream.makeOffsetAutoLoop("${quote(sym)}_offset");""")
      emit(s"""${quote(sym)}_rst_done <== stream.offset(${quote(sym)}_rst_en, -${quote(sym)}_offset-1);""")
    }

  }

	def emitPipeForEachEpilog(sym: Sym[Any], writesToAccumRam:Boolean, cchain:Exp[CounterChain],
		firstInd:Sym[FixPt[Signed,B32,B0]]) = {
    // Check if this is an accumulator map - whether it writes to a memory marked 'isAccum'
    if (writesToAccumRam) {
      emitMaxJCounterChain(cchain, Some(s"${quote(cchain)}_en_from_pipesm | ${quote(sym)}_rst_en"),
            Some(s"stream.offset(${quote(cchain)}_en_from_pipesm & ${quote(cchain)}_chain.getCounterWrap(${quote(firstInd)}), -${quote(sym)}_offset-1)"))
    } else {
      emitMaxJCounterChain(cchain, Some(s"${quote(cchain)}_en_from_pipesm"))
    }
	}
	
	def emitPipeReduceEpilog(sym: Sym[Any], specializeReduce:Boolean, cchain:Exp[CounterChain]) = {
    if (specializeReduce) {
      emitMaxJCounterChain(cchain, Some(s"${quote(cchain)}_en_from_pipesm"))
    } else {
      emit(s"""DFEVar ${quote(sym)}_loopLengthVal = ${quote(sym)}_offset.getDFEVar(this, dfeUInt(8));""")
      emit(s"""CounterChain ${quote(sym)}_redLoopChain =
				control.count.makeCounterChain(${quote(cchain)}_en_from_pipesm);""")
      emit(s"""DFEVar ${quote(sym)}_redLoopCtr = ${quote(sym)}_redLoopChain.addCounter(${quote(sym)}_loopLengthVal, 1);""")
      emit(s"""DFEVar ${quote(sym)}_redLoop_done = stream.offset(${quote(sym)}_redLoopChain.getCounterWrap(${quote(sym)}_redLoopCtr), -1);""")
      emitMaxJCounterChain(cchain, Some(s"${quote(cchain)}_en_from_pipesm & ${quote(sym)}_redLoop_done"))
    }
	}

	def emitMaxJCounterChain(cchain: Exp[CounterChain], en: Option[String], done: Option[String]=None) = {
		val sym = cchain
    // 'En' and 'done' signal contract: Enable signal is declared here, done signal must be
    // declared before this method is called.
    // Both signals are defined here.
    if (!enDeclaredSet.contains(sym)) {
      emit(s"""DFEVar ${quote(sym)}_en = ${en.get};""")
      enDeclaredSet += sym
    }
    emit(s"""CounterChain ${quote(sym)}_chain = control.count.makeCounterChain(${quote(sym)}_en);""")

    // For Pipes, max must be derived from PipeSM
    // For everyone else, max is as mentioned in the ctr
		val Def(EatReflect(Counterchain_new(counters, nIter))) = cchain
		counters.zipWithIndex.map {case (ctr,i) =>
			val Def(EatReflect(Counter_new(start, end, step, _))) = ctr
			val max = parentOf(cchain.asInstanceOf[Rep[CounterChain]]) match {
				case Some(s) => s.tp match {
					case p:Pipeline => s"${quote(ctr)}_max"
					case _ => quote(end)
				}
				case None => quote(end)
			}
      val pre = if (par(ctr) == 1) "DFEVar" else "DFEVector<DFEVar>"
      if (par(ctr) == 1) {
        emit(s"""$pre ${quote(ctr)} = ${quote(cchain)}_chain.addCounter(${quote(max)}, ${quote(step)});""")
      } else {
        emit(s"""$pre ${quote(ctr)} = ${quote(cchain)}_chain.addCounterVect(${quote(par)}, ${quote(max)}, ${quote(step)});""")
      }
    }

    val doneStr = if (!done.isDefined) {
        s"stream.offset(${quote(cchain)}_chain.getCounterWrap(${quote(counters(0))}),-1)"
    } else {
      done.get
    }

    if (!doneDeclaredSet.contains(sym)) {
      emit(s"""DFEVar ${quote(sym)}_done = $doneStr;""")
      doneDeclaredSet += sym
    } else {
      emit(s"""${quote(sym)}_done <== $doneStr;""")
  	}

  }


}

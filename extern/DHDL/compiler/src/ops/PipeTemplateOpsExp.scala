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

  case class Counterchain_new(counters: List[Rep[Counter]])(implicit ctx: SourceContext) extends Def[CounterChain]

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

    // Reified load function
    val acc = reflectMutableSym( fresh[C[T]] )  // Has to be mutable since we write to "it"
    val ldBlk = reifyEffects(__mem.ld(acc, inds))

    // Reified store function
    val res = fresh[T]
    val stBlk = reifyEffects(__mem.st(acc, inds, res))

    // Reified map function
    val mBlk = reifyEffects( func(inds) )

    // Reified reduction function
    val rV = (fresh[T], fresh[T])
    val rBlk = reifyEffects( rFunc(rV._1, rV._2) )

    val effects = summarizeEffects(mBlk) andAlso summarizeEffects(rBlk) andAlso
                  summarizeEffects(stBlk) andAlso summarizeEffects(ldBlk) andAlso Write(List(accum.asInstanceOf[Sym[C[T]]]))

    reflectEffect(Pipe_reduce(cchain, accum, ldBlk, stBlk, mBlk, rBlk, is, acc, res, rV), effects.star)
  }

  def counterchain_new(counters: List[Rep[Counter]])(implicit ctx: SourceContext): Rep[CounterChain] = {
    // HACK: Not actually mutable, but isn't being scheduled properly otherwise
    reflectMutable(Counterchain_new(counters)(ctx))
  }

  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@Pipe_foreach(c,func,inds) => reflectPure(Pipe_foreach(f(c),f(func),inds)(pos))(mtype(manifest[A]),pos)
    case Reflect(e@Pipe_foreach(c,func,inds), u, es) => reflectMirrored(Reflect(Pipe_foreach(f(c),f(func),inds)(pos), mapOver(f,u), f(es)))(mtype(manifest[A]),pos)

    case e@Pipe_reduce(c,a,ld,st,func,rFunc,inds,acc,res,rV) => reflectPure(Pipe_reduce(f(c),f(a),f(ld),f(st),f(func),f(rFunc),inds,acc,res,rV)(pos, e.memC, e.mT, e.mC))(mtype(manifest[A]), pos)
    case Reflect(e@Pipe_reduce(c,a,ld,st,func,rFunc,inds,acc,res,rV), u, es) => reflectMirrored(Reflect(Pipe_reduce(f(c),f(a),f(ld),f(st),f(func),f(rFunc),inds,acc,res,rV)(pos, e.memC, e.mT, e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@Counterchain_new(counters) => reflectPure(Counterchain_new(counters)(pos))(mtype(manifest[A]), pos)
    case Reflect(e@Counterchain_new(counters), u, es) => reflectMirrored(Reflect(Counterchain_new(counters)(pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(e, f)
  }

  // --- Dependencies
  override def syms(e: Any): List[Sym[Any]] = e match {
    case Pipe_foreach(chain, func, _) => syms(chain) ::: syms(func)
    case e: Pipe_reduce[_,_] => syms(e.cchain) ::: syms(e.accum) ::: syms(e.func) ::: syms(e.rFunc) ::: syms(e.ldFunc) ::: syms(e.stFunc)
    case _ => super.syms(e)
  }
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case Pipe_foreach(chain, func, _) => readSyms(chain) ::: readSyms(func)
    case e: Pipe_reduce[_,_] => readSyms(e.cchain) ::: readSyms(e.accum) ::: readSyms(e.func) ::: readSyms(e.rFunc) ::: readSyms(e.ldFunc) ::: readSyms(e.stFunc)
    case _ => super.readSyms(e)
  }
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case Pipe_foreach(chain, func, _) => freqCold(func) ::: freqCold(chain)
    case e: Pipe_reduce[_,_] => freqCold(e.func) ::: freqCold(e.rFunc) ::: freqCold(e.ldFunc) ::: freqCold(e.stFunc) ::: freqCold(e.cchain) ::: freqCold(e.accum)
    case _ => super.symsFreq(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Pipe_foreach(chain,func,inds) => inds ::: effectSyms(func) ::: effectSyms(chain)
    case e: Pipe_reduce[_,_] => e.inds ::: List(e.rV._1, e.rV._2, e.acc, e.res) ::: effectSyms(e.cchain) ::: effectSyms(e.func) ::: effectSyms(e.rFunc) ::: effectSyms(e.ldFunc) ::: effectSyms(e.stFunc) ::: effectSyms(e.accum)
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
    case e@Counterchain_new(counters) =>
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

    case _ => super.emitNode(sym, rhs)
  }

  /*override def quote(x: Exp[Any]) = x match {
		case s@Sym(n) => s.tp.erasure.getSimpleName() + "_x" + n
    case _ => super.quote(x)
  }*/
}

trait DotGenPipeTemplateOps extends DotGenEffect{
  val IR: PipeTemplateOpsExp with FixPtOpsExp with FltPtOpsExp with TpesOpsExp with NosynthOpsExp
	        with OffChipMemOpsExp with RegOpsExp with DHDLCodegenOps

					/*
  import IR.{Sym, Exp, Def}
  import IR.{ConstFix, ConstFlt, ConstBit, EatReflect}
  import IR.{Counterchain_new, Offchip_new, Reg_Reg_new, Set_arg, Set_mem, Pipe_foreach, Pipe_reduce}
  import IR.{CounterChain, FixPt, Signed, B32, B0}
	*/
 	import IR.{__ifThenElse => _, Nosynth___ifThenElse => _, __whileDo => _,
 					Forloop => _, println => _ , _}

	
  override def emitFileHeader() {
		super.emitFileHeader()
    emit(s"compound=true")
    emit(s"""graph [splines=\"ortho\" clusterrank=\"local\"]""")
    emit(s"edge [arrowsize=$arrowSize penwidth=$edgeThickness]")
    emit(s"""node [fontsize=$fontsize shape=$defaultShape style=\"filled\" fillcolor=\"$bgcolor\"]""")
    emit(s"fontsize=$fontsize")
	}

	def emitNestedIdx(cchain:Exp[CounterChain], inds:List[Sym[FixPt[Signed,B32,B0]]]) = cchain match {
    case Def(EatReflect(Counterchain_new(counters))) =>
	     inds.zipWithIndex.foreach {case (iter, idx) => emitAlias(iter, counters(idx)) }
	}

	var emittedCtrChain = Set.empty[Sym[Any]]

  override def initializeGenerator(buildDir:String): Unit = {
		emittedCtrChain = Set.empty[Sym[Any]]
		super.initializeGenerator(buildDir)
	}

	def emitCtrChain(cchain: Exp[CounterChain]):Unit = {
		val Def(EatReflect(d)) = cchain
		emitCtrChain(cchain.asInstanceOf[Sym[CounterChain]],
									 d.asInstanceOf[Def[Any]])
	}

	def emitCtrChain(sym: Sym[Any], rhs: Def[Any]):Unit = rhs match {
	  case e@Counterchain_new(counters) =>
			if (!emittedCtrChain.contains(sym)) {
				emittedCtrChain = emittedCtrChain + sym
    		emit(s"""subgraph cluster_${quote(sym)} {""")
    		emit(s""" label=${quote(sym)} """)
    		emit(s""" style="rounded, filled" """)
    		emit(s""" fillcolor="${counterColor}" """)
				//emit(s""" ${quote(sym)} [label="" style="invisible" height=0 size=0 margin=0 ]""")
    		counters.foreach{ ctr =>
    		  emit(s"""   ${quote(ctr)}""")
    		}
    		emit("}")
			}
		case _ => 
	}

  def emitBlock(y: Block[Any], name:String, color:String): Unit = { 
      emit(s"""subgraph cluster_${name} {""")
      emit(s"""label="${name}" """)
      emit(s"""color="gray" """)
      emit(s"""style="filled" """)
			emit(s"""fillcolor="${color}"""")
			emitBlock(y)
			emit(s"""}""")
	}

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
	  case e@Counterchain_new(counters) =>
			parentOf(sym) match {
				//TODO: check whether parent of cchain is empty, if is emit ctrchain
				//emitCtrChain(sym, rhs)
				//case None => emitCtrChain(sym, rhs)
				case _ =>
			}

    case e@Pipe_foreach(cchain, func, inds) =>
      emitNestedIdx(cchain, inds)
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""label="${quote(sym)}"""")
      emit(s"""color="gray"""")
      emit(s"""style="filled" """)
			emit(s"""fillcolor="${pipeFillColor}"""")
			emitCtrChain(cchain)
      emitBlock(func, "foreachFunc", mapFuncColor)             // Map function
      emit("}")

    case e@Pipe_reduce(cchain, accum, ldFunc, stFunc, func, rFunc, inds, acc, res, rV) =>
      emitAlias(acc, accum)
      emitNestedIdx(cchain, inds)
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""label="${quote(sym)}"""")
      emit(s"""color="gray"""")
      emit(s"""style="filled" """)
			emit(s"""fillcolor="${pipeFillColor}"""")
      emit(s"""define(`${quote(acc)}', `${quote(accum)}')""")
			val Def(EatReflect(d)) = cchain
			emitCtrChain(cchain)
      emitBlock(func, "mapFunc", mapFuncColor)             // Map function
      emitBlock(ldFunc, "ldFunc", ldFuncColor)             // Map function
      emitAlias(rV._1, getBlockResult(ldFunc))
      emitAlias(rV._2, getBlockResult(func))
      emitBlock(rFunc, "reduceFunc", reduceFuncColor)             // Map function
      emitAlias(res, getBlockResult(rFunc))
      emitBlock(stFunc, "stFunc", stFuncColor)             // Map function
      emit("}")

    case _ => super.emitNode(sym,rhs)
	}

  override def quote(x: Exp[Any]) = x match {
		case s@Sym(n) => s.tp.erasure.getSimpleName() + 
										(if (nameOf(s)!="") "_" else "") + nameOf(s) + "_x" + n
    case _ => super.quote(x)
	}
}

trait MaxJGenPipeTemplateOps extends MaxJGenEffect {
  val IR: PipeTemplateOpsExp with FixPtOpsExp with FltPtOpsExp with TpesOpsExp with NosynthOpsExp
	        with OffChipMemOpsExp with RegOpsExp with CounterOpsExp with DHDLCodegenOps  

					/*
  import IR.{Sym, Exp, Def, Rep}
  import IR.{ConstFix, ConstFlt, ConstBit, EatReflect}
  import IR.{Counterchain_new, Offchip_new, Reg_Reg_new, Set_arg, Set_mem, Pipe_foreach, Pipe_reduce}
  import IR.{CounterChain, FixPt, Signed, B32, B0}
	import IR.{Coarse, Fine, Disabled, styleOf}
	import IR.{Pipeline, Overload57}
	*/
 import IR.{__ifThenElse => _, Nosynth___ifThenElse => _, __whileDo => _,
 					Forloop => _, println => _ , _}

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Counterchain_new(counters) =>

    case e@Pipe_foreach(cchain, func, inds) =>
			styleOf(sym.asInstanceOf[Rep[Pipeline]]) match {
				case Coarse =>
				case Fine =>
					//TODO: assume bram not write to accum
    			//val writesToAccumRam = mapNode.nodes.filter { _.isInstanceOf[St] }.exists { _.asInstanceOf[St].mem.isAccum }
					val writesToAccumRam = false
					emitPipeProlog(sym, cchain, inds, writesToAccumRam)
					emitPipeForEachEpilog(sym, writesToAccumRam, cchain, inds(0))
				case Disabled =>
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
			}
      emitBlock(func)
      emitBlock(ldFunc)
      emitBlock(rFunc)
      emitBlock(stFunc)

    case _ => super.emitNode(sym, rhs)
  }

  override def quote(x: Exp[Any]) = x match {
		case s@Sym(n) => s.tp.erasure.getSimpleName() + 
										(if (nameOf(s)!="") "_" else "") + nameOf(s) + "_x" + n
    case _ => super.quote(x)
  }

	
	def emitPipeProlog(sym: Sym[Any], cchain: Exp[CounterChain], inds:List[Sym[FixPt[Signed,B32,B0]]],
		writesToAccumRam:Boolean) = {
		val Def(EatReflect(Counterchain_new(counters))) = cchain
		//TODO: assume pipe is top level
    //if (!n.hasParent()) {
      emit(s"""DFEVar ${quote(sym)}_en = top_en;""")
      emit(s"""DFEVar ${quote(sym)}_done = dfeBool().newInstance(this);""")
      emit(s"""top_done <== ${quote(sym)}_done;""")
      //enDeclaredSet += n
      //doneDeclaredSet += n
    //}

    emit(s"""SMIO ${quote(sym)}_sm = addStateMachine("${quote(sym)}_sm", new PipeSM(this, ${counters.size}));""")
    emit(s"""${quote(sym)}_sm.connectInput("sm_en", ${quote(sym)}_en);""")
    emit(s"""${quote(sym)}_done <== stream.offset(${quote(sym)}_sm.getOutput("sm_done"),-1);""")

		counters.zipWithIndex.map {case (ctr,i) =>
			val Def(EatReflect(Counter_new(start, end, step))) = ctr
      emit(s"""${quote(sym)}_sm.connectInput("sm_maxIn_$i", ${quote(end)});""")
      emit(s"""DFEVar ${quote(ctr)}_max = ${quote(sym)}_sm.getOutput("ctr_maxOut");""")
    }

    emit(s"""DFEVar ${quote(cchain)}_done = dfeBool().newInstance(this);""")
    emit(s"""${quote(sym)}_sm.connectInput("ctr_done", ${quote(cchain)}_done);""")
    emit(s"""DFEVar ${quote(cchain)}_en_from_pipesm = ${quote(sym)}_sm.getOutput("ctr_en");""")
    //doneDeclaredSet += ctr

    emit(s"""DFEVar ${quote(sym)}_rst_done = dfeBool().newInstance(this);""")
    emit(s"""${quote(sym)}_sm.connectInput("rst_done", ${quote(sym)}_rst_done);""")
    emit(s"""DFEVar ${quote(sym)}_rst_en = ${quote(sym)}_sm.getOutput("rst_en");""")

  	//def isUnitCtr = size != 0 && (max.head match {case Const(1) => (size == 1); case _ => false })

		val isUnitCtr = if (1 == counters.size) {
			val Def(EatReflect(Counter_new(start, end, step))) = counters(0)
			end match {
				case Def(ConstFixPt(1, _, _, _)) => true
				case _ => false
			}
		} else false

    if (isUnitCtr && !writesToAccumRam) {
      emit(s"""${quote(sym)}_rst_done <== constant.var(true);""")
    } else {
      emit(s"""OffsetExpr ${quote(sym)}_offset = stream.makeOffsetAutoLoop("${quote(sym)}_offset");""")
      emit(s"""${quote(sym)}_rst_done <== stream.offset(${quote(sym)}_rst_en, -${quote(sym)}_offset-1);""")
    }

	}

	def emitMaxJCounterChain(cchain: Exp[CounterChain], en: Option[String], done: Option[String]=None) = {
    // For Pipes, max must be derived from PipeSM
    // For everyone else, max is as mentioned in the ctr
    //val maxStrs = n.max.zipWithIndex map { t =>
    //  val m = t._1
    //  val i = t._2
    //  n.getParent() match {
    //    case p: Pipe => s"${quote(n)}_max_$i"
    //    case _ => quote(m)
    //  }
    //}
		val Def(EatReflect(Counterchain_new(counters))) = cchain
		counters.zipWithIndex.map {case (ctr,i) =>
			val Def(EatReflect(Counter_new(start, end, step))) = ctr
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
        emit(s"""$pre ${quote(ctr)} = ${quote(cchain)}_chain.addCounterVect(${par(ctr)}, ${quote(max)}, ${quote(step)});""")
      }
    }

    val doneStr = if (!done.isDefined) {
        s"stream.offset(${quote(cchain)}_chain.getCounterWrap(${quote(counters(0))}),-1)"
    } else {
      done.get
    }

		//TODO
    //if (!doneDeclaredSet.contains(n)) {
    //  emit(s"""dfevar ${quote(n)}_done = $doneStr;""")
    //  doneDeclaredSet += n
    //} else {
    //  emit(s"""${quote(n)}_done <== $doneStr;""")
    //}
    emit(s"""dfevar ${quote(cchain)}_done = $doneStr;""")
		/*
		*/
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

}

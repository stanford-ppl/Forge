package dhdl.compiler.ops

import scala.virtualization.lms.common.{EffectExp, ScalaGenEffect, DotGenEffect, MaxJGenEffect}
import scala.virtualization.lms.internal.{Traversal}
import scala.reflect.{Manifest,SourceContext}
import scala.collection.mutable.Set
import java.io.{File, FileWriter, PrintWriter}
import ppl.delite.framework.transform.{DeliteTransform}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait ControllerTemplateOpsExp extends ControllerTemplateOps with MemoryTemplateOpsExp with CounterExternOpsExp with EffectExp {
  this: DHDLExp =>

  type Idx = FixPt[Signed,B32,B0]

  // --- Nodes
  case class Pipe_foreach(cchain: Exp[CounterChain], func: Block[Unit], inds: List[Sym[Idx]])(implicit ctx: SourceContext) extends Def[Pipeline]
  case class Pipe_fold[T,C[T]] (
    // - Inputs
    cchain: Exp[CounterChain],  // Loop counter chain
    accum:  Exp[C[T]],          // Reduction accumulator
    foldAccum: Boolean,         // Act as a fold (true) or a reduce (false)
    // - Reified blocks
    iFunc:  Block[Idx],         // Calculation of 1D index for load and store
    ldFunc: Block[T],           // Accumulator load function (reified with acc, idx)
    stFunc: Block[Unit],        // Accumulator store function (reified with acc, idx, res)
    func:   Block[T],           // Map function
    rFunc:  Block[T],           // Reduction function
    // - Bound args
    inds:   List[Sym[Idx]],     // Loop iterators
    idx:    Sym[Idx],           // Index for addressing in ldFunc and stFunc (aliases with iFunc.res)
    acc:    Sym[C[T]],          // Reduction accumulator (aliases with accum)
    res:    Sym[T],             // Reduction intermediate result (aliases with rFunc.res)
    rV:    (Sym[T], Sym[T])     // Reduction function inputs
  )(implicit val ctx: SourceContext, val memC: Mem[T,C], val mT: Manifest[T], val mC: Manifest[C[T]]) extends Def[Pipeline]

  case class Accum_fold[T,C[T]](
    // - Inputs
    ccOuter: Exp[CounterChain], // Counter chain for map (outer) loop
    ccInner: Exp[CounterChain], // Counter chain for reduce (inner) loop
    accum:  Exp[C[T]],          // Reduction accumulator
    foldAccum: Boolean,         // Act as a fold (true) or a reduce (false)
    // - Reified blocks
    iFunc:  Block[Idx],         // Calculation of 1D index for loads and stores
    func: Block[C[T]],          // Map function
    resLdFunc: Block[T],        // Partial result load function
    ldFunc: Block[T],           // Accumulator load function
    rFunc: Block[T],            // Reduction function
    stFunc: Block[Unit],        // Accumulator store function
    // - Bound args
    indsOuter: List[Sym[Idx]],  // Map (outer) loop iterators
    indsInner: List[Sym[Idx]],  // Reduce (inner) loop iterators
    idx: Sym[Idx],              // Index used in addressing in ldFunc and stFunc
    part: Sym[C[T]],            // Partial result (aliases with func.res)
    acc: Sym[C[T]],             // Reduction accumulator (aliases with accum)
    res: Sym[T],                // Reduction intermediate result (aliases with rFunc.res)
    rV:  (Sym[T], Sym[T])       // Reduction function inputs
  )(implicit val ctx: SourceContext, val memC: Mem[T,C], val mT: Manifest[T], val mC: Manifest[C[T]]) extends Def[Pipeline]

  // --- Internals
  def pipe_foreach(cchain: Rep[CounterChain], func: Rep[Indices] => Rep[Unit])(implicit ctx: SourceContext): Rep[Pipeline] = {
    val inds = List.fill(lenOf(cchain)){ fresh[Idx] } // Arbitrary number of bound args. Awww yeah.
    val blk = reifyEffects( func(indices_create(inds)) )
    reflectEffect(Pipe_foreach(cchain, blk, inds), summarizeEffects(blk).star andAlso Simple())
  }
  def pipe_fold[T,C[T]](cchain: Rep[CounterChain], accum: Rep[C[T]], func: Rep[Indices] => Rep[T], rFunc: (Rep[T],Rep[T]) => Rep[T], foldAccum: Boolean = true)(implicit ctx: SourceContext, __mem: Mem[T,C], __mT: Manifest[T], __mC: Manifest[C[T]]): Rep[Pipeline]  = {
    // Loop indices
    val is = List.fill(lenOf(cchain)){ fresh[Idx] }
    val inds = indices_create(is)

    val idx = fresh[Idx]
    val iBlk = reifyEffects( __mem.flatIdx(accum, inds) )

    // Reified map function
    val mBlk = reifyEffects( func(inds) )

    // Reified load function
    val acc = reflectMutableSym( fresh[C[T]] )  // Has to be mutable since we write to "it"
    setProps(acc, getProps(accum))

    val ldBlk = reifyEffects(__mem.ld(acc, idx))

   // Reified reduction function
    val rV = (fresh[T], fresh[T])
    val rBlk = reifyEffects( rFunc(rV._1, rV._2) )

    // Reified store function
    val res = fresh[T]
    val stBlk = reifyEffects(__mem.st(acc, idx, res))

    val effects = summarizeEffects(iBlk) andAlso summarizeEffects(mBlk) andAlso summarizeEffects(ldBlk) andAlso
                  summarizeEffects(rBlk) andAlso summarizeEffects(stBlk) andAlso Write(List(accum.asInstanceOf[Sym[C[T]]]))

    reflectEffect(Pipe_fold[T,C](cchain, accum, foldAccum, iBlk, ldBlk, stBlk, mBlk, rBlk, is, idx, acc, res, rV), effects.star)
  }

  private def accum_common[T,C[T]](cchain: Rep[CounterChain], cchainRed: Rep[CounterChain], accum: Rep[C[T]], func: Block[C[T]], isMap: List[Sym[Idx]], rFunc: (Rep[T],Rep[T]) => Rep[T], foldAccum: Boolean = true)(implicit ctx: SourceContext, __mem: Mem[T,C], __mT: Manifest[T], __mC: Manifest[C[T]]): Rep[Pipeline] = {
    val isRed = List.fill(lenOf(cchainRed)){ fresh[Idx] } // Reduce loop indices
    val indsRed = indices_create(isRed)

    val idx = fresh[Idx]
    val iBlk = reifyEffects( __mem.flatIdx(getBlockResult(func), indsRed) )

    val part = fresh[C[T]]
    setProps(part, getProps(func))
    // Partial result load
    val ldPartBlk = reifyEffects( __mem.ld(part, idx) )

    val acc = reflectMutableSym( fresh[C[T]] )
    setProps(acc, getProps(accum))
    // Accumulator load
    val ldBlk = reifyEffects( __mem.ld(acc, idx) )

    val rV = (fresh[T],fresh[T])
    // Reified reduction function
    val rBlk = reifyEffects( rFunc(rV._1, rV._2) )

    val res = fresh[T]
    // Accumulator store function
    val stBlk = reifyEffects( __mem.st(acc, idx, res) )

    val effects = summarizeEffects(iBlk) andAlso summarizeEffects(func) andAlso summarizeEffects(ldPartBlk) andAlso
                  summarizeEffects(ldBlk) andAlso summarizeEffects(rBlk) andAlso summarizeEffects(stBlk) andAlso Write(List(accum.asInstanceOf[Sym[C[T]]]))

    reflectEffect(Accum_fold[T,C](cchain, cchainRed, accum, foldAccum, iBlk, func, ldPartBlk, ldBlk, rBlk, stBlk, isMap, isRed, idx, part, acc, res, rV), effects.star)
  }


  def accum_fold[T,C[T]](cchain: Rep[CounterChain], cchainRed: Rep[CounterChain], accum: Rep[C[T]], func: Rep[Indices] => Rep[C[T]], rFunc: (Rep[T],Rep[T]) => Rep[T], foldAccum: Boolean = true)(implicit ctx: SourceContext, __mem: Mem[T,C], __mT: Manifest[T], __mC: Manifest[C[T]]): Rep[Pipeline] = {
    val isMap = List.fill(lenOf(cchain)){ fresh[Idx] }   // Map loop indices
    val indsMap = indices_create(isMap)
    val mBlk = reifyEffects( func(indsMap) )             // Reified map function

    accum_common(cchain, cchainRed, accum, mBlk, isMap, rFunc, foldAccum)
  }

  /*def accum_reduce[T,C[T]](cchain: Rep[CounterChain], cchainRed: Rep[CounterChain], func: Rep[Indices] => Rep[C[T]], rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext, __mem: Mem[T,C], __mT: Manifest[T], __mC: Manifest[C[T]]): (Rep[C[T]], Rep[Pipeline]) = {
    val isMap = List.fill(lenOf(cchain)){ fresh[Idx] }   // Map loop indices
    val indsMap = indices_create(isMap)
    val mBlk = reifyEffects( func(indsMap) )             // Reified map function
    val accum = __mem.empty(getBlockResult(mBlk))
    val pipe = accum_common(cchain, cchainRed, accum, mBlk, isMap, rFunc, false)
    (accum, pipe)
  }*/

  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@Pipe_foreach(c,func,inds) => reflectPure(Pipe_foreach(f(c),f(func),inds)(pos))(mtype(manifest[A]),pos)
    case Reflect(e@Pipe_foreach(c,func,inds), u, es) => reflectMirrored(Reflect(Pipe_foreach(f(c),f(func),inds)(pos), mapOver(f,u), f(es)))(mtype(manifest[A]),pos)

    case e@Pipe_fold(c,a,fA,ld,st,iFunc,func,rFunc,inds,idx,acc,res,rV) => reflectPure(Pipe_fold(f(c),f(a),fA,f(ld),f(st),f(iFunc),f(func),f(rFunc),inds,idx,acc,res,rV)(e.ctx, e.memC, e.mT, e.mC))(mtype(manifest[A]), pos)
    case Reflect(e@Pipe_fold(c,a,fA,ld,st,iFunc,func,rFunc,inds,idx,acc,res,rV), u, es) => reflectMirrored(Reflect(Pipe_fold(f(c),f(a),fA,f(ld),f(st),f(iFunc),f(func),f(rFunc),inds,idx,acc,res,rV)(e.ctx, e.memC, e.mT, e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@Accum_fold(c1,c2,a,fA,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV) => reflectPure(Accum_fold(f(c1),f(c2),f(a),fA,f(iFunc),f(func),f(ld1),f(ld2),f(rFunc),f(st),inds1,inds2,idx,part,acc,res,rV)(e.ctx,e.memC,e.mT,e.mC))(mtype(manifest[A]), pos)
    case Reflect(e@Accum_fold(c1,c2,a,fA,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV), u, es) => reflectMirrored(Reflect(Accum_fold(f(c1),f(c2),f(a),fA,f(iFunc),f(func),f(ld1),f(ld2),f(rFunc),f(st),inds1,inds2,idx,part,acc,res,rV)(e.ctx,e.memC,e.mT,e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(e, f)
  }

  override def propagate(lhs: Exp[Any], rhs: Def[Any]) = rhs match {
    case Pipe_fold(c,a,fA,ld,st,iFunc,func,rFunc,inds,idx,acc,res,rV) =>
      setProps(acc, getProps(a))
      setProps(res, getProps(rFunc))
      setProps(rV._1, getProps(func))
      setProps(rV._2, getProps(func))
      setProps(idx, getProps(iFunc))
    case Accum_fold(c1,c2,a,fA,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV) =>
      setProps(acc, getProps(a))
      setProps(part, getProps(func))
      setProps(res, getProps(rFunc))
      setProps(rV._1, getProps(ld1))
      setProps(rV._2, getProps(ld2))
      setProps(idx, getProps(iFunc))

    case _ => super.propagate(lhs, rhs)
  }

  // --- Dependencies
  override def syms(e: Any): List[Sym[Any]] = e match {
    case Pipe_foreach(chain, func, _) => syms(chain) ::: syms(func)
    case e: Pipe_fold[_,_] => syms(e.cchain) ::: syms(e.accum) ::: syms(e.iFunc) ::: syms(e.func) ::: syms(e.rFunc) ::: syms(e.ldFunc) ::: syms(e.stFunc)
    case e: Accum_fold[_,_] => syms(e.ccOuter) ::: syms(e.ccInner) ::: syms(e.accum) ::: syms(e.iFunc) ::: syms(e.func) ::: syms(e.resLdFunc) ::: syms(e.ldFunc) ::: syms(e.rFunc) ::: syms(e.stFunc)
    case _ => super.syms(e)
  }
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case Pipe_foreach(chain, func, _) => readSyms(chain) ::: readSyms(func)
    case e: Pipe_fold[_,_] => readSyms(e.cchain) ::: readSyms(e.accum) ::: readSyms(e.iFunc) ::: readSyms(e.func) ::: readSyms(e.rFunc) ::: readSyms(e.ldFunc) ::: readSyms(e.stFunc)
    case e: Accum_fold[_,_] => readSyms(e.ccOuter) ::: readSyms(e.ccInner) ::: readSyms(e.accum) ::: readSyms(e.iFunc) ::: readSyms(e.func) ::: readSyms(e.resLdFunc) ::: readSyms(e.ldFunc) ::: readSyms(e.rFunc) ::: readSyms(e.stFunc)
    case _ => super.readSyms(e)
  }
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case Pipe_foreach(chain, func, _) => freqCold(func) ::: freqCold(chain)
    case e: Pipe_fold[_,_] => freqNormal(e.iFunc) ::: freqCold(e.func) ::: freqCold(e.rFunc) ::: freqCold(e.ldFunc) ::: freqCold(e.stFunc) ::: freqCold(e.cchain) ::: freqCold(e.accum)
    case e: Accum_fold[_,_] => freqNormal(e.ccOuter) ::: freqNormal(e.ccInner) ::: freqNormal(e.accum) ::: freqNormal(e.iFunc) ::: freqNormal(e.func) ::: freqNormal(e.resLdFunc) ::: freqNormal(e.ldFunc) ::: freqNormal(e.rFunc) ::: freqNormal(e.stFunc)
    case _ => super.symsFreq(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Pipe_foreach(chain,func,inds) => inds ::: effectSyms(func) ::: effectSyms(chain)
    case e: Pipe_fold[_,_] => e.inds ::: List(e.rV._1, e.rV._2, e.acc, e.res, e.idx) ::: effectSyms(e.cchain) ::: effectSyms(e.iFunc) ::: effectSyms(e.func) ::: effectSyms(e.rFunc) ::: effectSyms(e.ldFunc) ::: effectSyms(e.stFunc) ::: effectSyms(e.accum)
    case e: Accum_fold[_,_] => e.indsOuter ::: e.indsInner ::: List(e.idx, e.rV._1, e.rV._2, e.acc, e.res, e.part) ::: effectSyms(e.ccOuter) ::: effectSyms(e.ccInner) ::: effectSyms(e.accum) ::: effectSyms(e.iFunc) ::: effectSyms(e.func) ::: effectSyms(e.resLdFunc) ::: effectSyms(e.ldFunc) ::: effectSyms(e.rFunc) ::: effectSyms(e.stFunc)
    case _ => super.boundSyms(e)
  }
}

trait ScalaGenControllerTemplateOps extends ScalaGenEffect {
  val IR: ControllerTemplateOpsExp with DHDLIdentifiers
  import IR._

  def emitNestedLoop(iters: List[Sym[Idx]], cchain: Exp[CounterChain])(emitBlk: => Unit) = {
    iters.zipWithIndex.foreach{ case (iter,idx) =>
      stream.println("for( " + quote(iter) + " <- " + quote(cchain) + ".apply(" + idx + ".toInt)) {")
    }
    emitBlk
    stream.println("}" * iters.length)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Pipe_foreach(cchain, func, inds) =>
      emitNestedLoop(inds, cchain){ emitBlock(func) }
      emitValDef(sym, "()")

    case e@Pipe_fold(cchain, accum, fA, iFunc, ldFunc, stFunc, func, rFunc, inds, idx, acc, res, rV) =>
      emitValDef(acc, quote(accum)) // Assign bound accumulator to accum
      emitNestedLoop(inds, cchain){
        emitBlock(iFunc)            // Idx function
        emitValDef(idx, quote(getBlockResult(iFunc)))
        emitBlock(func)             // Map function
        emitBlock(ldFunc)           // Load corresponding value from accumulator
        emitValDef(rV._1, quote(getBlockResult(ldFunc)))
        emitValDef(rV._2, quote(getBlockResult(func)))
        emitBlock(rFunc)            // Reduction function
        emitValDef(res, quote(getBlockResult(rFunc)))
        emitBlock(stFunc)           // Write back to accumulator
      }
      emitValDef(sym, "()")

    case e@Accum_fold(ccOuter, ccInner, accum, fA, iFunc, func, ldPart, ldFunc, rFunc, stFunc, indsOuter, indsInner, idx, part, acc, res, rV) =>
      emitValDef(acc, quote(accum)) // Assign bound accumulator to accum
      emitNestedLoop(indsOuter, ccOuter){
        emitBlock(func)
        emitValDef(part, quote(getBlockResult(func)))

        emitNestedLoop(indsInner, ccInner){
          emitBlock(iFunc)
          emitValDef(idx, quote(getBlockResult(iFunc)))
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

trait DotGenControllerTemplateOps extends DotGenCounterExternOps {
  val IR: ControllerTemplateOpsExp with TpesOpsExp with CounterExternOpsExp
          with OffChipMemOpsExp with RegOpsExp with DeliteTransform with DHDLCodegenOps

  import IR._

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

    case e@Pipe_fold(cchain, accum, foldAccum, iFunc, ldFunc, stFunc, func, rFunc, inds, idx, acc, res, rV) =>
			var label = quote(sym)
			styleOf(sym.asInstanceOf[Rep[Pipeline]]) match {
				case Coarse => label = label.replace("DHDLPipeline", "MetaPipe")
				case Fine => label = label.replace("DHDLPipeline", "Pipe")
				case Disabled => label = label.replace("DHDLPipeline", "Sequential")
			}
      emitValDef(acc, accum)
      emitNestedIdx(cchain, inds)
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""label="${quote(sym)}"""")
      emit(s"""color=$pipeBorderColor""")
      emit(s"""style="bold, filled" """)
			emit(s"""fillcolor=$pipeFillColor""")
      emit(s"""define(`${quote(acc)}', `${quote(accum)}')""")
			//val Def(EatReflect(d)) = cchain
			emitCtrChain(cchain)
      emitBlock(iFunc, quote(sym) + "_idxFunc", "idxFunc", ldFillColor)
			emitValDef(idx, quote(getBlockResult(iFunc)))
      emitBlock(func, quote(sym) + "_mapFunc", "mapFunc", mapFillColor)
      emitBlock(ldFunc, quote(sym) + "_ldFunc", "ldFunc", ldFillColor)
      emitValDef(rV._1, getBlockResult(ldFunc))
      emitValDef(rV._2, getBlockResult(func))
      emitBlock(rFunc, quote(sym) + "_reduceFunc", "reduceFunc", reduceFillColor)
      emitValDef(res, getBlockResult(rFunc))
      emitBlock(stFunc, quote(sym) + "_stFunc", "stFunc" , stFillColor)
      emit("}")

    case e@Accum_fold(ccOuter, ccInner, accum, foldAccum, iFunc, func, ldPart, ldFunc, rFunc, stFunc, indsOuter, indsInner, idx, part, acc, res, rV) =>
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
					s.tp match {
						case ss:Register[_] =>
							tstr = tstr.replace("Register", regType(s) match {
								case Regular => "Reg"
								case ArgumentIn => "ArgIn"
								case ArgumentOut => "ArgOut"
							})
						case ss:Pipeline =>
							tstr = tstr.replace("Pipeline", styleOf(s) match {
								case Fine => "Pipe"
								case Coarse => "MetaPipe"
								case Disabled => "Sequential"
							})
						case _ => //println(s.tp)
					}
					tstr = tstr.replace("BlockRAM", "BRAM")
					val quoteStr = tstr + nameOf(s).map(nm=>"_"+nm).getOrElse("") + "_x" + n
					/*
					if (quoteStr.contains("108")) {
						println("sym:" + quoteStr)
						s match {
							case Def(d) => println("def:" + d)
							case _ => println("don't know what this is")
						}
					}
					*/
					quoteStr
				}
			}
    case s => super.quote(x)
  }
}

trait MaxJGenControllerTemplateOps extends MaxJGenEffect {
  val IR: ControllerTemplateOpsExp with TpesOpsExp with ParallelOpsExp
          with OffChipMemOpsExp with RegOpsExp with CounterExternOpsExp
          with DHDLCodegenOps with DeliteTransform

 import IR._ //{__ifThenElse => _, Nosynth___ifThenElse => _, __whileDo => _,
             // Forloop => _, println => _ , _}

  override def quote(x: Exp[Any]) = x match {
		case s@Sym(n) => {
			var tstr = s.tp.erasure.getSimpleName()
			tstr = tstr.replace("DHDL","")
			tstr = tstr.replace("Register", regType(s) match {
				case Regular => "Reg"
				case ArgumentIn => "ArgIn"
				case ArgumentOut => "ArgOut"
			})
			tstr = tstr.replace("BlockRAM", "BRAM")
			tstr + nameOf(s).map(nm=>"_"+nm).getOrElse("") + "_x" + n
		}
    case _ => super.quote(x)
  }

  /* Set of control nodes which already have their enable signal emitted */
  val enDeclaredSet = Set.empty[Exp[Any]]

  /* Set of control nodes which already have their done signal emitted */
  val doneDeclaredSet = Set.empty[Exp[Any]]

	var traversals: List[Traversal{val IR: MaxJGenControllerTemplateOps.this.IR.type}] = Nil

  def runTraversals[A:Manifest](b: Block[A]): Block[A] = {
    println("MaxJCodegen: applying transformations")
    var curBlock = b
    println("Traversals:\n\t" + traversals.map(_.name).mkString("\n\t"))

    for (t <- traversals) {
      printlog("  Block before transformation: " + curBlock)
      curBlock = t.run(curBlock)
      printlog("  Block after transformation: " + curBlock)
    }
    println("MaxJGodegen: done transforming")
    (curBlock)
  }

  override def initializeGenerator(buildDir:String): Unit = {
		enDeclaredSet.clear
		doneDeclaredSet.clear
		traversals = IR.traversals
		super.initializeGenerator(buildDir)
	}

  override def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {
    val y = runTraversals(body)
		super.emitSource(args, y, className, out)
	}

  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    emit(s"""${maxJPre(sym)} ${quote(sym)} = ${quote(rhs)};""")
  }

  def emitValDef(sym: Sym[Any], exp: Exp[Any]): Unit = {
    emitValDef(sym, quote(exp))
  }

  def emitBlock(y: Block[Any], blockName:String): Unit = {
    emitComment(s"Block ${blockName} {")
    emitBlock(y)
    emitComment(s"} Block ${blockName}")
  }

	def emitNestedIdx(cchain:Exp[CounterChain], inds:List[Sym[FixPt[Signed,B32,B0]]]) = {
    val Def(EatReflect(Counterchain_new(counters, nIter))) = cchain
	  inds.zipWithIndex.foreach {case (iter, idx) => emitValDef(iter, counters(idx)) }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Counterchain_new(counters,nIters) =>

    case e@Pipe_foreach(cchain, func, inds) =>
      emitController(sym, Some(cchain))
      emitNestedIdx(cchain, inds)
      emitBlock(func, s"${quote(sym)} Foreach")             // Map function

    case e@Pipe_fold(cchain, accum, fA, iFunc, ldFunc, stFunc, func, rFunc, inds, idx, acc, res, rV) =>
      emitController(sym, Some(cchain))
      emitNestedIdx(cchain, inds)
      emitBlock(iFunc, s"${quote(sym)} Index Calculation")
      emitValDef(idx, quote(getBlockResult(iFunc)))
      emitBlock(func, s"${quote(sym)} Foreach")
      emitBlock(ldFunc, s"${quote(sym)} Load")
      emitValDef(rV._1, quote(getBlockResult(ldFunc)))
      emitValDef(rV._2, quote(getBlockResult(func)))
      emitBlock(rFunc, s"${quote(sym)} Reduce")
      emitValDef(res, quote(getBlockResult(rFunc)))
      emitBlock(stFunc, s"${quote(sym)} Store")

		case e@Pipe_parallel(func: Block[Unit]) =>
      emitController(sym, None)
      emitBlock(func, s"${quote(sym)} Parallel")

    case _ => super.emitNode(sym,rhs)
  }

  def emitController(sym:Sym[Any], cchain:Option[Exp[CounterChain]]) {
    val smStr = styleOf(sym) match {
			case Coarse => "MPSM"
      case Fine => "PipeSM"
      case Disabled => "SeqSM"
      case ForkJoin => "ParSM"
    }
    emitComment(s"""${smStr} ${quote(sym)} {""")

    /* Emit done signal */
    if (parentOf(sym).isEmpty) {
      emit(s"""DFEVar ${quote(sym)}_en = top_en;""")
      emit(s"""DFEVar ${quote(sym)}_done = dfeBool().newInstance(this);""")
      emit(s"""top_done <== ${quote(sym)}_done;""")
      enDeclaredSet += sym
      doneDeclaredSet += sym
    }

    /* State Machine Instatiation */
    emit(s"""SMIO ${quote(sym)}_sm = addStateMachine("${quote(sym)}_sm", new ${quote(sym)}_${smStr}(this));""")
    // IO
    emit(s"""${quote(sym)}_sm.connectInput("sm_en", ${quote(sym)}_en);""")
    emit(s"""${quote(sym)}_done <== stream.offset(${quote(sym)}_sm.getOutput("sm_done"),-1);""")
    styleOf(sym) match {
      case Fine =>
        emit(s"""DFEVar ${quote(sym)}_rst_en = ${quote(sym)}_sm.getOutput("rst_en");""")
      case Coarse =>
		    val Def(EatReflect(Counterchain_new(counters, nIters))) = cchain.get
        emit(s"""${quote(sym)}_sm.connectInput("sm_numIter", ${quote(getBlockResult(nIters))});""")
        emit(s"""DFEVar ${quote(sym)}_rst_en = ${quote(sym)}_sm.getOutput("rst_en");""")
      case Disabled =>
		    val Def(EatReflect(Counterchain_new(counters, nIters))) = cchain.get
        emit(s"""${quote(sym)}_sm.connectInput("sm_numIter", ${quote(getBlockResult(nIters))});""")
        emit(s"""DFEVar ${quote(sym)}_rst_en = ${quote(sym)}_sm.getOutput("rst_en");""")
      case ForkJoin =>
    }

    /* Control Signals to Children Controllers */
    if (styleOf(sym)!=Fine) {
		  childrenOf(sym).zipWithIndex.foreach { case (c, idx) =>
		  	emit(s"""DFEVar ${quote(c)}_done = dfeBool().newInstance(this);""")
		  	emit(s"""${quote(sym)}_sm.connectInput("s${idx}_done", ${quote(c)}_done);""")
        emit(s"""DFEVar ${quote(c)}_en = ${quote(sym)}_sm.getOutput("s${quote(idx)}_en");""")
		  	enDeclaredSet += c
		  	doneDeclaredSet += c
		  }
    }

    if (styleOf(sym)!=ForkJoin) {
      emitCChainCtrl(sym, cchain.get)
    }
    if (styleOf(sym)==Fine){
      emit(s"""DFEVar ${quote(sym)}_rst_done = dfeBool().newInstance(this);""")
      emit(s"""${quote(sym)}_sm.connectInput("rst_done", ${quote(sym)}_rst_done);""")
      emit(s"""DFEVar ${quote(sym)}_rst_en = ${quote(sym)}_sm.getOutput("rst_en");""")

      emit(s"""OffsetExpr ${quote(sym)}_offset = stream.makeOffsetAutoLoop("${quote(sym)}_offset");""")
      emit(s"""${quote(sym)}_rst_done <== stream.offset(${quote(sym)}_rst_en, -${quote(sym)}_offset-1);""")

    }

    emitComment(s"""} ${smStr} ${quote(sym)}""")
  }

  def emitCChainCtrl(sym: Sym[Any], cchain: Exp[CounterChain]) {
		val Def(EatReflect(Counterchain_new(counters, nIters))) = cchain

    /* Reset CounterChain */
    //TODO: support reset of counterchain to sequential and metapipe in templete
    counters.zipWithIndex.map {case (ctr,i) =>
      val Def(EatReflect(Counter_new(start, end, step, par))) = ctr
      emit(s"""${quote(sym)}_sm.connectInput("sm_maxIn_$i", ${quote(end)});""")
      emit(s"""DFEVar ${quote(ctr)}_max = ${quote(sym)}_sm.getOutput("ctr_maxOut");""")
    }
    emit(s"""${quote(sym)}_sm.connectInput("ctr_done", ${quote(cchain)}_done);""")
    emit(s"""DFEVar ${quote(cchain)}_en_from_pipesm = ${quote(sym)}_sm.getOutput("ctr_en");""")

    /* Emit CounterChain */
		emit(s"""DFEVar ${quote(cchain)}_done = dfeBool().newInstance(this);""")
		doneDeclaredSet += cchain
    styleOf(sym) match {
      case Fine =>
        val Def(EatReflect(d)) = sym; d match {
          case n:Pipe_foreach =>
            val writesToAccumRam = writtenIn(sym).exists {s => s match {
                case Def(EatReflect(Bram_new(_,_))) => isAccum(sym)
                case _ => false
              }
            }
            if (writesToAccumRam) {
              emitMaxJCounterChain(cchain, Some(s"${quote(cchain)}_en_from_pipesm | ${quote(sym)}_rst_en"),
                    Some(s"stream.offset(${quote(cchain)}_en_from_pipesm & ${quote(cchain)}_chain.getCounterWrap(${quote(counters.head)}), -${quote(sym)}_offset-1)"))
            } else {
              emitMaxJCounterChain(cchain, Some(s"${quote(cchain)}_en_from_pipesm"))
            }
          case n:Pipe_fold[_,_] =>
			      //TODO : what is this? seems like all reduce supported are specialized
            //  def specializeReduce(r: ReduceTree) = {
            //  val lastGraph = r.graph.takeRight(1)(0)
            //  (lastGraph.nodes.size == 1) & (r.accum.input match {
            //    case in:Add => true
            //    case in:MinWithMetadata => true
            //    case in:MaxWithMetadata => true
            //    case in:Max => true
            //    case _ => false
            //  })
			      val specializeReduce = true;
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
      case Coarse =>
        emitMaxJCounterChain(cchain, Some(s"${quote(childrenOf(sym).head)}_done"))
      case Disabled =>
		    emitMaxJCounterChain(cchain, Some(s"${quote(childrenOf(sym).last)}_done"))
    }

  }

	def emitMaxJCounterChain(cchain: Exp[CounterChain], en: Option[String], done: Option[String]=None) = {
		val sym = cchain
    // 'En' and 'done' signal contract: Enable signal is declared here, done signal must be
    // declared before this method is called.
    // Both signals are defined here.
    emitComment("MaxJCounterChain {")
    if (!enDeclaredSet.contains(sym)) {
      emit(s"""DFEVar ${quote(sym)}_en = ${en.get};""")
      enDeclaredSet += sym
    }
    emit(s"""CounterChain ${quote(sym)} = control.count.makeCounterChain(${quote(sym)}_en);""")

    // For Pipes, max must be derived from PipeSM
    // For everyone else, max is as mentioned in the ctr
		val Def(EatReflect(Counterchain_new(counters, nIter))) = cchain
		counters.zipWithIndex.map {case (ctr,i) =>
			val Def(EatReflect(Counter_new(start, end, step, _))) = ctr
			val max = parentOf(cchain.asInstanceOf[Rep[CounterChain]]) match {
				case Some(s) => s.tp match {
          //TODO
					case p:Pipeline => s"${quote(ctr)}_max"
					case _ => quote(end)
				}
				case None => quote(end)
			}
      if (parOf(ctr) == 1) {
        emit(s"""${maxJPre(ctr)} ${quote(ctr)} = ${quote(cchain)}.addCounter(${quote(max)}, ${quote(step)});""")
      } else {
        emit(s"""${maxJPre(ctr)} ${quote(ctr)} = ${quote(cchain)}.addCounterVect(${quote(parOf(ctr))}, ${quote(max)}, ${quote(step)});""")
      }
    }

    val doneStr = if (!done.isDefined) {
        s"stream.offset(${quote(cchain)}.getCounterWrap(${quote(counters(0))}),-1)"
    } else {
      done.get
    }

    if (!doneDeclaredSet.contains(sym)) {
      emit(s"""DFEVar ${quote(sym)}_done = $doneStr;""")
      doneDeclaredSet += sym
    } else {
      emit(s"""${quote(sym)}_done <== $doneStr;""")
  	}

    emitComment("} MaxJCounterChain")
  }


}
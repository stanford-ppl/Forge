package dhdl.compiler.ops

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

import scala.virtualization.lms.internal.{Traversal, QuotingExp}

import scala.reflect.{Manifest,SourceContext}
import java.io.{File, PrintWriter}
import sys.process._
import scala.language.postfixOps

import scala.collection.mutable.Set

import ppl.delite.framework.Config

trait DotIRPrinter extends Traversal with QuotingExp {
	val IR: DHDLExp with MemoryAnalysisExp
	import IR.{infix_until => _, looprange_until => _, println => _, _}

  override val name = "DotIRPrinter"
  override val eatReflect = true
  debugMode = false

  var inHwScope = false
  var fileNum = 0
  //val emittedCtrChain = Set.empty[Exp[Any]]
  val emittedSize = Set.empty[Exp[Any]]

  def alwaysGen(x: => Any) {
    val oldScope = inHwScope
    inHwScope = true
    x
    inHwScope = oldScope
  }

  /* Special case to handle nodes producing HW inputs outside of hardware scope */
  def hackGen(x: Exp[Any]): Unit = x match {
    case Def(EatReflect(_:Reg_new[_])) => // Nothing
    case ConstFix(_) => // Nothing
    case ConstFlt(_) => // Nothing
    case Def(d) if !emittedSize.contains(x) =>
      alwaysGen{ traverse(x.asInstanceOf[Sym[Any]], d) }
      emittedSize += x
      syms(d).foreach{s => hackGen(s) }
    case _ => // Nothing
  }


	var stream:PrintWriter = _
	def newStream(fileName:String):PrintWriter = {
		val path = Config.buildDir + java.io.File.separator + fileName + ".dot"
		val pw = new PrintWriter(path)
		pw
	}

  def quoteOthers(s: Sym[Any], id:Int):String = {
		var tstr = s.tp.erasure.getSimpleName()
		tstr = tstr.replace("DHDL","")
		if (isRegister(s.tp)) {
			tstr = tstr.replace("Register", regType(s) match {
				case Regular => "Reg"
				case ArgumentIn => "ArgIn"
				case ArgumentOut => "ArgOut"
			})
		}
    else if (isPipeline(s.tp)) {
      tstr = tstr.replace("Pipeline", styleOf(s) match {
				case Fine => "Pipe"
				case Coarse => "MetaPipe"
				case Disabled => "Sequential"
			})
		}
    else if (isBRAM(s.tp)) {
      tstr = tstr.replace("BlockRAM", "BRAM")
    }
    tstr + nameOf(s).map{n => "_"+n}.getOrElse("") + "_x" + id
  }

  def getId(x: Exp[Any]):Int = x match {
    case s@Sym(id) => id
    case _ => -1
  }

  override def quote(x: Exp[Any]):String = x match {
    case s@Sym(id) => s match {
			case Def(ConstFix(c)) => c.toString
			case Def(ConstFlt(c)) => c.toString
			case Def(ConstBit(c)) => c.toString
      case Def(Tpes_Fix_to_int(v)) => 
        val symStr = quoteOthers(s, id)
        emitValDef(symStr, v)
        symStr
      case Def(Tpes_Int_to_fix(v)) => 
        val symStr = quoteOthers(s, id)
        emitValDef(symStr, v)
        symStr
      case Def(Tpes_String_to_fixpt(s)) => "args_x" + id
			case _ =>
        quoteOthers(s, id)
		}
    case _ => super.quote(x)
  }

	def emit(str: String):Unit = {
		stream.println(str)
	}
	def emitEdge(x:Exp[Any], y:Exp[Any]):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)}""")
	}
	def emitEdge(x:Exp[Any], y:Exp[Any], label:String):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)} [ headlabel="${label}" ]""")
	}
	def emitComment(str: String):Unit = {
		stream.println(s"""/* $str */ """)
	}

  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
    println("-------Start DotIRPrinter --------")
    val filename = Config.degFilename.replace(".deg", "")
    stream = newStream(filename + fileNum)
		//emittedCtrChain.clear
		emittedSize.clear
    emit("digraph{")
    emit(s"compound=true")
    emit(s"""graph [splines="ortho" clusterrank="local" rankdir = "LR"]""")
    emit(s"edge [arrowsize=$arrowSize penwidth=$edgeThickness]")
    emit(s"""node [fontsize=$fontsize shape=$defaultShape style="filled" fillcolor=$bgcolor ]""")
    emit(s"fontsize=$fontsize")
		b
	}

  override def postprocess[A:Manifest](b: Block[A]): Block[A] = {
    emit("}")
    stream.flush()
    stream.close()
    fileNum += 1
		b
	}

  def emitBlock(y: Block[Any]): Unit = traverseBlock(y)

  def emitBlock(y: Block[Any], name:String, label:String, color:String): Unit = {
    emit(s"""subgraph cluster_${name} {""")
    emit(s"""label="${name}" """)
    emit(s"""style="filled" """)
		emit(s"""fillcolor=$color""")
		emit(s"""color=none""")
		emitBlock(y)
		emit(s"""}""")
	}

	def emitValDef(lhs: Exp[Any], rhs: Exp[Any]):Unit = {
		emitValDef(lhs.asInstanceOf[Sym[Any]], quote(rhs))
	}
	def emitValDef(lhs: String, rhs: Exp[Any]):Unit = {
		emitValDef(lhs, quote(rhs))
	}
  def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    emitValDef(quote(sym), rhs)
  }
  def emitValDef(sym: String, rhs: String): Unit = {
		stream.println(s"""define(`${sym}', `${rhs}')""")
  }

	def emitNestedIdx(cchain:Exp[CounterChain], inds:List[Sym[FixPt[Signed,B32,B0]]]) = {
    val Def(EatReflect(Counterchain_new(counters, nIter))) = cchain
	  inds.zip(counters).foreach{case (iter, ctr) => emitValDef(iter, ctr) }
  }

  def emitParallelNestedIdx(cchain: Exp[CounterChain], inds: List[List[Sym[FixPt[Signed,B32,B0]]]]) = {
    val Def(EatReflect(Counterchain_new(counters, nIter))) = cchain
    inds.zip(counters).foreach{case (iters, ctr) => iters.foreach{iter => emitValDef(iter, ctr) }}
  }

  def emitCounter(sym:Exp[Any]) = {
    val Def(EatReflect(d)) = sym; d match {
      case e@Counter_new(start,end,step,_) =>
		  	var l = s""""${quote(sym)}"""
        if (isConst(start)) {
		  		l += "|start=" + quote(start)
		  	} else {
		  		emitEdge(start, sym, "start")
		  	}
		  	if (isConst(end)) {
		  		l += "|end=" + quote(end)
		  	} else {
		  		emitEdge(end, sym, "end")
		  	}
		  	if (isConst(step)) {
		  		l += "|step=" + quote(step)
		  	} else {
		  		emitEdge(step, sym, "step")
		  	}
		  	l += "\""
        emit(s"""${quote(sym)} [ label=$l shape="record" style="filled,rounded"
		  				color=$counterInnerColor ]""")
    }
  }

	def emitCtrChain(cchain: Exp[CounterChain]):Unit = {
		val Def(EatReflect(d)) = cchain
		emitCtrChain(cchain, d.asInstanceOf[Def[Any]])
	}

	def emitCtrChain(sym: Exp[Any], rhs: Def[Any]):Unit = rhs match {
	  case e@Counterchain_new(counters, nIter) =>
    	counters.foreach{ ctr =>
        emitCounter(ctr)
    	}
    	emit(s"""subgraph cluster_${quote(sym)} {""")
    	emit(s""" label=${quote(sym)} """)
    	emit(s""" style="rounded, filled" """)
    	emit(s""" fillcolor=$counterColor""")
    	counters.foreach{ ctr =>
    	  emit(s"""   ${quote(ctr)}""")
    	}
    	emit("}")
			//if (!emittedCtrChain.contains(sym)) {
			//	emittedCtrChain += sym
			//}
		case _ =>
	}

  def emitVector(sym: Sym[Any]):Unit = {
    if (isDblBuf(sym)) {
      emit(s"""${quote(sym)} [margin=0 rankdir="LR" label="{<st> | <ld>}" xlabel="${quote(sym)}"""")
      emit(s"""shape="record" color=$dblbufBorderColor  style="filled"""")
      emit(s"""fillcolor=$vectorFillColor ]""")
    }
    else {
      emit(s"""${quote(sym)} [label="${quote(sym)}" shape="square" style="filled" fillcolor=$vectorFillColor]""")
    }
  }

  def emitOp(sym:Exp[Any], s:String):Unit = {
    emit(s"""${quote(sym)} [label="$s"  shape="square" style="filled" fillcolor="white"]""")
  }

  def emitOp(sym:Exp[Any], s:String, a:Exp[Any]):Unit = {
    var lab = s
    if (isConst(a))
      lab = s"${lab}(${quote(a)})"
    emitOp(sym, lab)
    if (!isConst(a))
      emitEdge(a, sym)
  }

  def emitOp(sym:Exp[Any], s:String, a:Exp[Any], b:Exp[Any]):Unit = {
    var lab = s
    if (isConst(a))
      lab = s"${quote(a)} ${lab}"
    if (isConst(b))
      lab = s"${lab} ${quote(b)}"
    emitOp(sym, lab)
    if (!isConst(a))
      emitEdge(a, sym)
    if (!isConst(b))
      emitEdge(b, sym)
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]): Unit = {
    debug(s"[$inHwScope] $lhs = $rhs")
    //println(s"[$inHwScope] $lhs = $rhs")
    if (inHwScope) emitHWNode(lhs, rhs)
    else emitOtherNode(lhs, rhs)
  }

  def isConst(sym: Exp[Any]) = sym match {
    case Def(ConstBit(_)) => true 
    case Def(ConstFix(_)) => true 
    case Def(ConstFlt(_)) => true 
    case Def(Tpes_Int_to_fix(v)) => true
    case _ => false 
  }

  def emitOtherNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Hwblock(func) =>
      alwaysGen { emitBlock(func) }

    case _:Reg_new[_] => regType(sym) match {
      case Regular if isDblBuf(sym) =>
        emit(s"""${quote(sym)} [margin=0, rankdir="LR", label="{<st> | <ld>}" xlabel="${quote(sym)}" """)
        emit(s"""      shape="record" color=$dblbufBorderColor style="filled" """)
        emit(s"""      fillcolor=$regFillColor ]""")

      case Regular =>
        emit(s"""${quote(sym)} [label="${quote(sym)}" shape="square" style="filled" fillcolor=$regFillColor ]""")

      case ArgumentIn =>
        emit(s"""${quote(sym)} [label="${quote(sym)}" shape="Msquare" style="filled" fillcolor=$regFillColor ]""")

      case ArgumentOut =>
        emit(s"""${quote(sym)} [label="${quote(sym)}" shape="Msquare" style="filled" fillcolor=$regFillColor ]""")
    }

    case Offchip_new(size) =>
      //if (!emittedSize.contains(size)) hackGen(size)
      var label = s""" "${quote(sym)} """
      //if (isConst(size)) {
      //  label += ", size = " + quote(size)
      //}
      //else emitEdge(size, sym, "size")
      label += "\""
      emit(s"""${quote(sym)} [label=$label shape="square" fontcolor="white" color="white" style="filled" """)
      emit(s"""               fillcolor=$dramFillColor color=black]""")

    case ConstBit(v) =>
      emit(s"""${quote(sym)} [label=${quote(v)} style="filled" fillcolor="lightgray" color="none"]""")
    case ConstFixPt(v,_,_,_) =>
      emit(s"""${quote(sym)} [label=${quote(v)} style="filled" fillcolor="lightgray" color="none"]""")
    case ConstFltPt(v,_,_) =>
      emit(s"""${quote(sym)} [label=${quote(v)} style="filled" fillcolor="lightgray" color="none"]""")

    case Tpes_Fix_to_int(v) => emitValDef(sym, v)
    case Tpes_Int_to_fix(v) => emitValDef(sym, v)

    case Reflect(d, u, es) => traverse(sym, d)
    case _ =>
      //println(sym + "=" + rhs)
      debug(s"...ignored")
      blocks(rhs).foreach{blk => traverseBlock(blk)}
  }

  def emitHWNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {

	  case e@Counterchain_new(counters, nIter) =>

		case e@Pipe_parallel(func) =>
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""	label = "parallel ${quote(sym)}"""")
      emit(s"""	style = "filled, bold"""")
      emit(s"""	fillcolor = $parallelFillColor""")
      emit(s"""	color = $parallelBorderColor""")
      emitBlock(func)
			emit(s"""}""")

    case e@Unit_pipe(func) =>
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s""" label = "pipe ${quote(sym)}"""")
      emit(s""" style = "filled, bold"""")
      emit(s""" fillcolor = $pipeFillColor""")
      emit(s""" color = $pipeBorderColor""")
      emitBlock(func)
      emit(s"""}""")

    case e@Pipe_foreach(cchain, func, inds) =>
			emitNestedIdx(cchain, inds)
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""label="${quote(sym)}"""")
      emit(s"""color=$pipeBorderColor""")
      emit(s"""style="bold, filled" """)
			emit(s"""fillcolor=$pipeFillColor""")
			emitCtrChain(cchain)
      emitBlock(func, quote(sym) + "_foreachFunc", "foreachFunc", foreachFillColor)             // Map function
      emit("}")

    case e@Pipe_fold(cchain, accum, zero, fA, iFunc, ldFunc, stFunc, func, rFunc, inds, idx, acc, res, rV) =>
			emitValDef(acc, accum)
      emitNestedIdx(cchain, inds)
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""label="${quote(sym)}"""")
      emit(s"""color=$pipeBorderColor""")
      emit(s"""style="bold, filled" """)
			emit(s"""fillcolor=$pipeFillColor""")
      emitValDef(acc, accum)
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

    case e@Accum_fold(ccOuter, ccInner, accum, zero, fA, iFunc, func, ldPart, ldFunc, rFunc, stFunc, indsOuter, indsInner, idx, part, acc, res, rV) =>
      emitValDef(acc, accum)
      emitNestedIdx(ccOuter, indsOuter)
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""  label="${quote(sym)}"""")
      emit(s"""  style="bold, filled" """)
      emit(s"""  fillcolor=$mpFillColor""")
      emit(s"""  color=$mpBorderColor""")
      emitCtrChain(ccOuter)
      if (!isUnitCounterChain(ccInner)) {
        emitCtrChain(ccInner)
        emitNestedIdx(ccInner, indsInner)
      }
      emitBlock(func, quote(sym) + "_mapFunc", "mapFunc", mapFillColor)
      emitValDef(part, getBlockResult(func))
      emitBlock(iFunc, quote(sym) + "_idxFunc", "idxFunc", ldFillColor)
      emitValDef(idx, getBlockResult(iFunc))
      emitBlock(ldPart, quote(sym) + "_ldPart", "ldPart", ldFillColor)
      emitBlock(ldFunc, quote(sym) + "_ldFunc", "ldFunc", ldFillColor)
      emitValDef(rV._1, getBlockResult(ldPart))
      emitValDef(rV._2, getBlockResult(ldFunc))
      emitBlock(rFunc, quote(sym) + "_reduceFunc", "reduceFunc", reduceFillColor)
      emitValDef(res, getBlockResult(rFunc))
      emitBlock(stFunc, quote(sym) + "_stFund", "stFunc", stFillColor)
      emit("}")

    case Cache_new(offchip) =>
      if (isDblBuf(sym)) {
        emit(s"""${quote(sym)} [margin=0 rankdir="LR" label="{<st> | <ld>}" xlabel="${quote(sym)}"""")
        emit(s"""               shape="record" color=$dblbufBorderColor  style="filled" """)
        emit(s"""               fillcolor=$cacheFillColor ]""")
      }
      else {
        emit(s"""${quote(sym)} [label="${quote(sym)}" shape="square" style="filled" fillcolor=$cacheFillColor ]""")
      }
      emitEdge(offchip, sym)

    case Cache_load(cache, addr) =>
      emitEdge(addr, cache, "addr")
      emitValDef(sym, cache)

    case Cache_store(cache, addr, value) =>
      emitEdge(addr, cache, "addr")
      emitEdge(value, cache, "data")

    case Fifo_new(size, zero) =>
      val qsym = quote(sym)
      if (isDblBuf(sym)) {
        emit(s"""$qsym [margin=0 rankdir="LR" label="{<st> | <ld>}" xlabel="$qsym """")
        emit(s"""shape="record" color=$dblbufBorderColor  style="filled"""")
        emit(s"""fillcolor=$bramFillColor ]""")
      }
      else {
        emit(s"""$qsym [label="$qsym " shape="square" style="filled" fillcolor=$bramFillColor ]""")
      }
    case Push_fifo(fifo,value,en) =>
      emitEdge(value,fifo, "data")
      emitEdge(en,fifo,"en")

    case Pop_fifo(fifo) =>
      emitValDef(sym, fifo)

    case Count_fifo(fifo) =>
      emitValDef(sym, fifo)

		case Bram_new(size, zero) =>
      val qsym = quote(sym)
      if (isDblBuf(sym)) {
      	emit(s"""$qsym [margin=0 rankdir="LR" label="{<st> | <ld>}" xlabel="$qsym """")
        emit(s"""shape="record" color=$dblbufBorderColor  style="filled"""")
        emit(s"""fillcolor=$bramFillColor ]""")
      }
      else {
        emit(s"""$qsym [label="$qsym " shape="square" style="filled" fillcolor=$bramFillColor ]""")
      }

    case Bram_load(bram,addr) =>
      emitEdge(addr, bram, "addr")
			emitValDef(sym, bram)

    case Bram_store(bram,addr,value) =>
      emitEdge(addr, bram, "addr")
      emitEdge(value, bram, "data")

    case e@Offchip_store_cmd(mem,stream,ofs,len,p) =>
      emitEdge(stream, mem, "data")
      emitEdge(ofs, mem, "addr")

    case e@Offchip_load_cmd(mem,stream,ofs,len,p) =>
      emitEdge(ofs, mem, "addr")
      emitEdge(mem, stream, "data")

    case Reg_read(reg) =>
      emitValDef(sym, reg)

    case Reg_write(reg, value) =>
      emitEdge(value, reg)

    case Fixpt_to_fltpt(x) =>
      emit(s"""${quote(sym)} [ label="fix2flt" ]""")
      emitEdge(x, sym)
    case Fltpt_to_fixpt(x) =>
      emit(s"""${quote(sym)} [ label="flt2fix" ]""")
      emitEdge(x, sym)
    case Convert_fixpt(x) =>
      emit(s"""${quote(sym)} [ label="fix2fix" ]""")
      emitEdge(x, sym)
    case Convert_fltpt(x) =>
      emit(s"""${quote(sym)} [ label="flt2flt" ]""")
      emitEdge(x, sym)

    case FixPt_Neg(a)   => emitOp(sym, "neg", a)
    case FixPt_Add(a,b) => emitOp(sym, "+"  , a, b)
    case FixPt_Sub(a,b) => emitOp(sym, "-"  , a, b)
    case FixPt_Mul(a,b) => emitOp(sym, "*"  , a, b)
    case FixPt_Div(a,b) => emitOp(sym, "/"  , a, b)
    case FixPt_Mod(a,b) => emitOp(sym, "%"  , a, b)
    case FixPt_Lt(a,b)  => emitOp(sym, "<"  , a, b)
    case FixPt_Leq(a,b) => emitOp(sym, "<=" , a, b)
    case FixPt_Neq(a,b) => emitOp(sym, "!=" , a, b)
    case FixPt_Eql(a,b) => emitOp(sym, "==" , a, b)
    case FixPt_And(a,b) => emitOp(sym, "&"  , a, b)
    case FixPt_Or(a,b)  => emitOp(sym, "|"  , a, b)
    case FixPt_Lsh(a,b) => emitOp(sym, "<<" , a, b)
    case FixPt_Rsh(a,b) => emitOp(sym, ">>" , a, b)

    case FltPt_Neg(a)   => emitOp(sym, "neg", a)
    case FltPt_Add(a,b) => emitOp(sym, "+"  , a, b)
    case FltPt_Sub(a,b) => emitOp(sym, "-"  , a, b)
    case FltPt_Mul(a,b) => emitOp(sym, "*"  , a, b)
    case FltPt_Div(a,b) => emitOp(sym, "/"  , a, b)
    case FltPt_Lt(a,b)  => emitOp(sym, "<"  , a, b)
    case FltPt_Leq(a,b) => emitOp(sym, "<=" , a, b)
    case FltPt_Neq(a,b) => emitOp(sym, "!=" , a, b)
    case FltPt_Eql(a,b) => emitOp(sym, "==" , a, b)

    case Bit_Not(a)    => emitOp(sym, "~"  , a)
    case Bit_And(a,b)  => emitOp(sym, "&&" , a, b)
    case Bit_Or(a,b)   => emitOp(sym, "||" , a, b)
    case Bit_Xor(a,b)  => emitOp(sym, "==" , a, b)
    case Bit_Xnor(a,b) => emitOp(sym, "!=" , a, b)

    case FixPt_Abs(a)  => emitOp(sym, "abs" , a)
    case FltPt_Abs(a)  => emitOp(sym, "abs" , a)
    case FltPt_Log(a)  => emitOp(sym, "log" , a)
    case FltPt_Exp(a)  => emitOp(sym, "exp" , a)
    case FltPt_Sqrt(a) => emitOp(sym, "sqrt", a)

    case Rand_flt() => emitOp(sym, s"RandFlt_x${getId(sym)}")
    case Rand_fix() => emitOp(sym, s"RandFix_x${getId(sym)}")
    case Rand_bit() => emitOp(sym, s"RandBit_x${getId(sym)}")

    //case Rand_flt() => emit(s"""${quote(sym)} [label="RandFlt_x${getId(sym)}" shape="square" style="filled" fillcolor="white"]""")
    //case Rand_fix() => emit(s"""${quote(sym)} [label="RandFix_x${getId(sym)}" shape="square" style="filled" fillcolor="white"]""")
    //case Rand_bit() => emit(s"""${quote(sym)} [label="RandBit_x${getId(sym)}" shape="square" style="filled" fillcolor="white"]""")

    case Mux2(s,a,b) =>
      emit(s"""${quote(sym)} [label="mux", shape="diamond" style="filled" fillcolor="white"]""")
      emitEdge(s, sym, "sel")
      emitEdge(a, sym, "a")
      emitEdge(b, sym, "b")

    case ParPipeForeach(cc,func,inds) =>
      emitParallelNestedIdx(cc, inds)
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""label="${quote(sym)}"""")
      emit(s"""color=$pipeBorderColor""")
      emit(s"""style="bold, filled" """)
      emit(s"""fillcolor=$pipeFillColor""")
      emitCtrChain(cc)
      emitBlock(func, quote(sym) + "_foreach", "foreach", foreachFillColor)             // Map function
      emit("}")

    case ParPipeReduce(cc,accum,func,rFunc,inds,acc,rV) =>
      emitValDef(acc, accum)
      emitParallelNestedIdx(cc, inds)
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""label="${quote(sym)}"""")
      emit(s"""color=$pipeBorderColor""")
      emit(s"""style="bold, filled" """)
      emit(s"""fillcolor=$pipeFillColor""")
      emitCtrChain(cc)
      emitBlock(func, quote(sym) + "_mapreduce", "mapreduce", foreachFillColor)             // Map function
      emit("}")

		case _ => emitOtherNode(sym, rhs)
	}

	val arrowSize = 0.6
	val edgeThickness = 0.5
	val ctrlColor = s""""red""""
	val counterColor = s""""#e8e8e8""""
	val counterInnerColor = s""""gray""""
	val fontsize = 10
	val defaultShape = "square"
	val bgcolor = s""""white""""

	// Pipe Colors
	//val pipeFillColor = "#4FA1DB"
	val pipeFillColor = s""""white""""
	val pipeBorderColor = s""""black""""

	// Block Colors
	val foreachFillColor = s""""#F6EC93""""
	val mapFillColor = s""""#56D9D2""""
	val reduceFillColor = s""""#FE7365""""
	val ldFillColor = s""""#7be58f""""
	val stFillColor = s""""#7be58f""""

	// Metapipeline colors
	val mpFillColor = s""""#4FA1DB""""
	val mpBorderColor = s""""#4FA1DB""""
	val mpStageFillColor = s""""#BADDFF""""
	val mpStageBorderColor = s""""none""""

	// Parallel colors
	//val parallelFillColor = "#4FDBC2"
	val parallelFillColor = s""""white""""
	//val parallelBorderColor = s""""#00AB8C""""
	val parallelBorderColor = s""""black""""
	val parallelStageFillColor = s""""#CCFFF6""""
	val parallelStageBorderColor = s""""none""""

	// Tile transfer colors
	val tileTransFillColor = s""""#FFA500""""

	// Memories
  val vectorFillColor = s""""#8bd645""""
  val fifoFillColor = s""""70C6E6""""
	val bramFillColor = s""""#70C6E6""""
	val cacheFillColor = s""""#B3A582""""
	val dramFillColor = s""""#685643""""
	val regFillColor = s""""#8bd645""""
	val dblbufBorderColor = s""""#4fb0b0""""

}

package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.{Traversal}
import java.io.{File, PrintWriter}
import sys.process._
import scala.language.postfixOps

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._
import scala.collection.mutable.Set

import ppl.delite.framework.DeliteApplication

trait DotIRPrinterExp {
  this: DHDLExp =>
}

trait DotIRPrinter extends Traversal  {
	val IR:DHDLExp
	import IR.{infix_until => _, looprange_until => _, println => _, _}

  debugMode = true 
  override val name = "DotIRPrinter"

	val buildDir:String = "/Users/Yaqi/Documents/hyperdsl/published/DHDL"
	var stream:PrintWriter = _
	def newStream(fileName:String):PrintWriter = {
		val path = buildDir + java.io.File.separator + fileName + ".dot"
		val pw = new PrintWriter(path)
		pw
	}

  def quote(x: String):String = x

  def quote(x: Exp[Any]):String = x match {
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
					val quoteStr = tstr + (if (nameOf(s)!="") "_" else "") + nameOf(s) + "_x" + n
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
    case s => "ERROR: Dont know how to quote" 
  }

	def emit(str: String):Unit = {
		stream.println(str)
	}
	def emitEdge(x:Sym[Any], y:Exp[Any]):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)}""")
	}
	def emitEdge(x:Sym[Any], y:Exp[Any], label:String):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)} [ headlabel="${label}" ]""")
	}
	def emitEdge(x:Sym[Any], y:Sym[Any]):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)}""")
	}
	def emitEdge(x:Sym[Any], y:Sym[Any], label:String):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)} [ headlabel="${label}" ]""")
	}
	def emitEdge(x:Exp[Any], y:Sym[Any]):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)}""")
	}
	def emitEdge(x:Exp[Any], y:Sym[Any], label:String):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)} [ headlabel="${label}" ]""")
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


	val emittedCtrChain = Set.empty[Exp[Any]]
	val emittedSize = Set.empty[Exp[Any]]
  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
    stream = newStream("DotIR")
		emittedCtrChain.clear
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

    val filepath = s"${buildDir}${java.io.File.separator}DotIR"
	  println("--------generate dhdl dot IR ------------")
	  // Move define macro to beginning of the file
	  Process(s"cp ${filepath}.dot ${filepath}_pre.m4")!
	  Process(s"grep 'define' ${filepath}_pre.m4 > ${filepath}.m4")!
	  Process(s"grep -v '^define' ${filepath}_pre.m4 >> ${filepath}.m4")!
	  // Generate dot file
	  Process(s"m4 ${filepath}.m4 > ${filepath}.dot")!
	  // Temp fix to dot gen
	  //s"sed -i '' '/()/d' ${filepath}.dot" !
	  // Generate dot graph
	  Process(s"dot -Tpdf ${filepath}.dot > ${filepath}.pdf")!
	  Process(s"open ${filepath}.pdf")!

		b
	}

  override def traverseStm(stm: Stm): Unit = stm match { // override this to implement custom traversal
    case TP(sym, rhs) => {
			traverseNode(sym,rhs)
			super.traverseStm(stm)
		}
    case _ => super.traverseStm(stm)
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
  def emitValDef(sym: Sym[Any], rhs: String): Unit = {
		stream.println(s"""define(`${quote(sym)}', `${rhs}')""")
  }

	def emitNestedIdx(cchain:Exp[CounterChain], inds:List[Sym[FixPt[Signed,B32,B0]]]) = {
    val Def(EatReflect(Counterchain_new(counters, nIter))) = cchain
	  inds.zipWithIndex.foreach {case (iter, idx) => emitValDef(iter, counters(idx)) }
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

  def traverseNode(sym: Sym[Any], rhs: Def[Any]): Unit = {
    val qsym = quote(sym)
    rhs match {
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
			if (parentOf(sym).isEmpty) {
				emitCtrChain(sym, rhs)
			}

		case e@Pipe_parallel(func: Block[Unit]) =>
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""	label = "parallel_${quote(sym)}"""")
      emit(s"""	style = "filled, bold"""")
      emit(s"""	fillcolor = $parallelFillColor""")
      emit(s"""	color = $parallelBorderColor""")
      emitBlock(func)
			emit(s"""}""")

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

    case e@Pipe_reduce(cchain, accum, iFunc, ldFunc, stFunc, func, rFunc, inds, idx, acc, res, rV) =>
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
			val Def(EatReflect(d)) = cchain
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

    case e@Block_reduce(ccOuter, ccInner, accum, iFunc, func, ldPart, ldFunc, rFunc, stFunc, indsOuter, indsInner, idx, part, acc, res, rV) =>
      emit(s"""subgraph ${quote(sym)} {""")
      emit(s"""  label = quote(sym)""")
      emit(s"""  style = "filled" """)
      emit(s"""  fillcolor = "${mpFillColor}" """)
      emit(s"""  color = "${mpBorderColor}" """)
      val sym_ctrl = quote(sym) + "_ctrl"
      emit(s"""  ${sym_ctrl} [label="ctrl" height=0 style="filled" fillcolor="${mpBorderColor} "]""")
      emit(s"""}""")

    case TileTransfer(mem,local,strides,memOfs,tileStrides,cchain,iters, store) => // Load
			val l = s"Tile${if (store) "Store" else "Load"}_" + qsym.split("_")(1)
      emit(s"""subgraph cluster_${quote(sym)} {""")
			emit(s"""label="$l"""")
			emit(s"""shape="rectangle"""")
			emit(s"""style="rounded, filled"""")
			emit(s"""fillcolor=$tileTransFillColor""")
			emit(s"""color="black"""")
			var nl = if (store) "TileSt" else "TileLd"
			nl += "|stride=\\{"
			strides.zipWithIndex.foreach{ case (s, i) =>
				if (quote(s).forall(_.isDigit)) {
					nl += s"$i:${quote(s)}"
					if (i!=strides.length-1) nl += ", "
				} else {
					emitEdge(s, sym, s"stride $i")
				}
			}
			nl += "\\}"
			/*if (quote(memOfs).forall(_.isDigit))
				nl += s"|memOfs=${quote(memOfs)}"
			else
				emitEdge(memOfs, sym, "memOfs")*/
			emit(s"""${quote(sym)} [label="$nl" shape="record" style="rounded, filled" color="black" fillcolor="gray"]""")
			emitCtrChain(cchain)
			emit(s"""} """)
			if (store) {
				emitEdge(sym, mem)
				emitEdge(local, sym)
			} else {
				emitEdge(mem, sym)
				emitEdge(sym, local)
			}

		case Offchip_new(size) =>
			/* Special case to hand nodes producing size of offchip outside hardware scope. Not actual
       * codegen to Offchip_new */
			def hackGen(x: Exp[Any]): Unit = x match {
				case Def(EatReflect(_:Reg_new[_])) => // Nothing
				case ConstFix(_) => // Nothing
				case ConstFlt(_) => // Nothing
				case Def(d) =>
					traverseNode(x.asInstanceOf[Sym[Any]], d)
					syms(d).foreach{ s => s match {
							case _ => hackGen(s)
						}
					}
				case _ => // Nothing
			}
			if (!emittedSize.contains(size)) {
				hackGen(size)
				emittedSize += size
			}

    case e:Reg_new[_] => 

		case e@Bram_new(size, zero) =>
      if (isDblBuf(sym)) {
      	emit(s"""$qsym [margin=0 rankdir="LR" label="{<st> | <ld>}" xlabel="$qsym """")
        emit(s"""shape="record" color=$dblbufBorderColor  style="filled"""")
        emit(s"""fillcolor=$bramFillColor ]""")
      } else {
        emit(s"""$qsym [label="$qsym " shape="square" style="filled" fillcolor=$bramFillColor ]""")
      }

    case e@Bram_load(bram,addr) =>
			emit(s"""${quote(addr)} -> ${quote(bram)} [ headlabel="addr" ]""")
			emitValDef(sym, bram)

    case e@Bram_store(bram,addr,value) =>
			emit(s"""${quote(addr)} -> ${quote(bram)} [ headlabel="addr" ]""")
			emit(s"""${quote(value)} -> ${quote(bram)} [ headlabel="data" ]""")

    case Reflect(s, u, effects) =>
      traverseNode(sym, s)
    case Reify(s, u, effects) =>
		case _ => {
			emit("// tp:" + sym.tp.erasure.getSimpleName() + " rhs:" + rhs)
		}
	}
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
	val bramFillColor = s""""#70C6E6""""
	val cacheFillColor = s""""#B3A582""""
	val dramFillColor = s""""#685643""""
	val regFillColor = s""""#8bd645""""
	val dblbufBorderColor = s""""#4fb0b0""""

}

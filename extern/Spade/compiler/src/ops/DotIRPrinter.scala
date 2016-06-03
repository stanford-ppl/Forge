package spade.compiler.ops

import spade.shared._
import spade.shared.ops._
import spade.compiler._
import spade.compiler.ops._

import scala.virtualization.lms.internal.QuotingExp

import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.analysis.HungryTraversal
import java.io.{File, PrintWriter}
import sys.process._
import scala.language.postfixOps

import scala.collection.mutable.Set

import ppl.delite.framework.Config

trait DotIRPrinter extends HungryTraversal with QuotingExp {
	val IR: SpadeExp
	import IR._

  debugMode = false
  override val name = "DotIRPrinter"
  var inHwScope = false
  var fileNum = 0
  val emittedCtrChain = Set.empty[Exp[Any]]
  val emittedSize = Set.empty[Exp[Any]]

  def alwaysGen(x: => Any) {
    val oldScope = inHwScope
    inHwScope = true
    x
    inHwScope = oldScope
  }

	var stream:PrintWriter = _
	def newStream(fileName:String):PrintWriter = {
		val path = Config.buildDir + java.io.File.separator + fileName + ".dot"
		val pw = new PrintWriter(path)
		pw
	}

  override def quote(x: Exp[Any]):String = x match {
    case s@Sym(n) =>
				var tstr = s.tp.erasure.getSimpleName()
				tstr = tstr.replace("Spade","")
				val quoteStr = tstr +"_" + n
				quoteStr
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
    val filename = Config.degFilename.replace(".deg", "")
    stream = newStream(filename + fileNum)
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
  def emitValDef(sym: Sym[Any], rhs: String): Unit = {
		stream.println(s"""define(`${quote(sym)}', `${rhs}')""")
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]): Unit = {
    emitAnyNode(lhs, rhs)
  }

  def emitAnyNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case mn@Reflect(n,_,_) =>
      emitAnyNode(sym, n)
    case mn@Alu_new() =>
      emit(s"""${quote(sym)} [label=\"${quote(sym)}\" style="filled" fillcolor="lightgray" color="none"]""")
    case mn@Link_new(n1,n2) =>
      emitEdge(n1, n2)
    case _ =>
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

package ppl.dsl.forge
package templates
package library

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.util.matching._
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._

import core._
import shared.{BaseGenOps,BaseGenDataStructures}
import Utilities._

trait LibGenOps extends BaseGenOps with BaseGenDataStructures {  
  this: ForgeCodeGenInterpreter =>
  
  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._
  
  override def quote(x: Exp[Any]) : String = x match {    
    case Def(PrintLines(p, lines)) =>    
      // since this is called from emitWithIndent, the first line has an extra indent
      lines.map(l => (" "*4)+quote(l)).mkString(nl) 
      
    case Const(s: String) => replaceWildcards(s) // don't add quotes 
    
    case _ => super.quote(x)
  }  
  
  def emitSingleTaskImpls(opsGrp: DSLOps, stream: PrintWriter) {
    emitBlockComment("SingleTask Impls", stream)   
    stream.println()
    stream.println("trait " + opsGrp.grp.name + "WrapperImpl {")
    stream.println("  this: " + dsl + "Application => ")
    stream.println()    
    for (o <- unique(opsGrp.ops)) { 
      o.opTpe match {
        case single:SingleTask => 
          check(o)
          stream.print("  " + makeOpImplMethodSignature(o))
          stream.println(" = {")
          stream.println(inline(o, single.func))
          stream.println("  }")
          stream.println()
        case _ =>
      }
    }
    stream.println("}")
  }
      
  def emitOp(o: Rep[DSLOp], stream: PrintWriter, indent: Int = 0) {
    val rules = CodeGenRules(o.grp)
    o.opTpe match {
      case `codegenerated` => 
        val rule = rules.find(_.op == o).map(_.rule).getOrElse(err("could not find codegen rule for op: " + o.name))
        emitWithIndent(inline(o, rule), stream, indent) 
      case single:SingleTask => 
        emitWithIndent(makeOpImplMethodNameWithArgs(o), stream, indent)
      case map:Map =>
        check(o)
        val dc = DeliteCollections(map.tpePars._3)        
        emitWithIndent("def func: " + quote(map.tpePars._1) + " => " + quote(map.tpePars._2) + " = " + inline(o, map.func), stream, indent)            
        // TODO: this isn't quite right. how do we know which of dc.allocs tpePars is the one that corresponds to our return tpe, e.g. map.tpePars._2?
        emitWithIndent("val out = " + makeOpMethodName(dc.alloc) + makeTpePars(instTpePar(dc.alloc.tpePars, map.tpePars._1, map.tpePars._2)) + "(" + makeOpMethodNameWithArgs(dc.size) + ")", stream, indent)
        emitWithIndent("for (i <- 0 until " + makeOpMethodNameWithArgs(dc.size) + ") {", stream, indent)            
        emitWithIndent(makeOpMethodName(dc.update) + "(out, i, func(" + makeOpMethodName(dc.apply) + "(" + opArgPrefix+map.argIndex + ", i)))", stream, indent+2)
        emitWithIndent("}", stream, indent)            
        emitWithIndent("out", stream, indent)                
      case zip:Zip =>
        check(o)          
        val dc = DeliteCollections(zip.tpePars._4)        
        emitWithIndent("def func: (" + quote(zip.tpePars._1) + "," + quote(zip.tpePars._2) + ") => " + quote(zip.tpePars._3) + " = " + inline(o, zip.func), stream, indent)            
        emitWithIndent("val out = " + makeOpMethodName(dc.alloc) + makeTpePars(instTpePar(dc.alloc.tpePars, zip.tpePars._1, zip.tpePars._3)) + "(" + makeOpMethodNameWithArgs(dc.size) + ")", stream, indent)
        emitWithIndent("for (i <- 0 until " + makeOpMethodNameWithArgs(dc.size) + ") {", stream, indent)            
        emitWithIndent(makeOpMethodName(dc.update) + "(out, i, func(" + makeOpMethodName(dc.apply) + "(" + opArgPrefix+zip.argIndices._1 + ", i)," + makeOpMethodName(dc.apply) + "(" + opArgPrefix+zip.argIndices._2 + ", i)))", stream, indent+2)
        emitWithIndent("}", stream, indent)            
        emitWithIndent("out", stream, indent)        
    }    
  }
  
  def emitClass(opsGrp: DSLOps, stream: PrintWriter) {
    if (grpIsTpe(opsGrp.grp)) {
      val tpe = grpAsTpe(opsGrp.grp)
      val data = DataStructs.filter(_.tpe == tpe).apply(0) // TODO
      stream.print("class " + data.tpe.name)
      stream.print(makeTpeParsWithBounds(data.tpePars))
      stream.print("(")  
      stream.print(makeFieldArgs(data))
      stream.print(") {")
      stream.println()
      stream.println(makeFieldsWithInitArgs(data))
      for (o <- unique(opsGrp.ops) if o.style == infix) {       
        stream.print("  def " + o.name + makeTpeParsWithBounds(o.tpePars.drop(1)))
        stream.print("(" + o.args.drop(1).zipWithIndex.map(t => opArgPrefix + (t._2+1) + ": " + repify(t._1)).mkString(",") + ")") 
        stream.print(makeImplicitArgsWithCtxBoundsWithType(o.implicitArgs, o.tpePars, without = data.tpePars))
        stream.println(" = {")
        // cheat a little bit for consistency: the codegen rule may refer to this arg
        emitWithIndent("val " + opArgPrefix + 0 + " = this", stream, 4)
        emitOp(o, stream, indent=4)
        stream.println("  }")
      }
      stream.println("}")
      stream.println()
    }      
    
    for (o <- unique(opsGrp.ops)) {       
      stream.print("  def " + makeOpMethodName(o) + makeTpeParsWithBounds(o.tpePars))
      stream.print(makeOpArgsWithType(o))
      stream.print(makeOpImplicitArgsWithOverloadWithType(o))
      stream.println(" = {")      
      o.style match {
        case `static` => emitOp(o, stream, indent=4)
        case `infix` if grpIsTpe(opsGrp.grp) => emitWithIndent(opArgPrefix + 0 + "." + o.name + "(" + o.args.drop(1).zipWithIndex.map(t => opArgPrefix + (t._2+1)).mkString(",") + ")", stream, 4)
        case `infix` => emitOp(o, stream, indent=4)
        case `direct` => emitOp(o, stream, indent=4)        
      }
      stream.println("  }")        
    }
    stream.println()    
        
  }
}

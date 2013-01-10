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
      lines.map(l => (" "*4)+inlineStr(quote(l))).mkString(nl) 
    case _ => super.quote(x)
  }  
  
  def inline(o: Rep[DSLOp], rule: String) = {
    var b = rule
    for (i <- 0 until o.args.length) {
      b = b.replaceAllLiterally(o.quotedArg(i), opArgPrefix + i)
    }    
    var c = b
    for (i <- 0 until o.tpePars.length) {
      c = c.replaceAllLiterally(o.tpeInstance(i), o.tpePars.apply(i).name)
    }    
    inlineStr(c)
  }
  
  def inlineStr(s: String) = {
    var x = s
    val quotePattern = new Regex("""quote\((.*?)\)""", "body")
    x = quotePattern replaceAllIn (x, m => m.group("body"))
    
    if (x.startsWith("\"") && x.endsWith("\"")) (x.slice(1,x.length-1))
    else x    
  }
  
  def emitOp(o: Rep[DSLOp], stream: PrintWriter, indent: Int = 0) {
    val rules = CodeGenRules(o.grp)
    o.opTpe match {
      case `codegenerated` => 
        val rule = rules.find(_.op == o).map(_.rule).getOrElse(err("could not find codegen rule for op: " + o.name))
        emitWithIndent(inline(o, quote(rule)), stream, indent) 
      case zip:Zip =>
        check(zip, o)          
        val dc = DeliteCollections(o.args.apply(0))        
        emitWithIndent("def func: (" + quote(zip.tpePars._1) + "," + quote(zip.tpePars._2) + ") => " + quote(zip.tpePars._3) + " = " + zip.func, stream, indent)            
        emitWithIndent("val out = " + makeOpMethodName(dc.alloc) + makeTpePars(dc.alloc.tpePars) + "(" + makeOpMethodNameWithArgs(dc.size) + ")", stream, indent)
        emitWithIndent("for (i <- 0 until " + makeOpMethodNameWithArgs(dc.size) + ") {", stream, indent)            
        emitWithIndent(makeOpMethodName(dc.update) + "(out, i, func(" + makeOpMethodName(dc.apply) + "(" + opArgPrefix+zip.argIndices._1 + ", i)," + makeOpMethodName(dc.apply) + "(" + opArgPrefix+zip.argIndices._2 + ", i)))", stream, indent+2)
        emitWithIndent("}", stream, indent)            
        emitWithIndent("out", stream, indent)        
    }    
  }
  
  def emitClass(ops: DSLOps, stream: PrintWriter) {
    if (grpIsTpe(ops.grp)) {
      val tpe = grpAsTpe(ops.grp)
      val data = DataStructs.filter(_.tpe == tpe).apply(0) // TODO
      stream.print("class " + data.tpe.name)
      stream.print(makeTpeParsWithBounds(data.tpePars))
      stream.print("(")  
      stream.print(makeFieldArgs(data))
      stream.print(") {")
      stream.println()
      stream.println(makeFieldsWithInitArgs(data))
      for (o <- unique(ops.ops) if o.style == infix) {       
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
    
    for (o <- unique(ops.ops)) {       
      stream.print("  def " + makeOpMethodName(o) + makeTpeParsWithBounds(o.tpePars))
      stream.print(makeOpArgsWithType(o))
      stream.print(makeOpImplicitArgsWithOverloadWithType(o))
      stream.println(" = {")      
      o.style match {
        case `static` => emitOp(o, stream, indent=4)
        case `infix` if grpIsTpe(ops.grp) => emitWithIndent(opArgPrefix + 0 + "." + o.name + "(" + o.args.drop(1).zipWithIndex.map(t => opArgPrefix + (t._2+1)).mkString(",") + ")", stream, 4)
        case `infix` => emitOp(o, stream, indent=4)
        case `direct` => emitOp(o, stream, indent=4)        
      }
      stream.println("  }")        
    }
    stream.println()    
        
  }
}

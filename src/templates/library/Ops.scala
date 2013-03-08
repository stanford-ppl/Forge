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

  // inline 'unquotes' calls as well in the library version
  // is this always the right thing to do? probably not! how do we know?
  // override def inline(o: Rep[DSLOp], str: Exp[String], quoter: Exp[Any] => String = quote) = {     
  //   var x = super.inline(o, str, quoter)
  //   val quotePattern = new Regex("""\"\+(.*?)\+\"""", "body")
  //   x = quotePattern replaceAllIn (x, m => m.group("body"))
  //   x
  // }
  
  override def quote(x: Exp[Any]): String = x match {    
    case Def(PrintLines(p, lines)) =>    
      // since this is called from emitWithIndent, the first line has an extra indent
      lines.map(l => (" "*4)+quote(l)).mkString(nl) 
      
    // case Def(QuoteSeq(i)) => "("+opArgPrefix+i+": _*)"  // not exactly a quoted sequence..
    case Def(QuoteSeq(i)) => /*"todo_gibbons4_17"*/ opArgPrefix+i
      
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
        emitWithIndent("val out = " + makeOpMethodName(dc.alloc) + makeTpePars(instTpePar(dc.alloc.tpePars, map.tpePars._1, map.tpePars._2)) + "(" + makeOpMethodNameWithNamedArgs(dc.size, List(o.args.apply(map.argIndex))) + ")", stream, indent)
        emitWithIndent("for (i <- 0 until " + makeOpMethodNameWithNamedArgs(dc.size, List(o.args.apply(map.argIndex))) + ") {", stream, indent)            
        emitWithIndent(makeOpMethodName(dc.update) + "(out, i, func(" + makeOpMethodName(dc.apply) + "(" + o.args.apply(map.argIndex)._1 + ", i)))", stream, indent+2)
        emitWithIndent("}", stream, indent)            
        emitWithIndent("out", stream, indent)                
      case zip:Zip =>
        check(o)          
        val dc = DeliteCollections(zip.tpePars._4)        
        emitWithIndent("def func: (" + quote(zip.tpePars._1) + "," + quote(zip.tpePars._2) + ") => " + quote(zip.tpePars._3) + " = " + inline(o, zip.func), stream, indent)            
        emitWithIndent("val out = " + makeOpMethodName(dc.alloc) + makeTpePars(instTpePar(dc.alloc.tpePars, zip.tpePars._1, zip.tpePars._3)) + "(" + makeOpMethodNameWithNamedArgs(dc.size, List(o.args.apply(zip.argIndices._1))) + ")", stream, indent)
        emitWithIndent("for (i <- 0 until " + makeOpMethodNameWithNamedArgs(dc.size, List(o.args.apply(zip.argIndices._1))) + ") {", stream, indent)            
        emitWithIndent(makeOpMethodName(dc.update) + "(out, i, func(" + makeOpMethodName(dc.apply) + "(" + o.args.apply(zip.argIndices._1)._1 + ", i)," + makeOpMethodName(dc.apply) + "(" + o.args.apply(zip.argIndices._2)._1 + ", i)))", stream, indent+2)
        emitWithIndent("}", stream, indent)            
        emitWithIndent("out", stream, indent)        
      case reduce:Reduce =>
        check(o)
        val dc = DeliteCollections(reduce.tpePars._2)        
        emitWithIndent("def func: (" + quote(reduce.tpePars._1) + "," + quote(reduce.tpePars._1) + ") => " + quote(reduce.tpePars._1) + " = " + inline(o, reduce.func), stream, indent)                    
        emitWithIndent("var acc = " + makeOpMethodNameWithArgs(reduce.zero), stream, indent)
        emitWithIndent("for (i <- 0 until " + makeOpMethodNameWithNamedArgs(dc.size, List(o.args.apply(reduce.argIndex))) + ") {", stream, indent)            
        emitWithIndent("acc = " + " func(acc, " + makeOpMethodName(dc.apply) + "(" + o.args.apply(reduce.argIndex)._1 + ", i))", stream, indent+2)
        emitWithIndent("}", stream, indent)            
        emitWithIndent("acc", stream, indent)                      
      case filter:Filter =>
        check(o)
        val dc = DeliteCollections(filter.tpePars._3)        
        emitWithIndent("def func: " + quote(filter.tpePars._1) + " => " + quote(filter.tpePars._2) + " = " + inline(o, filter.func), stream, indent)            
        emitWithIndent("def cond: " + quote(filter.tpePars._1) + " => " + quote(MBoolean) + " = " + inline(o, filter.cond), stream, indent)            
        emitWithIndent("val out = " + makeOpMethodName(dc.alloc) + makeTpePars(instTpePar(dc.alloc.tpePars, filter.tpePars._1, filter.tpePars._2)) + "(0)", stream, indent)
        emitWithIndent("for (i <- 0 until " + makeOpMethodNameWithNamedArgs(dc.size, List(o.args.apply(filter.argIndex))) + ") {", stream, indent)            
        emitWithIndent("if (cond(i)) {", stream, indent+2)
        emitWithIndent("// TODO: dc_append", stream, indent+4)
        // emitWithIndent(makeOpMethodName(dc.append) + "(out, func(" + makeOpMethodName(dc.apply) + "(" + NOPE NOPE NOPE opArgPrefix+map.argIndex + ", i)))", stream, indent+4)
        emitWithIndent("}", stream, indent+2)            
        emitWithIndent("}", stream, indent)            
        emitWithIndent("out", stream, indent)                      
      case foreach:Foreach =>
        check(o)
        val dc = DeliteCollections(foreach.tpePars._2)        
        emitWithIndent("def func: " + quote(foreach.tpePars._1) + " => " + quote(MUnit) + " = " + inline(o, foreach.func), stream, indent)            
        emitWithIndent("for (i <- 0 until " + makeOpMethodNameWithNamedArgs(dc.size, List(o.args.apply(foreach.argIndex))) + ") {", stream, indent)            
        emitWithIndent("func(" + makeOpMethodName(dc.apply) + "(" + o.args.apply(foreach.argIndex)._1 + ", i))", stream, indent+2)
        emitWithIndent("}", stream, indent)            
    }    
  }
  
  def emitClass(opsGrp: DSLOps, stream: PrintWriter) {
    if (grpIsTpe(opsGrp.grp)) {
      val tpe = grpAsTpe(opsGrp.grp)
      val d = DataStructs.find(_.tpe == tpe)
      d.foreach { data => 
        stream.print("class " + data.tpe.name)
        stream.print(makeTpeParsWithBounds(data.tpePars))
        stream.print("(")  
        stream.print(makeFieldArgs(data))
        stream.print(") {")
        stream.println()
        stream.println(makeFieldsWithInitArgs(data))
        for (o <- unique(opsGrp.ops) if o.style == infix) {       
          stream.print("  def " + o.name + makeTpeParsWithBounds(o.tpePars.drop(1)))
          stream.print("(" + o.args.map(t => t._1 + ": " + repify(t._2)).mkString(",") + ")") 
          stream.print(makeImplicitArgsWithCtxBoundsWithType(o.implicitArgs, o.tpePars, without = data.tpePars))
          stream.println(" = {")
          // cheat a little bit for consistency: the codegen rule may refer to this arg
          //emitWithIndent("val " + opArgPrefix + 0 + " = this", stream, 4) // cheating - TODO gibbons4
          emitOp(o, stream, indent=4)
          stream.println("  }")
        }
        stream.println("}")
        stream.println()
      }
      if (d.isEmpty && !isPrimitiveType(tpe)) {
        // what should we actually do here? let the user define a lib type in this case?
        warn("no data structure found for tpe " + tpe.name + ". emitting type alias " + quote(tpe) + " = scala." + quote(tpe) + ", which may be nonsense.")
        stream.println("  type " + quote(tpe) + " = scala." + quote(tpe))
      }
    }
    
    for (o <- unique(opsGrp.ops)) {       
      stream.print(" def " + makeOpMethodName(o) + makeTpeParsWithBounds(o.tpePars))
      stream.print(makeOpArgsWithType(o))
      stream.print(makeOpImplicitArgsWithOverloadWithType(o))
      stream.println(" = {")      
      o.style match {
        case `static` => emitOp(o, stream, indent=4)
        case `infix` if grpIsTpe(opsGrp.grp) && DataStructs.exists(_.tpe == grpAsTpe(opsGrp.grp)) => 
          val args = o.args.map(t => t._1).mkString(",")
          val argsWithParen = if (args == "") args else "(" + args + ")"
          emitWithIndent(o.args.apply(0)._1 + "." + o.name + argsWithParen, stream, 4)
        case `infix` => emitOp(o, stream, indent=4)
        case `direct` => emitOp(o, stream, indent=4)        
      }
      stream.println("  }")        
    }
    stream.println()    
        
  }
}

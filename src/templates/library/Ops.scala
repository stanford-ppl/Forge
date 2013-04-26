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
    // case Def(QuoteSeq(i)) => "("+i+": _*)"  // not exactly a quoted sequence..
    case Def(QuoteSeq(argName)) => argName      
    case Const(s: String) => replaceWildcards(s) // don't add quotes     
    case _ => super.quote(x)
  }  
  
  def emitImpls(opsGrp: DSLOps, stream: PrintWriter) {
    emitBlockComment("SingleTask and Composite Impls", stream)   
    stream.println()
    stream.println("trait " + opsGrp.grp.name + "WrapperImpl {")
    stream.println("  this: " + dsl + "Application with " + dsl + "CompilerOps => ")
    stream.println()    
    emitSingleTaskImplMethods(opsGrp, stream, 2)
    emitCompositeImplMethods(opsGrp, stream, 2)
    stream.println("}")
  }
      
  def emitOp(o: Rep[DSLOp], stream: PrintWriter, indent: Int = 0) {
    Impls(o) match {
      case codegen:CodeGen => 
        val rule = codegen.decls.getOrElse($cala, err("could not find Scala codegen rule for op: " + o.name))
        emitWithIndent(inline(o, rule.decl), stream, indent) 
      case Getter(structArgIndex,field) =>
        emitWithIndent(inline(o, quotedArg(structArgIndex)) + "." + field, stream, indent)
      case Setter(structArgIndex,field,value) =>
        emitWithIndent(inline(o, quotedArg(structArgIndex)) + "." + field + " = " + inline(o,value), stream, indent)
      case Allocates(tpe,init) =>        
        val initialVals = init.map(i => inline(o,i)).mkString(",")              
        emitWithIndent("new " + quote(tpe) + "(" + initialVals + ")", stream, indent)
      case single:SingleTask => 
        emitWithIndent(makeOpImplMethodNameWithArgs(o), stream, indent)
      case composite:Composite =>
        emitWithIndent(makeOpImplMethodNameWithArgs(o), stream, indent)
      case map:Map =>
        val dc = ForgeCollections(getHkTpe(o.retTpe))        
        emitWithIndent("def func: " + quote(map.tpePars._1) + " => " + quote(map.tpePars._2) + " = " + inline(o, map.func), stream, indent)            
        // TODO: this isn't quite right. how do we know which of dc.allocs tpePars is the one that corresponds to our return tpe, e.g. map.tpePars._2?
        emitWithIndent("val out = " + makeOpMethodName(dc.alloc) + makeTpePars(instTpePar(dc.alloc.tpePars, map.tpePars._1, map.tpePars._2)) + "(" + makeOpMethodName(dc.size) + "(" + o.args.apply(map.argIndex).name +")" + ")", stream, indent) // TODO - makeArg
        emitWithIndent("for (i <- 0 until " + makeOpMethodName(dc.size) + "(" + o.args.apply(map.argIndex).name + ")" + ") {", stream, indent)            
        emitWithIndent(makeOpMethodName(dc.update) + "(out, i, func(" + makeOpMethodName(dc.apply) + "(" + o.args.apply(map.argIndex).name + ", i)))", stream, indent+2)
        emitWithIndent("}", stream, indent)            
        emitWithIndent("out", stream, indent)                
      case zip:Zip =>
        val dc = ForgeCollections(getHkTpe(o.retTpe))        
        emitWithIndent("def func: (" + quote(zip.tpePars._1) + "," + quote(zip.tpePars._2) + ") => " + quote(zip.tpePars._3) + " = " + inline(o, zip.func), stream, indent)            
        emitWithIndent("val out = " + makeOpMethodName(dc.alloc) + makeTpePars(instTpePar(dc.alloc.tpePars, zip.tpePars._1, zip.tpePars._3)) + "(" + makeOpMethodName(dc.size) + "(" + o.args.apply(zip.argIndices._1).name + ")" + ")", stream, indent)
        emitWithIndent("for (i <- 0 until " + makeOpMethodName(dc.size) + "(" + o.args.apply(zip.argIndices._1).name + ")" + ") {", stream, indent)   
        emitWithIndent(makeOpMethodName(dc.update) + "(out, i, func(" + makeOpMethodName(dc.apply) + "(" + o.args.apply(zip.argIndices._1).name + ", i)," + makeOpMethodName(dc.apply) + "(" + o.args.apply(zip.argIndices._2).name + ", i)))", stream, indent+2)
        emitWithIndent("}", stream, indent)            
        emitWithIndent("out", stream, indent)        
      case reduce:Reduce =>
        val dc = ForgeCollections(reduce.tpePars._2)        
        emitWithIndent("def func: (" + quote(reduce.tpePars._1) + "," + quote(reduce.tpePars._1) + ") => " + quote(reduce.tpePars._1) + " = " + inline(o, reduce.func), stream, indent)                    
        emitWithIndent("var acc = " + makeOpMethodNameWithArgs(reduce.zero), stream, indent)
        emitWithIndent("for (i <- 0 until " + makeOpMethodName(dc.size) + "(" + o.args.apply(reduce.argIndex).name + ")" + ") {", stream, indent)            
        emitWithIndent("acc = " + " func(acc, " + makeOpMethodName(dc.apply) + "(" + o.args.apply(reduce.argIndex).name + ", i))", stream, indent+2)
        emitWithIndent("}", stream, indent)            
        emitWithIndent("acc", stream, indent)                      
      case filter:Filter =>
        val dc = ForgeCollections(getHkTpe(o.retTpe)).asInstanceOf[ParallelCollectionBuffer]        
        emitWithIndent("def func: " + quote(filter.tpePars._1) + " => " + quote(filter.tpePars._2) + " = " + inline(o, filter.func), stream, indent)            
        emitWithIndent("def cond: " + quote(filter.tpePars._1) + " => " + quote(MBoolean) + " = " + inline(o, filter.cond), stream, indent)            
        emitWithIndent("val out = " + makeOpMethodName(dc.alloc) + makeTpePars(instTpePar(dc.alloc.tpePars, filter.tpePars._1, filter.tpePars._2)) + "(0)", stream, indent)
        emitWithIndent("for (i <- 0 until " + makeOpMethodName(dc.size) + "(" + o.args.apply(filter.argIndex).name + ")"  + ") {", stream, indent)
        emitWithIndent("val e = " + makeOpMethodName(dc.apply) + "(" + opArgPrefix+filter.argIndex + ", i)", stream, indent)
        emitWithIndent("if (cond(e)) {", stream, indent+2)
        emitWithIndent(makeOpMethodName(dc.append) + "(out, i, func(e))", stream, indent+4)
        emitWithIndent("}", stream, indent+2)            
        emitWithIndent("}", stream, indent)            
        emitWithIndent("out", stream, indent)                      
      case foreach:Foreach =>
        val dc = ForgeCollections(foreach.tpePars._2)        
        emitWithIndent("def func: " + quote(foreach.tpePars._1) + " => " + quote(MUnit) + " = " + inline(o, foreach.func), stream, indent)            
        emitWithIndent("for (i <- 0 until " + makeOpMethodName(dc.size) + "(" + o.args.apply(foreach.argIndex).name + ")" + ") {", stream, indent)            
        emitWithIndent("func(" + makeOpMethodName(dc.apply) + "(" + o.args.apply(foreach.argIndex).name + ", i))", stream, indent+2)
        emitWithIndent("}", stream, indent)            
    }    
  }
  
  def emitClass(opsGrp: DSLOps, stream: PrintWriter) {
    if (grpIsTpe(opsGrp.grp)) {
      val tpe = grpAsTpe(opsGrp.grp)
      val d = DataStructs.get(tpe)
      d.foreach { data => 
        stream.println("class " + data.tpe.name + makeTpeParsWithBounds(data.tpe.tpePars) + "(" + makeFieldArgs(data) + ") {")
        stream.println(makeFieldsWithInitArgs(data))
        for (o <- unique(opsGrp.ops) if o.style == infix && o.args.apply(0).tpe == tpe) {       
          stream.print("  def " + o.name + makeTpeParsWithBounds(o.tpePars.drop(1)))
          //stream.print("(" + o.args/*.drop(1)*/.map(t => t.name + ": " + repify(t.tpe) + " = " + unit(t.default)).mkString(",") + ")") TODO 
          stream.print("(" + o.args.drop(1).map(t => argify(t, repify)).mkString(",") + ")")  
          stream.print(makeImplicitArgsWithCtxBoundsWithType(o.implicitArgs, o.tpePars, without = data.tpe.tpePars))
          stream.println(" = {")
          emitWithIndent("val " + o.args.apply(0).name + " = this", stream, 4)
          emitOp(o, stream, indent=4)
          stream.println("  }")
        }
        stream.println("}")
        stream.println()
      }
      if (d.isEmpty && !isForgePrimitiveType(tpe)) {
        warn("(library) no data structure found for tpe " + tpe.name + ". emitting empty class declaration") 
        stream.println("  class " + tpe.name + makeTpeParsWithBounds(tpe.tpePars) + "() {}")
        stream.println()
      }
    }
    
    for (o <- unique(opsGrp.ops)) {       
      stream.print("  def " + makeOpMethodName(o) + makeTpeParsWithBounds(o.tpePars))
      stream.print(makeOpArgsWithType(o))
      stream.print(makeOpImplicitArgsWithOverloadWithType(o))
      stream.println(" = {")      
      o.style match {
        case `static` => emitOp(o, stream, indent=4)
        case `infix` if grpIsTpe(opsGrp.grp) && DataStructs.contains(grpAsTpe(opsGrp.grp)) && o.args.apply(0).tpe == grpAsTpe(opsGrp.grp) => 
          //val args = o.args/*.drop(1)*/.map(t => t.name)).mkString(",")
          val args = o.args.drop(1).map(t => t.name).mkString(",")
          val argsWithParen = if (args == "") args else "(" + args + ")"
          emitWithIndent(o.args.apply(0).name + "." + o.name + argsWithParen, stream, 4)
        case `infix` | `direct` | `compiler` => emitOp(o, stream, indent=4)
      }
      stream.println("  }")        
    }
    stream.println()    
        
  }
}

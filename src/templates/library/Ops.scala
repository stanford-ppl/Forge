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
  
  def requiresImpl(o: Rep[DSLOp]) = Impls(o) match {
    case _:CodeGen => false
    case Getter(structArgIndex,field) => false
    case Setter(structArgIndex,field,value) => false
    case Allocates(tpe,init) => false
    case _ => true
  }
  
  def emitImpls(opsGrp: DSLOps, stream: PrintWriter) {
    emitBlockComment("SingleTask and Composite Impls", stream)   
    stream.println()
    stream.println("trait " + opsGrp.grp.name + "WrapperImpl {")
    stream.println("  this: " + dsl + "Application with " + dsl + "CompilerOps => ")
    stream.println()        
    val indent = 2
    for (o <- unique(opsGrp.ops) if requiresImpl(o)) { 
      emitWithIndent(makeOpImplMethodSignature(o) + " = {", stream, indent)      
      Impls(o) match {
        case single:SingleTask => 
          inline(o, single.func, quoteLiteral).split(nl).foreach { line => emitWithIndent(line, stream, indent+2 )}
        case composite:Composite =>
          inline(o, composite.func, quoteLiteral).split(nl).foreach { line => emitWithIndent(line, stream, indent+2 )}
        case map:Map =>
          val dc = ForgeCollections(getHkTpe(o.retTpe))        
          emitWithIndent("def func: " + repify(map.tpePars._1) + " => " + repify(map.tpePars._2) + " = " + inline(o, map.func), stream, indent+2)            
          emitWithIndent("val in = " + o.args.apply(map.argIndex).name, stream, indent+2)
          emitWithIndent("val out = " + makeOpMethodName(dc.alloc) + makeTpePars(instAllocReturnTpe(dc.alloc.tpePars, map.tpePars._2)) + "(in, " + makeOpMethodName(dc.size) + "(in)" + ")", stream, indent+2) // TODO - makeArg
          emitWithIndent("var i = 0", stream, indent+2)  
          emitWithIndent("while (i < " + makeOpMethodName(dc.size) + "(in)" + ") {", stream, indent+2)            
          emitWithIndent(makeOpMethodName(dc.update) + "(out, i, func(" + makeOpMethodName(dc.apply) + "(in, i)))", stream, indent+4)
          emitWithIndent("i += 1", stream, indent+4)
          emitWithIndent("}", stream, indent+2)            
          emitWithIndent("out", stream, indent+2)                
        case zip:Zip =>
          val dc = ForgeCollections(getHkTpe(o.retTpe))        
          emitWithIndent("def func: (" + repify(zip.tpePars._1) + "," + repify(zip.tpePars._2) + ") => " + repify(zip.tpePars._3) + " = " + inline(o, zip.func), stream, indent+2)            
          emitWithIndent("val inA = " + o.args.apply(zip.argIndices._1).name, stream, indent+2)
          emitWithIndent("val inB = " + o.args.apply(zip.argIndices._2).name, stream, indent+2)
          emitWithIndent("val out = " + makeOpMethodName(dc.alloc) + makeTpePars(instAllocReturnTpe(dc.alloc.tpePars, zip.tpePars._3)) + "(inA, " + makeOpMethodName(dc.size) + "(inA)" + ")", stream, indent+2)
          emitWithIndent("var i = 0", stream, indent+2)  
          emitWithIndent("while (i < " + makeOpMethodName(dc.size) + "(inA)" + ") {", stream, indent+2)   
          emitWithIndent(makeOpMethodName(dc.update) + "(out, i, func(" + makeOpMethodName(dc.apply) + "(inA, i)," + makeOpMethodName(dc.apply) + "(inB, i)))", stream, indent+4)
          emitWithIndent("i += 1", stream, indent+4)
          emitWithIndent("}", stream, indent+2)            
          emitWithIndent("out", stream, indent+2)        
        case reduce:Reduce =>
          val c = o.args.apply(reduce.argIndex)
          val dc = ForgeCollections(getHkTpe(c.tpe))        
          emitWithIndent("def func: (" + repify(reduce.tpePar) + "," + repify(reduce.tpePar) + ") => " + repify(reduce.tpePar) + " = " + inline(o, reduce.func), stream, indent+2)                    
          emitWithIndent("val in = " + c.name, stream, indent+2)
          emitWithIndent("var acc = " + makeOpMethodNameWithArgs(reduce.zero), stream, indent+2)
          emitWithIndent("var i = 0", stream, indent+2)  
          emitWithIndent("while (i < " + makeOpMethodName(dc.size) + "(in)" + ") {", stream, indent+2)            
          emitWithIndent("acc = " + " func(acc, " + makeOpMethodName(dc.apply) + "(in, i))", stream, indent+4)
          emitWithIndent("i += 1", stream, indent+4)
          emitWithIndent("}", stream, indent+2)            
          emitWithIndent("acc", stream, indent+2)                      
        case filter:Filter =>
          val dc = ForgeCollections(getHkTpe(o.retTpe)).asInstanceOf[ParallelCollectionBuffer]        
          emitWithIndent("def func: " + repify(filter.tpePars._1) + " => " + repify(filter.tpePars._2) + " = " + inline(o, filter.func), stream, indent+2)            
          emitWithIndent("def cond: " + repify(filter.tpePars._1) + " => " + repify(MBoolean) + " = " + inline(o, filter.cond), stream, indent+2)            
          emitWithIndent("val in = " + o.args.apply(filter.argIndex).name, stream, indent+2)
          emitWithIndent("val out = " + makeOpMethodName(dc.alloc) + makeTpePars(instAllocReturnTpe(dc.alloc.tpePars, filter.tpePars._2)) + "(in,0)", stream, indent+2)
          emitWithIndent("var i = 0", stream, indent+2)  
          emitWithIndent("while (i < " + makeOpMethodName(dc.size) + "(in)"  + ") {", stream, indent+2)
          emitWithIndent("val e = " + makeOpMethodName(dc.apply) + "(in, i)", stream, indent+4)
          emitWithIndent("if (cond(e)) {", stream, indent+4)
          emitWithIndent(makeOpMethodName(dc.append) + "(out, i, func(e))", stream, indent+6)
          emitWithIndent("}", stream, indent+4)            
          emitWithIndent("i += 1", stream, indent+4)
          emitWithIndent("}", stream, indent+2)            
          emitWithIndent("out", stream, indent+2)                      
        case foreach:Foreach =>
          val c = o.args.apply(foreach.argIndex)
          val dc = ForgeCollections(getHkTpe(c.tpe))        
          emitWithIndent("def func: " + repify(foreach.tpePar) + " => " + repify(MUnit) + " = " + inline(o, foreach.func), stream, indent+2)            
          emitWithIndent("val in = " + c.name, stream, indent+2)
          emitWithIndent("var i = 0", stream, indent+2)  
          emitWithIndent("while (i < " + makeOpMethodName(dc.size) + "(in)" + ") {", stream, indent+2)            
          emitWithIndent("func(" + makeOpMethodName(dc.apply) + "(in, i))", stream, indent+4)
          emitWithIndent("i += 1", stream, indent+4)
          emitWithIndent("}", stream, indent+2)             
      }
      emitWithIndent("}", stream, indent)
      stream.println()
    }            
    stream.println("}")
  }

  def emitOverloadShadows(o: Rep[DSLOp], stream: PrintWriter, indent: Int = 0) {
    val i = nameClashId(o)
    if (i != "") emitWithIndent("def " + implicitOpArgPrefix + o.implicitArgs.length + " = ()", stream, indent)  
  }
  
  def emitOp(o: Rep[DSLOp], stream: PrintWriter, indent: Int = 0) {
    Impls(o) match {
      case codegen:CodeGen => 
        val rule = codegen.decls.getOrElse($cala, err("could not find Scala codegen rule for op: " + o.name))
        inline(o, rule.decl, quoteLiteral).split(nl).foreach { line => emitWithIndent(line, stream, indent) }      
      case Getter(structArgIndex,field) =>
        emitOverloadShadows(o, stream, indent)
        emitWithIndent(inline(o, quotedArg(o.args.apply(structArgIndex).name)) + "." + field, stream, indent)
      case Setter(structArgIndex,field,value) =>
        emitOverloadShadows(o, stream, indent)
        emitWithIndent(inline(o, quotedArg(o.args.apply(structArgIndex).name)) + "." + field + " = " + inline(o,value), stream, indent)
      case Allocates(tpe,init) =>        
        emitOverloadShadows(o, stream, indent)
        val initialVals = init.map(i => inline(o,i)).mkString(",")              
        emitWithIndent("new " + quote(tpe) + "(" + initialVals + ")", stream, indent)
      case _ => emitWithIndent(makeOpImplMethodNameWithArgs(o), stream, indent)
    }
  }
  
  def emitClass(opsGrp: DSLOps, stream: PrintWriter) {
    if (grpIsTpe(opsGrp.grp)) {
      val tpe = grpAsTpe(opsGrp.grp)
      val d = DataStructs.get(tpe)
      d.foreach { data => 
        stream.println("class " + data.tpe.name + makeTpeParsWithBounds(data.tpe.tpePars) + "(" + makeFieldArgs(data) + ") {")
        stream.println(makeFieldsWithInitArgs(data))
        // Actually emitting the infix methods as instance methods, while a little more readable, makes the interpreter methods
        // ambiguous with the op conversions unless they already exist on every instance and must be overridden (e.g. toString)
        for (o <- unique(opsGrp.ops) if overrideList.contains(o.name) && o.style == infixMethod && o.args.length > 0 && quote(o.args.apply(0).tpe) == quote(tpe)) {       
          stream.print("  "+makeDefWithOverride(o)+" " + o.name + makeTpeParsWithBounds(o.tpePars.drop(1)))
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
      stream.println("  " + makeOpMethodSignature(o) + " = {")
      o.style match {
        case `infixMethod` if overrideList.contains(o.name) && grpIsTpe(opsGrp.grp) && DataStructs.contains(grpAsTpe(opsGrp.grp)) && o.args.length > 1 && quote(o.args.apply(0).tpe) == quote(opsGrp.grp) => 
          //val args = o.args/*.drop(1)*/.map(t => t.name)).mkString(",")
          val args = o.args.drop(1).map(t => t.name).mkString(",")
          val argsWithParen = if (args == "") args else "(" + args + ")"
          emitWithIndent(o.args.apply(0).name + "." + o.name + argsWithParen, stream, 4)
        case _ => emitOp(o, stream, indent=4)
      }
      stream.println("  }")        
    }
    stream.println()    
        
  }
}

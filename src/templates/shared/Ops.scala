package ppl.dsl.forge
package templates
package shared

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._
import core._
import Utilities._

trait BaseGenOps extends ForgeCodeGenBase {  
  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  /**
   * Utility methods
   */  
  def baseOpsCls(grp: Rep[DSLGroup]) = {
    if (OpsGrp(grp).ops.exists(o => nameClashesUniversal(o).length > 1)) "Base with OverloadHack"
    else "Base"
  }
  
  /**
   * Quoting for formatted code-gen
   */  
    
   def inline(o: Rep[DSLOp], str: Exp[String], quoter: Exp[Any] => String = quote) = {     
     var b = quoter(str)
     //for (i <- 0 until o.args.length) {
     //  b = b.replaceAllLiterally(quoter(o.quotedArg(i)), opArgPrefix + i)
     //}    
     for (i <- 0 until o.tpePars.length) {
       b = b.replaceAllLiterally(quoter(o.tpeInstance(i)), o.tpePars.apply(i).name)
     }    
     b
   }  
      
   def replaceWildcards(s: String) = {
     // currently only 1 wildcard
     s.replaceAll(qu, "\"")
   }
   
   override def quote(x: Exp[Any]) : String = x match {
     case Def(QuoteBlockResult(name,List(byName),ret)) => name
     case Def(QuoteBlockResult(name,args,ret)) => name + makeArgs(args)
     case _ => super.quote(x)
   }  
  
  // used with overloading
  // if after converting Ts and Vars to Reps there are duplicates, remove them   
  def unique(ops: List[Rep[DSLOp]]): List[Rep[DSLOp]] = {
    val filtered = scala.collection.mutable.ArrayBuffer[Rep[DSLOp]]()
    val canonical = scala.collection.mutable.HashSet[String]()
    
    // add to filtered only if canonical version doesn't already exist
    for (o <- ops) {
      val t = o.name + makeOpArgsWithType(o) + makeOpImplicitArgs(o)
      if (!canonical.contains(t)) {
        filtered += o
        canonical += t
      }
    }
    filtered.toList
  }
    
  // normal args
  def simpleArgName(t: (String, Rep[DSLType])): String = t._1
  def makeArgs(args: List[(String, Rep[DSLType])], makeArgName: ((String, Rep[DSLType])) => String = simpleArgName) = "(" + args.map(makeArgName).mkString(",") + ")"
  def makeOpArgs(o: Rep[DSLOp]) = makeArgs(o.args)
  def makeOpFutureArgs(o: Rep[DSLOp]) = makeArgs(o.args, t => { val arg = t._1; if (t._2.stage == now) "unit("+arg+")" else arg })
  def makeOpArgsWithType(o: Rep[DSLOp], typify: Rep[DSLType] => String = repify) = makeArgs(o.args, t => t._1 + ": " + typify(t._2))
  def makeOpArgsWithNowType(o: Rep[DSLOp]) = makeOpArgsWithType(o, repifySome)
  
  // untyped implicit args
  def makeImplicitCtxBounds(tpePars: List[Rep[TypePar]]) = {
    tpePars.flatMap(a => a.ctxBounds.map(b => "implicitly["+b.name+"["+quote(a)+"]]")).mkString(",")
  }  
  def makeOpImplicitCtxBounds(o: Rep[DSLOp]) = makeImplicitCtxBounds(o.tpePars)    
  def makeImplicitArgs(implicitArgs: List[Rep[DSLType]], ctxBoundsStr: String = "") = {
    // ctxBounds must come before regular implicits
    val ctxBounds2 = if (ctxBoundsStr == "") "" else ctxBoundsStr+","
    if (implicitArgs.length > 0) "(" + ctxBounds2 + implicitArgs.zipWithIndex.map(t => implicitOpArgPrefix + t._2).mkString(",") + ")"
    else ""    
  }
  def makeOpImplicitArgs(o: Rep[DSLOp]) = makeImplicitArgs(o.implicitArgs, makeOpImplicitCtxBounds(o)) // explicitly passing implicits requires passing ctxBounds, too
  def makeOpImplicitArgsWithOverload(o: Rep[DSLOp], asVals: Boolean = false) = makeImplicitArgs(implicitArgsWithOverload(o), makeOpImplicitCtxBounds(o))
  
  // typed implicit args with context bounds (only needed for instance methods)
  // 'without' is used to subtract bounds that are already in scope
  def implicitCtxBoundsWithType(tpePars: List[Rep[TypePar]], without: List[Rep[TypePar]] = Nil) = {
    val withoutBounds = without.flatMap(a => a.ctxBounds)
    tpePars.flatMap(a => a.ctxBounds.diff(withoutBounds).map(b => tpe(b.name+"["+quote(a)+"]"))).distinct // a little hacky to use tpe like this
  }  
  def makeImplicitArgsWithCtxBoundsWithType(implicitArgs: List[Rep[DSLType]], tpePars: List[Rep[TypePar]], without: List[Rep[TypePar]] = Nil, asVals: Boolean = false) = {
    makeImplicitArgsWithType(implicitCtxBoundsWithType(tpePars, without) ++ implicitArgs, asVals)    
  }
  
  // typed implicit args without context bounds
  def makeImplicitArgsWithType(implicitArgs: List[Rep[DSLType]], asVals: Boolean = false) = {
    val prefix = if (asVals == true) "val " else ""
    if (implicitArgs.length > 0) "(implicit " + implicitArgs.zipWithIndex.map(t => prefix + implicitOpArgPrefix + t._2 + ": " + t._1.name).mkString(",") + ")"
    else ""    
  } 
  def makeOpImplicitArgsWithType(o: Rep[DSLOp], asVals: Boolean = false) = makeImplicitArgsWithType(o.implicitArgs, asVals)
  def makeOpImplicitArgsWithOverloadWithType(o: Rep[DSLOp], asVals: Boolean = false) = makeImplicitArgsWithType(implicitArgsWithOverload(o), asVals)
  
  // overload name clash resolution using implicit hack
  def nameClashesGrp(o: Rep[DSLOp]) = OpsGrp(o.grp).ops.filter(_.name == o.name) 
  def nameClashesUniversal(o: Rep[DSLOp]) = OpsGrp.values.toList.flatMap(g => g.ops.filter(_.name == o.name))
  def nameClashId(o: Rep[DSLOp]) = {
    val clashes = nameClashesUniversal(o)
    if (clashes.length > 1) (clashes.indexOf(o)+1).toString else ""    
  }
  def implicitArgsWithOverload(o: Rep[DSLOp]) = {    
    val i = nameClashId(o)
    if (i != "") {
      o.implicitArgs :+ tpe("Overloaded" + i)
    }
    else {
      o.implicitArgs
    }
  }      
  def needDisambiguate(o: Rep[DSLOp], clasher: Rep[DSLOp] => List[Rep[DSLOp]] = nameClashesGrp) = {
    // TODO: when do we need to disambiguate implicit args? (explicitly pass them). the fundamental reason is if there are
    // multiple possible implicits in scope.. how do we know? should we just always pass them explicitly? (this could improve staging performance, too)
     
    // the issue with nameClashes is that if the receiver method (e.g. string_+) also requires overload implicits, the ones from OverloadHack are also in scope
    // we should be able to solve this by de-prioritizing the OverloadHack vals somehow
    (clasher(o).length > 1 && unique(OpsGrp(o.grp).ops).contains(o)) // inefficient!! -> should refactor things so that we can store this with the op when it is created.
  }
  
  // method names
  def makeFullArgs(o: Rep[DSLOp], makeArgs: Rep[DSLOp] => String, clasher: Rep[DSLOp] => List[Rep[DSLOp]] = nameClashesGrp) = {
    makeTpePars(o.tpePars) + makeArgs(o) + (if (needDisambiguate(o,clasher)) makeOpImplicitArgsWithOverload(o) else "")  
  }
  def makeOpMethodName(o: Rep[DSLOp]) = {
    // adding the nameClashId is another way to avoid chaining the Overload implicit, but the weird method names that result are confusing
    val i = "" //nameClashId(o)    
    o.style match {
      case `static` => o.grp.name.toLowerCase + i + "_object_" + o.name.toLowerCase
      case _ => o.grp.name.toLowerCase + i + "_" + o.name.toLowerCase
    }
  }
  def makeOpMethodNameWithArgs(o: Rep[DSLOp]) = makeOpMethodName(o) + makeFullArgs(o, makeOpArgs)
  def makeOpMethodNameWithFutureArgs(o: Rep[DSLOp]) = makeOpMethodName(o) + makeFullArgs(o, makeOpFutureArgs, nameClashesUniversal) // front-end can name clash with anything
  def makeOpMethodSignature(o: Rep[DSLOp]) = "def " + makeOpMethodName(o) + makeTpeParsWithBounds(o.tpePars) + makeOpArgsWithType(o) + makeOpImplicitArgsWithOverloadWithType(o)
  def makeSyntaxMethod(o: Rep[DSLOp]) = {
    "def " + o.name + makeTpeParsWithBounds(o.tpePars) + makeOpArgsWithNowType(o) + makeOpImplicitArgsWithOverloadWithType(o) + " = " + makeOpMethodNameWithFutureArgs(o)
  }
  
  def makeOpImplMethodName(o: Rep[DSLOp]) = makeOpMethodName(o) + "_impl"
  def makeOpImplMethodNameWithArgs(o: Rep[DSLOp]) = makeOpImplMethodName(o) + nameClashId(o) + makeTpePars(o.tpePars) + makeOpArgs(o) + makeOpImplicitArgs(o)
  def makeOpImplMethodSignature(o: Rep[DSLOp]) = "def " + makeOpImplMethodName(o) + nameClashId(o) + makeTpeParsWithBounds(o.tpePars) + makeOpArgsWithType(o) + makeOpImplicitArgsWithType(o)  
  
  /**
   * Delite op sanity checking
   * 
   * These are mostly repetitive right now, but we could specialize the error checking more (or generalize it to be more concise).
   */
  def check(o: Rep[DSLOp]) { 
    o.opTpe match {
      case single:SingleTask => // nothing to check
      case map:Map =>
        val col = o.args.apply(map.argIndex)._2
        if (DeliteCollections.get(col).isEmpty) err("map argument " + col.name + " is not a DeliteCollection")
        if (map.tpePars.productIterator.exists(a => a.isInstanceOf[TypePar] && !o.tpePars.contains(a))) err("map op with undefined type arg: " + o.name)
        if (map.argIndex < 0 || map.argIndex > o.args.length) err("map op with illegal arg parameter: " + o.name)
      case zip:Zip =>
        val colA = o.args.apply(zip.argIndices._1)._2
        val colB = o.args.apply(zip.argIndices._2)._2
        if (DeliteCollections.get(colA).isEmpty) err("zip argument " + colA.name + " is not a DeliteCollection")
        if (DeliteCollections.get(colB).isEmpty) err("zip argument " + colB.name + " is not a DeliteCollection")
        if (zip.tpePars.productIterator.exists(a => a.isInstanceOf[TypePar] && !o.tpePars.contains(a))) err("zipWith op with undefined type arg: " + o.name)
        if (zip.argIndices.productIterator.asInstanceOf[Iterator[Int]].exists(a => a < 0 || a > o.args.length)) err("zipWith op with illegal arg parameter: " + o.name)
      case reduce:Reduce =>
        val col = o.args.apply(reduce.argIndex)._2
        if (DeliteCollections.get(col).isEmpty) err("reduce argument " + col.name + " is not a DeliteCollection")
        if (reduce.tpePars.productIterator.exists(a => a.isInstanceOf[TypePar] && !o.tpePars.contains(a))) err("reduce op with undefined type arg: " + o.name)
        if (reduce.argIndex < 0 || reduce.argIndex > o.args.length) err("reduce op with illegal arg parameter: " + o.name)        
        if (reduce.zero.retTpe != reduce.tpePars._1) err("reduce op with illegal zero parameter: " + o.name)
      case filter:Filter =>
        val col = o.args.apply(filter.argIndex)._2
        if (DeliteCollections.get(col).isEmpty) err("filter argument " + col.name + " is not a DeliteCollection")
        if (filter.tpePars.productIterator.exists(a => a.isInstanceOf[TypePar] && !o.tpePars.contains(a))) err("filter op with undefined type arg: " + o.name)
        if (filter.argIndex < 0 || filter.argIndex > o.args.length) err("filter op with illegal arg parameter: " + o.name)      
      case foreach:Foreach =>
        val col = o.args.apply(foreach.argIndex)._2
        if (DeliteCollections.get(col).isEmpty) err("foreach argument " + col.name + " is not a DeliteCollection")
        if (foreach.tpePars.productIterator.exists(a => a.isInstanceOf[TypePar] && !o.tpePars.contains(a))) err("foreach op with undefined type arg: " + o.name)
        if (foreach.argIndex < 0 || foreach.argIndex > o.args.length) err("foreach op with illegal arg parameter: " + o.name)      
    }
  }
  
  /**
   * Front-end codegen
   */
  def emitOpSyntax(opsGrp: DSLOps, stream: PrintWriter) {
    emitBlockComment("Operations", stream)
    stream.println()
    stream.println("trait " + opsGrp.name + " extends " + baseOpsCls(opsGrp.grp) + " {")
    stream.println("  this: " + dsl + " => ")
    stream.println()
        
    // static ops
    val staticOps = opsGrp.ops.filter(e=>e.style==static)
    if (staticOps.length > 0) {
      stream.println("  object " + opsGrp.grp.name + " {")
      for (o <- staticOps) {        
        stream.println("    " + makeSyntaxMethod(o))
      }
      stream.println("  }")
    }
    stream.println()
    
    // direct ops
    val directOps = opsGrp.ops.filter(e=>e.style==direct)
    for (o <- directOps) {
      stream.println("  " + makeSyntaxMethod(o))
    }
    
    // infix ops
    val infixOps = opsGrp.ops.filter(e=>e.style==infix)
    
    // certain ops (e.g. "apply" cannot be expressed with infix notation right now), so we use implicits as a workaround
    val needPimpClass = (noInfixList intersect infixOps.map(_.name)).nonEmpty
    val pimpStream = new StringBuilder()
    def infix_appendLine(x: StringBuilder, y: String) = x.append(y + System.getProperty("line.separator"))
    if (needPimpClass) {
      // set up a pimp-my-library style promotion
      // can only do this on DSL types
      val tpe = grpAsTpe(opsGrp.grp)                
      pimpStream.appendLine("  implicit def repTo" + tpe.name + "Ops" + makeTpeParsWithBounds(tpe.tpePars) + "(x: " + repify(tpe) + ") = new " + tpe.name + "OpsCls(x)")
      if (OpsGrp.exists(g => g._2.ops.exists(o => o.name == "__newVar"))) {
        pimpStream.appendLine("  implicit def varTo" + tpe.name + "Ops" + makeTpeParsWithBounds(tpe.tpePars) + "(x: " + varify(tpe) + ") = new " + tpe.name + "OpsCls(readVar(x))")
      }
      pimpStream.appendLine("")
      pimpStream.appendLine("  class " + tpe.name + "OpsCls" + makeTpeParsWithBounds(tpe.tpePars) + "(val " + opArgPrefix + "0: " + repify(tpe) + ") {") // not sure about this one
    }
    
    for (o <- infixOps) {
      if (noInfixList.contains(o.name)) {
        val tpe = grpAsTpe(opsGrp.grp)
        val otherArgs = "(" + o.args.map(t => t._1 + ": " + repifySome(t._1)).mkString(",") + ")"
        pimpStream.appendLine("    def " + o.name + makeTpeParsWithBounds(o.tpePars.diff(tpe.tpePars)) + otherArgs
          + (makeImplicitArgsWithCtxBoundsWithType(implicitArgsWithOverload(o), o.tpePars, without = tpe.tpePars)) + " = " + makeOpMethodNameWithFutureArgs(o))
      }
      else {
        stream.print("  def infix_" + o.name + makeTpeParsWithBounds(o.tpePars))
        stream.print(makeOpArgsWithNowType(o))
        stream.print(makeOpImplicitArgsWithOverloadWithType(o))
        stream.println(" = " + makeOpMethodNameWithFutureArgs(o))        
      }
    }
    stream.println()      
    
    if (needPimpClass) {
      pimpStream.appendLine("  }")
      stream.println(pimpStream)
    }
    
    // abstract methods
    for (o <- unique(opsGrp.ops)) {
      stream.println("  " + makeOpMethodSignature(o) + ": " + repify(o.retTpe))
    }
    
    stream.println("}")
  }
}

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
     for (i <- 0 until o.args.length) {
       val name = o.args.apply(i).name 
       b = b.replaceAllLiterally(quoter(o.quotedArg(name)), name) 
       // b = b.replaceAllLiterally(quoter(o.quotedArg(i)), name) // TODO eliminate need for this
     }    
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
     case Def(PrintLines(p, lines)) =>    
       // since this is called from emitWithIndent, the first line has an extra indent
       // without saving this value in the dynamic scope, we don't know what the indent was though.. guess '4' for now
       quote(lines(0)) + nl + lines.drop(1).map(l => (" "*4)+quote(l)).mkString(nl)      
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
  /*
  def simpleArgName(t: Rep[DSLArg]): String = t match {
    case Def(Arg(name, _, _)) => name
    case _ => err("unnamed Simple arg")
  }
  */
  def simpleArgName(t: Rep[DSLArg]): String = t.name
  def makeArgs(args: List[Rep[DSLArg]], makeArgName: (Rep[DSLArg] => String) = simpleArgName) = "(" + args.map(makeArgName).mkString(",") + ")"
  def makeOpArgs(o: Rep[DSLOp]) = makeArgs(o.args)
  def makeOpAnonArgs(o: Rep[DSLOp]) = makeArgs(o.args.zipWithIndex.map{ case (a,i) => arg(opArgPrefix + i, a.tpe, a.default) })
  def makeOpFutureArgs(o: Rep[DSLOp]) = makeArgs(o.args, t => { val arg = simpleArgName(t); if (t.tpe.stage == now) "unit("+arg+")" else arg })
  def makeOpArgsWithType(o: Rep[DSLOp], typify: Rep[DSLType] => String = repify) = makeArgs(o.args, t => argify(t, typify))
  def makeOpArgsWithNowType(o: Rep[DSLOp]) = makeOpArgsWithType(o, repifySome)
  def makeOpArgsWithDefaults(o: Rep[DSLOp], typify: Rep[DSLType] => String = repify) = makeArgs(o.args, t => argify(t, typify))
  def makeOpArgsWithNowDefault(o: Rep[DSLOp]) = makeOpArgsWithDefaults(o, repifySome)

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
    tpePars.flatMap(a => a.ctxBounds.diff(withoutBounds).map(b => ephemeralTpe(b.name+"["+quote(a)+"]"))).distinct 
  }  
  def makeImplicitArgsWithCtxBoundsWithType(implicitArgs: List[Rep[DSLType]], tpePars: List[Rep[TypePar]], without: List[Rep[TypePar]] = Nil, asVals: Boolean = false) = {
    makeImplicitArgsWithType(implicitArgs ++ implicitCtxBoundsWithType(tpePars, without), asVals)    
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
      o.implicitArgs :+ ephemeralTpe("Overloaded" + i)
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
  def specialCharacters = scala.collection.immutable.Map("=" -> "eq", "<" -> "lt", ">" -> "gt")
  def sanitize(x: String) = {
    var out = x
    specialCharacters.keys.foreach { k => if (x.contains(k)) out = out.replace(k, specialCharacters(k)) }
    out
  }
  
  def makeFullArgs(o: Rep[DSLOp], makeArgs: Rep[DSLOp] => String, clasher: Rep[DSLOp] => List[Rep[DSLOp]] = nameClashesGrp) = {
    makeTpePars(o.tpePars) + makeArgs(o) + (if (needDisambiguate(o,clasher)) makeOpImplicitArgsWithOverload(o) else "")  
  }
  def makeOpMethodName(o: Rep[DSLOp]) = {
    // adding the nameClashId is another way to avoid chaining the Overload implicit, but the weird method names that result are confusing
    val i = "" //nameClashId(o)    
    o.style match {
      case `static` => o.grp.name.toLowerCase + i + "_object_" + sanitize(o.name).toLowerCase
      case `compiler` => 
        if (o.name != sanitize(o.name)) err("compiler op name has special characters that require reformatting: " + o.name)
        o.name // should be callable directly from impl code
      case _ => o.grp.name.toLowerCase + i + "_" + sanitize(o.name).toLowerCase
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
  def makeOpImplMethodSignature(o: Rep[DSLOp]) = "def " + makeOpImplMethodName(o) + nameClashId(o) + makeTpeParsWithBounds(o.tpePars) + makeOpArgsWithType(o) + makeOpImplicitArgsWithType(o) + ": " + repify(o.retTpe) 
  
  /**
   * Op sanity checking
   * 
   * These are mostly repetitive right now, but we could specialize the error checking more (or generalize it to be more concise).
   */
  def check(o: Rep[DSLOp]) { 
    DeliteRules(o) match {
      case Def(Allocates(data,init)) =>
        if ((data.fields.map(_._1).diff(init.keys.toSeq)).length > 0) {
          err("allocator " + o.name + " does not have the same fields as data definition for " + data.tpe.name)
        }
      case Def(Getter(structArgIndex,field)) =>
        if (structArgIndex > o.args.length) err("arg index " + structArgIndex + " does not exist for op " + o.name)
        val struct = o.args.apply(structArgIndex).tpe
        val data = DataStructs.find(_.tpe == struct)
        if (data.isEmpty) err("no struct definitions found for arg index " + structArgIndex + " in op " + o.name)
        if (!data.get.fields.map(_.name).contains(field)) err("struct arg " + structArgIndex + " does not contain field " + field + " in op " + o.name)
      case Def(Setter(structArgIndex,field,value)) =>
        if (structArgIndex > o.args.length) err("arg index " + structArgIndex + " does not exist for op " + o.name)
        val struct = o.args.apply(structArgIndex).tpe
        val data = DataStructs.find(_.tpe == struct)
        if (data.isEmpty) err("no struct definitions found for arg index " + structArgIndex + " in op " + o.name)
        if (!data.get.fields.map(_.name).contains(field)) err("struct arg " + structArgIndex + " does not contain field " + field + " in op " + o.name)        
      case Def(Map(tpePars, argIndex, func)) =>
        val col = o.args.apply(argIndex).tpe
        if (DeliteCollections.get(col).isEmpty) err("map argument " + col.name + " is not a DeliteCollection")
        if (tpePars.productIterator.exists(a => a.isInstanceOf[TypePar] && !o.tpePars.contains(a))) err("map op with undefined type arg: " + o.name)
        if (argIndex < 0 || argIndex > o.args.length) err("map op with illegal arg parameter: " + o.name)
      case Def(Zip(tpePars, argIndices, func)) =>
        val colA = o.args.apply(argIndices._1).tpe
        val colB = o.args.apply(argIndices._2).tpe
        if (DeliteCollections.get(colA).isEmpty) err("zip argument " + colA.name + " is not a DeliteCollection")
        if (DeliteCollections.get(colB).isEmpty) err("zip argument " + colB.name + " is not a DeliteCollection")
        if (tpePars.productIterator.exists(a => a.isInstanceOf[TypePar] && !o.tpePars.contains(a))) err("zipWith op with undefined type arg: " + o.name)
        if (argIndices.productIterator.asInstanceOf[Iterator[Int]].exists(a => a < 0 || a > o.args.length)) err("zipWith op with illegal arg parameter: " + o.name)
      case Def(Reduce(tpePars, argIndex, zero, func)) =>
        val col = o.args.apply(argIndex).tpe
        if (DeliteCollections.get(col).isEmpty) err("reduce argument " + col.name + " is not a DeliteCollection")
        if (tpePars.isInstanceOf[TypePar] && !o.tpePars.contains(tpePars)) err("reduce op with undefined type arg: " + o.name)
        if (argIndex < 0 || argIndex > o.args.length) err("reduce op with illegal arg parameter: " + o.name)        
        if (zero.retTpe != tpePars) err("reduce op with illegal zero parameter: " + o.name)
      case Def(Filter(tpePars, argIndex, cond, func)) =>
        val col = o.args.apply(argIndex).tpe
        if (DeliteCollections.get(col).isEmpty || !DeliteCollections.get(col).forall(_.isInstanceOf[DeliteCollectionBuffer])) err("filter argument " + col.name + " is not a DeliteCollectionBuffer")
        if (tpePars.productIterator.exists(a => a.isInstanceOf[TypePar] && !o.tpePars.contains(a))) err("filter op with undefined type arg: " + o.name)
        if (argIndex < 0 || argIndex > o.args.length) err("filter op with illegal arg parameter: " + o.name)      
      case Def(Foreach(tpePars, argIndex, func)) =>
        val col = o.args.apply(argIndex).tpe
        if (DeliteCollections.get(col).isEmpty) err("foreach argument " + col.name + " is not a DeliteCollection")
        if (tpePars.isInstanceOf[TypePar] && !o.tpePars.contains(tpePars)) err("foreach op with undefined type arg: " + o.name)
        if (argIndex < 0 || argIndex > o.args.length) err("foreach op with illegal arg parameter: " + o.name)      
      case _ => // nothing to check
    }
  }
  
  /**
   * Common implementations for Single tasks and Composites
   */    
  def emitImplMethod(o: Rep[DSLOp], func: Rep[String], stream: PrintWriter, indent: Int = 0) {
    emitWithIndent(makeOpImplMethodSignature(o) + " = {", stream, indent)
    emitWithIndent(inline(o, func, quoteLiteral), stream, indent+2)
    emitWithIndent("}", stream, indent)
    stream.println()    
  }
  def emitSingleTaskImplMethods(opsGrp: DSLOps, stream: PrintWriter, indent: Int = 0) {
    for (o <- unique(opsGrp.ops)) { 
      DeliteRules(o) match {
        case Def(SingleTask(func)) => emitImplMethod(o,func,stream,indent)
        case _ =>
      }
    }    
  }  
  def emitCompositeImplMethods(opsGrp: DSLOps, stream: PrintWriter, indent: Int = 0) {
    for (o <- unique(opsGrp.ops)) { 
      DeliteRules(o) match {
        case Def(Composite(func)) => emitImplMethod(o,func,stream,indent)
        case _ =>
      }
    }    
  }
  
   
  /**
   * Front-end codegen
   */
  
  def checkOps(opsGrp: DSLOps) {
    for (o <- unique(opsGrp.ops)) check(o)
  }
  
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
    val allInfixOps = opsGrp.ops.filter(e=>e.style==infix)
    
    // certain ops (e.g. "apply" cannot be expressed with infix notation right now), so we use implicits as a workaround
    def noInfix(o: Rep[DSLOp]) = {
      // blacklist
      if (noInfixList.contains(o.name)) true
      else (o.args.exists { a => a.tpe match {
        // infix with function args doesn't always resolve correctly
        case Def(FTpe(fargs,fret,freq)) => true
        case _ => false
      }})
    }
    
    val (pimpOps, infixOps) = allInfixOps.partition(noInfix)
    if (pimpOps.nonEmpty) {
      // set up a pimp-my-library style promotion
      // can only do this on DSL types
      val tpe = grpAsTpe(opsGrp.grp)                
      stream.println("  implicit def repTo" + tpe.name + "Ops" + makeTpeParsWithBounds(tpe.tpePars) + "(x: " + repify(tpe) + ") = new " + tpe.name + "OpsCls(x)")
      if (OpsGrp.exists(g => g._2.ops.exists(o => o.name == "__newVar"))) {
        stream.println("  implicit def varTo" + tpe.name + "Ops" + makeTpeParsWithBounds(tpe.tpePars) + "(x: " + varify(tpe) + ") = new " + tpe.name + "OpsCls(readVar(x))")
      }
      stream.println("")
      stream.println("  class " + tpe.name + "OpsCls" + makeTpeParsWithBounds(tpe.tpePars) + "(val " + opArgPrefix + "0: " + repify(tpe) + ") {")
    
      for (o <- pimpOps) {
        val tpe = grpAsTpe(opsGrp.grp)
        val otherArgs = "(" + o.args.drop(1).map(t => t.name + ": " + repifySome(t.tpe) /*+ " = " + unit(t.default)*/).mkString(",") + ")" // TODO
        stream.println("    def " + o.name + makeTpeParsWithBounds(o.tpePars.filter(p => tpe.tpePars.contains(p.name))) + otherArgs
          + (makeImplicitArgsWithCtxBoundsWithType(implicitArgsWithOverload(o), o.tpePars, without = tpe.tpePars)) + " = " + makeOpMethodNameWithFutureArgs(o))          
      }        
      stream.println("  }")
    }    
    stream.println()
    
    for (o <- infixOps) {
      stream.print("  def infix_" + o.name + makeTpeParsWithBounds(o.tpePars))
      stream.print(makeOpArgsWithNowType(o))
      stream.print(makeOpImplicitArgsWithOverloadWithType(o))
      stream.println(" = " + makeOpMethodNameWithFutureArgs(o))        
    }
    stream.println()      
    
    
    // abstract methods
    for (o <- unique(opsGrp.ops.filter(e=>e.style != compiler))) {
      stream.println("  " + makeOpMethodSignature(o) + ": " + repify(o.retTpe))
    }
    
    stream.println("}")
    
    // compiler ops
    val compilerOps = opsGrp.ops.filter(e=>e.style == compiler)
    if (!compilerOps.isEmpty) {
      stream.println("trait " + opsGrp.grp.name + "CompilerOps extends " + opsGrp.name + " {")
      stream.println("  this: " + dsl + " => ")
      stream.println()    
      for (o <- compilerOps) {
        stream.println("  " + makeOpMethodSignature(o) + ": " + repify(o.retTpe))
      }      
      stream.println("}")
      stream.println()
    }
  }
}

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
  def validTpePar(o: Rep[DSLOp], tpePar: Rep[DSLType]) = tpePar match {
    case Def(TpePar(name,_)) => o.tpePars.exists(_.name == name)
    case _ => true
  }
  def check(o: Rep[DSLOp]) {
    if (!Impls.contains(o)) err("op " + o.name + " has no impl") 
    Impls(o) match {
      case Allocates(tpe,init) =>
        if (!DataStructs.contains(tpe)) err("op " + o.name + " allocates tpe " + tpe.name + " with no corresponding data definition")
        val data = DataStructs(tpe)
        if (init.length != data.fields.length)
          err("allocator " + o.name + " has a different number of fields than the data definition for " + tpe.name)
      case Getter(structArgIndex,field) =>
        if (structArgIndex > o.args.length) err("arg index " + structArgIndex + " does not exist for op " + o.name)
        val struct = o.args.apply(structArgIndex).tpe
        val data = DataStructs.get(struct)
        if (data.isEmpty) err("no struct definitions found for arg index " + structArgIndex + " in op " + o.name)
        if (!data.get.fields.map(_.name).contains(field)) err("struct arg " + structArgIndex + " does not contain field " + field + " in op " + o.name)
      case Setter(structArgIndex,field,value) =>
        if (structArgIndex > o.args.length) err("arg index " + structArgIndex + " does not exist for op " + o.name)
        val struct = o.args.apply(structArgIndex).tpe
        val data = DataStructs.get(struct)
        if (data.isEmpty) err("no struct definitions found for arg index " + structArgIndex + " in op " + o.name)
        if (!data.get.fields.map(_.name).contains(field)) err("struct arg " + structArgIndex + " does not contain field " + field + " in op " + o.name)        
      case map:Map =>
        val col = o.args.apply(map.argIndex).tpe
        if (ForgeCollections.get(col).isEmpty) err("map argument " + col.name + " is not a ParallelCollection")
        if (map.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("map op with undefined type par: " + o.name)
        if (map.argIndex < 0 || map.argIndex > o.args.length) err("map op with illegal arg parameter: " + o.name)
      case zip:Zip =>
        val colA = o.args.apply(zip.argIndices._1).tpe
        val colB = o.args.apply(zip.argIndices._2).tpe
        if (ForgeCollections.get(colA).isEmpty) err("zip argument " + colA.name + " is not a ParallelCollection")
        if (ForgeCollections.get(colB).isEmpty) err("zip argument " + colB.name + " is not a ParallelCollection")
        if (zip.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("zipWith op with undefined type parg: " + o.name)
        if (zip.argIndices.productIterator.asInstanceOf[Iterator[Int]].exists(a => a < 0 || a > o.args.length)) err("zipWith op with illegal arg parameter: " + o.name)
      case reduce:Reduce =>
        val col = o.args.apply(reduce.argIndex).tpe
        if (ForgeCollections.get(col).isEmpty) err("reduce argument " + col.name + " is not a ParallelCollection")
        if (reduce.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("reduce op with undefined type par: " + o.name)
        if (reduce.argIndex < 0 || reduce.argIndex > o.args.length) err("reduce op with illegal arg parameter: " + o.name)        
        if (reduce.zero.retTpe != reduce.tpePars._1) err("reduce op with illegal zero parameter: " + o.name)
      case filter:Filter =>
        val col = o.args.apply(filter.argIndex).tpe
        if (ForgeCollections.get(col).isEmpty || !ForgeCollections.get(col).forall(_.isInstanceOf[ParallelCollectionBuffer])) err("filter argument " + col.name + " is not a ParallelCollectionBuffer")
        if (filter.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("filter op with undefined type par: " + o.name)
        if (filter.argIndex < 0 || filter.argIndex > o.args.length) err("filter op with illegal arg parameter: " + o.name)      
      case foreach:Foreach =>
        val col = o.args.apply(foreach.argIndex).tpe
        if (ForgeCollections.get(col).isEmpty) err("foreach argument " + col.name + " is not a ParallelCollection")
        if (foreach.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("foreach op with undefined type par: " + o.name)
        if (foreach.argIndex < 0 || foreach.argIndex > o.args.length) err("foreach op with illegal arg parameter: " + o.name)      
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
      Impls(o) match {
        case single:SingleTask => emitImplMethod(o,single.func,stream,indent)
        case _ =>
      }
    }    
  }  
  def emitCompositeImplMethods(opsGrp: DSLOps, stream: PrintWriter, indent: Int = 0) {
    for (o <- unique(opsGrp.ops)) { 
      Impls(o) match {
        case composite:Composite => emitImplMethod(o,composite.func,stream,indent)
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

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
    // GenOverloadHack should be mixed in by package traits only
    "Base"
  }
  
  /**
   * Quoting for formatted code-gen
   */  
    
   def inline(o: Rep[DSLOp], str: Exp[String], quoter: Exp[Any] => String = quote) = {     
     var b = quoter(str)
     for (i <- 0 until o.args.length) {
       val name = o.args.apply(i).name 
       b = b.replaceAllLiterally(quoter(quotedArg(name)), name)
       // allow named args to be referred to by position as well 
       b = b.replaceAllLiterally(quoter(quotedArg(i)), name) 
     }    
     for (i <- 0 until o.implicitArgs.length) {
       val name = o.implicitArgs.apply(i).name 
       // implicit args can only be quoted by name so there is no ambiguity
       b = b.replaceAllLiterally(quoter(quotedArg(name)), name)
     }         
     for (i <- 0 until o.tpePars.length) {
       b = b.replaceAllLiterally(quoter(quotedTpe(i,o)), o.tpePars.apply(i).name)
     }    
     b
   }  
  
   def replaceWildcards(s: String) = {
     var o = s
     o = s.replaceAll(qu, "\"")

     // splice in the quoted symbol. we use a wildcard instead of an expression tree here
     // because string interpolation does not have a corresponding IR node.
     while (o.contains(symMarker)) {
       val st = o.indexOf(symMarker)
       val end = o.indexOf(symMarker, st+1)
       val symNum: Int = o.slice(st+symMarker.length, end).toInt
       val sym = globalDefs.find(_.lhs.apply(0).id == symNum).get.lhs.apply(0)
       o = o.slice(0,st) + quoteLiteral(sym) + o.slice(end+symMarker.length,o.length)
     }
     o
   }
          
   override def quote(x: Exp[Any]) : String = x match {
     case Def(QuoteBlockResult(func,args,ret,captured)) if (isThunk(func.tpe)) => func.name
     case Def(QuoteBlockResult(func,args,ret,captured)) => func.name + "(" + replaceWildcards(captured.mkString(",")) + ")"
     case Const(s: String) if quoteLiterally => replaceWildcards(s) // don't add quotes          
     case _ => super.quote(x)
   }  
   
  /**
   * For dc_alloc. By convention, dc_alloc's return tpe par must be its last tpe par, if it has one.
   */
  def instAllocReturnTpe(o: Rep[DSLOp], colTpePar: Rep[DSLType], elemTpePar: Rep[DSLType]): List[Rep[DSLType]] = {
    // dc_alloc is context sensitive: if the first argument is a tpe parameter, we assume a type signature of [R,CR]. otherwise, we assume a signature of [_,R]
    // note that dc_alloc always takes exactly 2 arguments (a collection and a size). this is still a bit hokey.
    if (o.tpePars.length > 0) {
      val colTpe = o.args.apply(0).tpe
      if (isTpePar(colTpe)) List(elemTpePar,colTpePar) else o.tpePars.dropRight(1).map(p => if (p == colTpe.tpePars.apply(0)) colTpePar.tpePars.apply(0) else p) :+ elemTpePar
    }
    else Nil
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
  def simpleArgName(t: Rep[DSLArg]): String = t.name
  def makeArgs(args: List[Rep[DSLArg]], makeArgName: (Rep[DSLArg] => String) = simpleArgName, addParen: Boolean = true) = {
    if (args.length == 0 && !addParen) {
      ""      
    }
    else {
      "(" + args.map(makeArgName).mkString(",") + ")"
    }
  }
  def makeArgsWithType(args: List[Rep[DSLArg]], typify: Rep[DSLType] => String = repify, addParen: Boolean = true) = makeArgs(args, t => argify(t, typify), addParen)
  def makeArgsWithNowType(args: List[Rep[DSLArg]], addParen: Boolean = true) = makeArgsWithType(args, repifySome, addParen)
  def makeOpArgs(o: Rep[DSLOp], addParen: Boolean = true) = makeArgs(o.args, addParen = addParen)
  def makeOpFutureArgs(o: Rep[DSLOp], makeArgName: (Rep[DSLArg] => String)) = makeArgs(o.args, t => { val arg = makeArgName(t); if (t.tpe.stage == now && !isTpeInst(t.tpe)) "unit("+arg+")" else arg })
  def makeOpArgsWithType(o: Rep[DSLOp], typify: Rep[DSLType] => String = repify, addParen: Boolean = true) = makeArgsWithType(o.args, typify, addParen)
  def makeOpArgsWithNowType(o: Rep[DSLOp], addParen: Boolean = true) = makeArgsWithNowType(o.args, addParen)
  // not used -- still needed?
  // def makeOpArgsWithDefaults(o: Rep[DSLOp], typify: Rep[DSLType] => String = repify) = makeArgs(o.args, t => argify(t, typify))
  // def makeOpArgsWithNowDefault(o: Rep[DSLOp]) = makeOpArgsWithDefaults(o, repifySome)

  // untyped implicit args
  def makeImplicitCtxBounds(tpePars: List[Rep[TypePar]]) = {
    tpePars.flatMap(a => a.ctxBounds.map(b => "implicitly["+b.name+"["+quote(a)+"]]")).mkString(",")
  }  
  def makeOpImplicitCtxBounds(o: Rep[DSLOp]) = makeImplicitCtxBounds(o.tpePars)    
  def makeImplicitArgs(implicitArgs: List[Rep[DSLArg]], ctxBoundsStr: String = "") = {
    // ctxBounds must come before regular implicits
    val ctxBounds2 = if (ctxBoundsStr == "") "" else ctxBoundsStr+","
    if (implicitArgs.length > 0) "(" + ctxBounds2 + implicitArgs.map(_.name).mkString(",") + ")"
    else ""    
  }
  def makeOpImplicitArgs(o: Rep[DSLOp]) = makeImplicitArgs(o.implicitArgs, makeOpImplicitCtxBounds(o)) // explicitly passing implicits requires passing ctxBounds, too
  def makeOpImplicitArgsWithOverload(o: Rep[DSLOp], asVals: Boolean = false) = makeImplicitArgs(implicitArgsWithOverload(o), makeOpImplicitCtxBounds(o))
  
  // typed implicit args with context bounds (only needed for instance methods)
  // 'without' is used to subtract bounds that are already in scope
  def implicitCtxBoundsWithType(tpePars: List[Rep[TypePar]], without: List[Rep[TypePar]] = Nil) = {
    val withoutBounds = without.flatMap(a => a.ctxBounds)
    tpePars.flatMap(a => a.ctxBounds.diff(withoutBounds).map(b => ephemeralTpe(b.name+"["+quote(a)+"]", stage = now))).distinct 
  }  
  def makeImplicitArgsWithCtxBoundsWithType(implicitArgs: List[Rep[DSLArg]], tpePars: List[Rep[TypePar]], without: List[Rep[TypePar]] = Nil, asVals: Boolean = false) = {
    val addArgs = implicitCtxBoundsWithType(tpePars, without)
    val l = implicitArgs.length
    makeImplicitArgsWithType(implicitArgs ++ addArgs.zip(l until l+addArgs.length).map(anyToImplicitArg), asVals)    
  }
  
  // typed implicit args without context bounds
  def makeImplicitArgsWithType(implicitArgs: List[Rep[DSLArg]], asVals: Boolean = false) = {
    val prefix = if (asVals == true) "val " else ""
    if (implicitArgs.length > 0) "(implicit " + implicitArgs.map(t => prefix + t.name + ": " + repifySome(t.tpe)).mkString(",") + ")"
    else ""    
  } 
  def makeOpImplicitArgsWithType(o: Rep[DSLOp], asVals: Boolean = false) = makeImplicitArgsWithType(o.implicitArgs, asVals)
  def makeOpImplicitArgsWithOverloadWithType(o: Rep[DSLOp], asVals: Boolean = false) = makeImplicitArgsWithType(implicitArgsWithOverload(o), asVals)
  
  // overload name clash resolution using implicit hack
  // problem: the local numbering of a signature can overlap with the same Overloaded# as one of the unique targets, causing an ambiguity
  // def nameClash(o1: Rep[DSLOp], o2: Rep[DSLOp]) = o1.name == o2.name && o1.args.length == o2.args.length && (o1.args.zip(o2.args).forall(t => getHkTpe(t._1.tpe).name == getHkTpe(t._2.tpe).name || (t._1.tpe.stage == future && t._2.tpe.stage == future)))
  def nameClash(o1: Rep[DSLOp], o2: Rep[DSLOp]) = o1.name == o2.name // forces a global numbering
  def nameClashesGrp(o: Rep[DSLOp]) = OpsGrp(o.grp).ops.filter(o2 => nameClash(o,o2)) 
  def nameClashesUniversal(o: Rep[DSLOp]) = OpsGrp.values.toList.flatMap(g => g.ops.filter(o2 => nameClash(o,o2)))
  def nameClashId(o: Rep[DSLOp]) = {
    val clashes = nameClashesUniversal(o)
    if (clashes.length > 1) (clashes.indexOf(o)+1).toString else ""    
  }
  def implicitArgsWithOverload(o: Rep[DSLOp]) = {    
    val i = nameClashId(o)
    if (i != "") {
      o.implicitArgs :+ anyToImplicitArg(ephemeralTpe("Overload" + i, stage = now), o.implicitArgs.length)
    }
    else {
      o.implicitArgs
    }
  }      
  def needDisambiguate(o: Rep[DSLOp], clasher: Rep[DSLOp] => List[Rep[DSLOp]] = nameClashesGrp) = {
    // TODO: when do we need to disambiguate implicit args? (explicitly pass them). the fundamental reason is if there are
    // multiple possible implicits in scope.. how do we know? should we just always pass them explicitly? (this could improve staging performance, too)
    // -- passing them all explicitly doesn't currently work because we collapse the back-end implementations, so not every front-end signature should pass an implicit
    //    in order to do this, we need to pass the implicits corresponding to the unique back-end signature
     
    // the issue with nameClashes is that if the receiver method (e.g. string_+) also requires overload implicits, the ones from OverloadHack are also in scope
    // we should be able to solve this by de-prioritizing the OverloadHack vals somehow
    (clasher(o).length > 1 && unique(OpsGrp(o.grp).ops).contains(o)) // inefficient!! -> should refactor things so that we can store this with the op when it is created.
  }
  
  // method names   
  val specialCharacters = scala.collection.immutable.Map("+" -> "pl", "-" -> "sub", "/" -> "div", "*" -> "mul", "=" -> "eq", "<" -> "lt", ">" -> "gt", "&" -> "and", "|" -> "or", "!" -> "bang", ":" -> "cln")
  def sanitize(x: String) = {
    var out = x
    specialCharacters.keys.foreach { k => if (x.contains(k)) out = out.replace(k, specialCharacters(k)) }
    out
  }
  
  def makeDefWithOverride(o: Rep[DSLOp]) = {
    if (overrideList.contains(o.name)) "override def"
    else "def"
  }
  def makeOpMethodName(o: Rep[DSLOp]) = {
    // adding the nameClashId is another way to avoid chaining the Overload implicit, but the weird method names that result are confusing
    val i = "" //nameClashId(o)    
    o.style match {
      case `staticMethod` => o.grp.name.toLowerCase + i + "_object_" + sanitize(o.name).toLowerCase
      case `compilerMethod` => 
        if (o.name != sanitize(o.name)) err("compiler op name has special characters that require reformatting: " + o.name)
        o.name // should be callable directly from impl code
      case _ => o.grp.name.toLowerCase + i + "_" + sanitize(o.name).toLowerCase
    }
  }
  def makeFullArgs(o: Rep[DSLOp], makeArgs: Rep[DSLOp] => String, clasher: Rep[DSLOp] => List[Rep[DSLOp]] = nameClashesGrp) = {
    makeTpePars(o.tpePars) + makeArgs(o) + (if (needDisambiguate(o,clasher)) makeOpImplicitArgsWithOverload(o) else "")  
  }  
  def makeOpMethodNameWithArgs(o: Rep[DSLOp]) = makeOpMethodName(o) + makeFullArgs(o, o => makeOpArgs(o))
  def makeOpMethodNameWithFutureArgs(o: Rep[DSLOp], makeArgName: Rep[DSLArg] => String = simpleArgName) = makeOpMethodName(o) + makeFullArgs(o, k => makeOpFutureArgs(k,makeArgName), nameClashesUniversal) // front-end can name clash with anything
  def makeOpMethodSignature(o: Rep[DSLOp]) = "def " + makeOpMethodName(o) + makeTpeParsWithBounds(o.tpePars) + makeOpArgsWithType(o) + makeOpImplicitArgsWithOverloadWithType(o)
  def makeSyntaxMethod(o: Rep[DSLOp], withReturnTpe: Boolean = false) = {
    // adding the return type increases verbosity in the generated code, so we omit it by default
    val ret = if (withReturnTpe) ": " + repifySome(o.retTpe) else ""
    val curriedArgs = o.curriedArgs.map(a => makeArgsWithNowType(a)).mkString("")
    "def " + o.name + makeTpeParsWithBounds(o.tpePars) + makeArgsWithNowType(o.firstArgs, o.effect != pure) + curriedArgs + makeOpImplicitArgsWithOverloadWithType(o) + ret + " = " + makeOpMethodNameWithFutureArgs(o)
  }
  
  def makeOpImplMethodName(o: Rep[DSLOp]) = makeOpMethodName(o) + "_impl"
  def makeOpImplMethodNameWithArgs(o: Rep[DSLOp]) = makeOpImplMethodName(o) + nameClashId(o) + makeTpePars(o.tpePars) + makeOpArgs(o) + makeOpImplicitArgs(o)
  def makeOpImplMethodSignature(o: Rep[DSLOp]) = "def " + makeOpImplMethodName(o) + nameClashId(o) + makeTpeParsWithBounds(o.tpePars) + makeOpArgsWithType(o) + makeOpImplicitArgsWithType(o) + ": " + repifySome(o.retTpe) 
  
  /**
   * Op sanity checking
   * 
   * These are mostly repetitive right now, but we could specialize the error checking more (or generalize it to be more concise).
   */
  def validTpePar(o: Rep[DSLOp], tpePar: Rep[DSLType]) = tpePar match {
    case Def(TpePar(name,_,_)) => o.tpePars.exists(_.name == name)
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
        val struct = getHkTpe(o.args.apply(structArgIndex).tpe)
        val data = DataStructs.get(struct)
        if (data.isEmpty) err("no struct definitions found for arg index " + structArgIndex + " in op " + o.name)
        if (!data.get.fields.map(_.name).contains(field)) err("struct arg " + structArgIndex + " does not contain field " + field + " in op " + o.name)
      case Setter(structArgIndex,field,value) =>
        if (structArgIndex > o.args.length) err("arg index " + structArgIndex + " does not exist for op " + o.name)
        val struct = getHkTpe(o.args.apply(structArgIndex).tpe)
        val data = DataStructs.get(struct)
        if (data.isEmpty) err("no struct definitions found for arg index " + structArgIndex + " in op " + o.name)
        if (!data.get.fields.map(_.name).contains(field)) err("struct arg " + structArgIndex + " does not contain field " + field + " in op " + o.name)        
      case map:Map =>
        val col = getHkTpe(o.args.apply(map.argIndex).tpe)
        if (ForgeCollections.get(col).isEmpty) err("map argument " + col.name + " is not a ParallelCollection")
        if (map.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("map op with undefined type par: " + o.name)
        if (map.argIndex < 0 || map.argIndex > o.args.length) err("map op with illegal arg parameter: " + o.name)
      case zip:Zip =>
        val colA = getHkTpe(o.args.apply(zip.argIndices._1).tpe)
        val colB = getHkTpe(o.args.apply(zip.argIndices._2).tpe)
        if (ForgeCollections.get(colA).isEmpty) err("zip argument " + colA.name + " is not a ParallelCollection")
        if (ForgeCollections.get(colB).isEmpty) err("zip argument " + colB.name + " is not a ParallelCollection")
        if (zip.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("zipWith op with undefined type parg: " + o.name)
        if (zip.argIndices.productIterator.asInstanceOf[Iterator[Int]].exists(a => a < 0 || a > o.args.length)) err("zipWith op with illegal arg parameter: " + o.name)
      case reduce:Reduce =>
        val col = getHkTpe(o.args.apply(reduce.argIndex).tpe)
        if (ForgeCollections.get(col).isEmpty) err("reduce argument " + col.name + " is not a ParallelCollection")
        if (!validTpePar(o,reduce.tpePar)) err("reduce op with undefined type par: " + o.name)
        if (reduce.argIndex < 0 || reduce.argIndex > o.args.length) err("reduce op with illegal arg parameter: " + o.name)        
        // if (reduce.zero.retTpe != reduce.tpePar) err("reduce op with illegal zero parameter: " + o.name)
      case mapreduce:MapReduce =>
        val col = getHkTpe(o.args.apply(mapreduce.argIndex).tpe)
        if (ForgeCollections.get(col).isEmpty) err("mapreduce argument " + col.name + " is not a ParallelCollection")
        if (mapreduce.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("mapreduce op with undefined type par: " + o.name)
        if (mapreduce.argIndex < 0 || mapreduce.argIndex > o.args.length) err("mapreduce op with illegal arg parameter: " + o.name)        
      case filter:Filter =>
        val col = getHkTpe(o.args.apply(filter.argIndex).tpe)
        if (ForgeCollections.get(col).isEmpty || !ForgeCollections.get(getHkTpe(o.retTpe)).forall(_.isInstanceOf[ParallelCollectionBuffer])) err("filter return type " + col.name + " is not a ParallelCollectionBuffer")
        if (filter.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("filter op with undefined type par: " + o.name)
        if (filter.argIndex < 0 || filter.argIndex > o.args.length) err("filter op with illegal arg parameter: " + o.name)      
      case hfr:HashFilterReduce =>
        val col = getHkTpe(o.args.apply(hfr.argIndex).tpe)
        if (ForgeCollections.get(col).isEmpty || !ForgeCollections.get(getHkTpe(o.retTpe)).forall(_.isInstanceOf[ParallelCollectionBuffer])) err("hashFilterReduce return type " + col.name + " is not a ParallelCollectionBuffer")
        if (hfr.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("hashFilterReduce op with undefined type par: " + o.name)
        if (hfr.argIndex < 0 || hfr.argIndex > o.args.length) err("hashFilterReduce op with illegal arg parameter: " + o.name)            
      case foreach:Foreach =>
        val col = getHkTpe(o.args.apply(foreach.argIndex).tpe)
        if (ForgeCollections.get(col).isEmpty) err("foreach argument " + col.name + " is not a ParallelCollection")
        if (!validTpePar(o,foreach.tpePar)) err("foreach op with undefined type par: " + o.name)
        if (foreach.argIndex < 0 || foreach.argIndex > o.args.length) err("foreach op with illegal arg parameter: " + o.name)      
      case _ => // nothing to check
    }
  }
      
   
  /**
   * Front-end codegen
   */
  
  def checkOps(opsGrp: DSLOps) {
    for (o <- unique(opsGrp.ops)) check(o)
  }
  
  // certain ops (e.g. "apply" cannot be expressed with infix notation right now), so we use implicits as a workaround
  def noInfix(o: Rep[DSLOp]) = {
    // blacklist or curried args or function args (in the latter two cases, infix doesn't always resolve correctly)
    noInfixList.contains(o.name) || o.curriedArgs.length > 0 || hasFuncArgs(o)
  }
  
  def emitOpSyntax(opsGrp: DSLOps, stream: PrintWriter) {
    emitBlockComment("Operations", stream)
    stream.println()
    
    // implicits go in a base class for lower priority
    val implicitOps = opsGrp.ops.filter(e=>e.style==implicitMethod)
    if (!implicitOps.isEmpty) {
      stream.println("trait " + opsGrp.name + "Base extends " + baseOpsCls(opsGrp.grp) + " {")
      stream.println("  this: " + dsl + " => ")
      stream.println()
      for (o <- implicitOps) {
        stream.println("  implicit " + makeSyntaxMethod(o, withReturnTpe = true))
      }
      stream.println()
      // we separate these just for generated code readability      
      for (o <- implicitOps) {
        stream.println("  " + makeOpMethodSignature(o) + ": " + repifySome(o.retTpe))
      }
      stream.println("}")
      stream.println()
    }
    
    val base = if (implicitOps.length > 0) opsGrp.name + "Base" else baseOpsCls(opsGrp.grp)
    stream.println("trait " + opsGrp.name + " extends " + base + " {")
    stream.println("  this: " + dsl + " => ")
    stream.println()
        
    // static ops
    val staticOps = opsGrp.ops.filter(e=>e.style==staticMethod)
    if (staticOps.length > 0) {
      stream.println("  object " + opsGrp.grp.name + " {")
      for (o <- staticOps) {        
        stream.println("    " + makeSyntaxMethod(o))
      }
      stream.println("  }")
      stream.println()
    }   
        
    // direct ops
    val directOps = opsGrp.ops.filter(e=>e.style==directMethod)
    for (o <- directOps) {
      stream.println("  " + makeSyntaxMethod(o))
    }
    if (directOps.length > 0) {
      stream.println()
    }    
    
    // infix ops
    val allInfixOps = opsGrp.ops.filter(e=>e.style==infixMethod)
        
    val (pimpOps, infixOps) = allInfixOps.partition(noInfix)
    if (pimpOps.nonEmpty) {
      // set up a pimp-my-library style promotion
      val ops = pimpOps.filterNot(o => getHkTpe(o.args.apply(0).tpe).name == "Var" || o.args.apply(0).tpe.stage == now)
      val tpes = ops.map(_.args.apply(0).tpe).distinct
      for (tpe <- tpes) {
        val tpePars = tpe match {
          case Def(TpeInst(_,args)) => args.filter(isTpePar).asInstanceOf[List[Rep[TypePar]]]
          case Def(TpePar(_,_,_)) => List(tpe.asInstanceOf[Rep[TypePar]])
          case _ => tpe.tpePars          
        }
        val tpeArgs = tpe match {
          case Def(TpeInst(hk,args)) => args.filterNot(isTpePar)
          case _ => Nil
        }

        val opsClsName = opsGrp.grp.name + tpe.name + tpeArgs.map(_.name).mkString("") + "OpsCls"
        stream.println("  implicit def repTo" + opsClsName + makeTpeParsWithBounds(tpePars) + "(x: " + repify(tpe) + ") = new " + opsClsName + "(x)")
        if (pimpOps.exists(o => o.args.apply(0).tpe.stage == now)) {
          stream.println("  implicit def liftTo" + opsClsName + makeTpeParsWithBounds(tpePars) + "(x: " + quote(tpe) + ") = new " + opsClsName + "(unit(x))")
        }
        // we provide the Var conversion even if no lhs var is declared, since it is essentially a chained implicit with readVar
        if (Tpes.exists(t => getHkTpe(t).name == "Var")) {
          stream.println("  implicit def varTo" + opsClsName + makeTpeParsWithBounds(tpePars) + "(x: " + varify(tpe) + ") = new " + opsClsName + "(readVar(x))")
        }
        stream.println("")
        stream.println("  class " + opsClsName + makeTpeParsWithBounds(tpePars) + "(val self: " + repify(tpe) + ") {")
    
        for (o <- ops if quote(o.args.apply(0).tpe) == quote(tpe)) {
          val otherArgs = makeArgsWithNowType(o.firstArgs.drop(1))
          val curriedArgs = o.curriedArgs.map(a => makeArgsWithNowType(a)).mkString("")
          val hkTpePars = if (isTpePar(tpe)) tpePars else getHkTpe(tpe).tpePars                  
          val otherTpePars = o.tpePars.filterNot(p => hkTpePars.map(_.name).contains(p.name))
          stream.println("    def " + o.name + makeTpeParsWithBounds(otherTpePars) + otherArgs + curriedArgs
            + (makeImplicitArgsWithCtxBoundsWithType(implicitArgsWithOverload(o), o.tpePars diff otherTpePars, without = hkTpePars)) + " = " + makeOpMethodNameWithFutureArgs(o, a => if (a.name ==  o.args.apply(0).name) "self" else simpleArgName(a)))
        }
        stream.println("  }")
      }
      stream.println()              
    }        
    
    for (o <- infixOps) {
      stream.print("  def infix_" + o.name + makeTpeParsWithBounds(o.tpePars))
      stream.print(makeOpArgsWithNowType(o))
      stream.print(makeOpImplicitArgsWithOverloadWithType(o))
      stream.println(" = " + makeOpMethodNameWithFutureArgs(o))        
    }
    stream.println()      
        
    // abstract methods
    for (o <- unique(opsGrp.ops.filter(e=>e.style != compilerMethod && e.style != implicitMethod))) {
      stream.println("  " + makeOpMethodSignature(o) + ": " + repifySome(o.retTpe))
    }
    
    stream.println("}")
    
    // compiler ops
    val compilerOps = opsGrp.ops.filter(e=>e.style == compilerMethod)
    if (!compilerOps.isEmpty) {
      stream.println("trait " + opsGrp.grp.name + "CompilerOps extends " + opsGrp.name + " {")
      stream.println("  this: " + dsl + " => ")
      stream.println()    
      for (o <- compilerOps) {
        stream.println("  " + makeOpMethodSignature(o) + ": " + repifySome(o.retTpe))
      }      
      stream.println("}")
      stream.println()
    }
  }
}

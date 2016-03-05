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


  /**
   * Overloading resolution, using implicit hack.
   * We need overloads for both front-end signatures (e.g., "+", universal) as well as abstract methods (e.g. vector_plus, grp).
   */

  // if after converting Ts and Vars to Reps there are duplicates, remove them.
  def unique(ops: List[Rep[DSLOp]]) = uniqueMap(ops)._1

  def uniqueMap(ops: List[Rep[DSLOp]]) = {
    // we maintain a separate ArrayBuffer along with the map to retain order
    val filtered = scala.collection.mutable.ArrayBuffer[Rep[DSLOp]]()
    val canonicalMap = scala.collection.mutable.HashMap[String,Rep[DSLOp]]()
    // add to filtered only if canonical version doesn't already exist
    for (o <- ops) {
      val t = canonicalize(o)
      if (!canonicalMap.contains(t)) {
        filtered += o
        canonicalMap(t) = o
      }
    }
    (filtered.toList,canonicalMap)
  }

  def canonicalize(o: Rep[DSLOp]) = o.grp.name + o.name + makeOpArgsWithType(o) + makeOpImplicitArgs(o)

  // precomputed for performance
  lazy val allOps = OpsGrp.values.toList.flatMap(g => g.ops)
  lazy val allOpsCanonicalMap = uniqueMap(allOps)._2
  def canonical(o: Rep[DSLOp]): Rep[DSLOp] = allOpsCanonicalMap.getOrElse(canonicalize(o), err("no canonical version of " + o.name + " found"))

  // The actual op comparison is a performance hotspot; checks should be decomposed to allow early exits.

  // Here we try to remove unnecessary overloaded implicits; unfortunately, it doesn't appear to have much of an impact (on compile times).
  // def nameClash(o1: Rep[DSLOp], o2: Rep[DSLOp]) = {
  //   val simpleChecks = o1.style == o2.style && o1.name == o2.name && o1.args.length == o2.args.length
  //   simpleChecks && {
  //     val o1infix = !noInfix(o1)
  //     val o2infix = !noInfix(o2)
  //     (o1infix == o2infix) && {
  //       // with implicit classes we don't need to consider the first argument for method overloading resolution,
  //       // unless it is the same type (after promotion) in both ops
  //       val usingImplicits = o1.style == infixMethod && !o1infix && !o2infix
  //       val (co1, co2) = (canonical(o1), canonical(o2)) // required to have a consistent view
  //       co1 == co2 || {
  //         val (o1args, o2args) = if (usingImplicits) (co1.args.drop(1), co2.args.drop(1)) else (co1.args, co2.args)
  //         val lhsMatch = {
  //           !usingImplicits || o1.args.length == 0 || {
  //             val t1 = o1.args.apply(0).tpe
  //             val t2 = o2.args.apply(0).tpe
  //             getHkTpe(t1).name == getHkTpe(t2).name || repify(t1) == repify(t2)
  //           }
  //         }
  //         lhsMatch && o1args.zip(o2args).forall { a =>
  //           getHkTpe(a._1.tpe).name == getHkTpe(a._2.tpe).name ||
  //           isTpePar(a._1.tpe) || isTpePar(a._2.tpe) ||
  //           (isFuncArg(a._1) && isFuncArg(a._2)) ||
  //           (repify(a._1).startsWith("Rep") && repify(a._2).startsWith("Rep")) // check for promotions
  //         }
  //       }
  //     }
  //   }
  // }

  def nameClash(o1: Rep[DSLOp], o2: Rep[DSLOp]) = simpleNameClash(o1,o2)

  def simpleNameClash(o1: Rep[DSLOp], o2: Rep[DSLOp]) = o1.style == o2.style && o1.name == o2.name // forces a global numbering

  def nameClashesGrp(o: Rep[DSLOp]) = opsGrpOf(o).map(_.ops.filter(o2 => o.grp.name == o2.grp.name && nameClash(o,o2))).getOrElse(Nil)

  def nameClashesUniversal(o: Rep[DSLOp]) = allOps.filter(o2 => nameClash(o,o2))

  def nameClashesSimple(o: Rep[DSLOp]) = allOps.filter(o2 => simpleNameClash(o,o2))

  def nameClashId(o: Rep[DSLOp], clasher: Rep[DSLOp] => List[Rep[DSLOp]] = nameClashesUniversal) = {
    val clashes = clasher(o)
    if (clashes.length > 1) (clashes.indexOf(o)+1).toString else ""
  }

  // refers to the back-end method signature
  // front-end overloads are added in an ad-hoc way right now, by always calling implicitArgsWithOverload
  def needOverload(o: Rep[DSLOp]) = {
    /*!Config.fastCompile &&*/ (!Labels.contains(o) && nameClashesGrp(o).length > 1)
  }

  // useCanonical should be false if we're referring to the front-end signature (e.g. '+') but true if we're
  // referring to the back-end signature (e.g. 'vector_plus')
  def implicitArgsWithOverload(o: Rep[DSLOp], useCanonical: Boolean = false) = {
    val o1 = if (useCanonical) canonical(o) else o
    val i = nameClashId(o1)
    if (i != "") {
      // redirect overloads can clash with regular overloads since they don't get separate abstract methods
      val overloadName = if (isRedirect(o)) "ROverload" else "Overload"
      o.implicitArgs :+ anyToImplicitArg(ephemeralTpe(overloadName + i, stage = now), o.implicitArgs.length)
    }
    else {
      o.implicitArgs
    }
  }

  /**
   * Op argument formatting
   */

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

  def makeFullArgs(o: Rep[DSLOp], makeArgs: Rep[DSLOp] => String) = {
    // we always pass implicit arguments explicitly (in practice, less issues arise this way)
    val implicitArgs = if (needOverload(o)) makeOpImplicitArgsWithOverload(o, useCanonical = true) else makeOpImplicitArgs(o)
    makeTpeParsAsArgs(o.tpePars, o.tpePars) + makeArgs(o) + implicitArgs
  }


  /**
   * Higher-kinded type parameter manipulation
   */

  def withoutHkTpePars(tpePars: List[Rep[TypePar]]): List[Rep[TypePar]] = {
    tpePars.filter {
      case Def(HkTpePar(n,args,c,s)) => false
      case _ => true
    }
  }

  def getHkTpeParInstantiations(tpePars: List[Rep[TypePar]], args: List[Rep[DSLArg]], implicitArgs: List[Rep[DSLArg]]): List[Rep[DSLArg]] = {
    // hkTpePars context bounds must be included, since we cannot use context bounds with them
    // We have to be careful -- we only want to include manifests for any concrete instantiations of the hkTpePar in the op.
    val hkTpePars = (tpePars collect { case d@Def(HkTpePar(n,t,sigs,s)) => (n,sigs) }).toMap
    val hkTpeParInsts =
      ((args ++ implicitArgs).map(_.tpe) flatMap {
        case d@Def(TpeInst(hkTpe, hkArgs)) if hkTpePars.contains(hkTpe.name) =>
          val sigs = hkTpePars(hkTpe.name)
          sigs.map(b => ephemeralTpe(b.name+"["+quote(d)+"]", stage = now))
        case _ => Nil
      }).distinct

    hkTpeParInsts.zipWithIndex.map(t => arg(implicitCtxBoundArgPrefix+"_hk_"+t._2, t._1))
  }


  /**
   * Op implicit argument formatting
   */

  // untyped implicit args
  def makeImplicitCtxBoundsStringList(tpePars: List[Rep[TypePar]]): List[String]  = {
    tpePars.flatMap { a =>
      a.ctxBounds.map(b => "implicitly["+b.name+"["+quote(a)+"]]")
    }
  }

  def makeImplicitArgs(tpePars: List[Rep[TypePar]], args: List[Rep[DSLArg]], implicitArgs: List[Rep[DSLArg]]) = {
    val ctxBoundsStringList = makeImplicitCtxBoundsStringList(withoutHkTpePars(tpePars))
    val implicitArgsStringList = implicitArgs.map(quote)
    val hkInstantiationsStringList = getHkTpeParInstantiations(tpePars, args, implicitArgs).map(quote)

    // passing order is: regular ctxBounds, then regular implicits, and finally hkInstantiations context bounds
    val allImplicitsStringList = ctxBoundsStringList ++ implicitArgsStringList ++ hkInstantiationsStringList
    if (allImplicitsStringList.length > 0) "(" + allImplicitsStringList.mkString(",") + ")"
    else ""
  }

  def makeOpImplicitArgs(o: Rep[DSLOp]) = {
    makeImplicitArgs(o.tpePars, o.args, o.implicitArgs)
  }

  def makeOpImplicitArgsWithOverload(o: Rep[DSLOp], asVals: Boolean = false, useCanonical: Boolean = false) = {
    makeImplicitArgs(o.tpePars, o.args, implicitArgsWithOverload(o, useCanonical))
  }

  // typed implicit args with context bounds (only needed for instance methods)
  // 'without' is used to subtract bounds that are already in scope
  def implicitCtxBoundsWithType(tpePars: List[Rep[TypePar]], without: List[Rep[TypePar]] = Nil) = {
    val withoutBounds = without.flatMap(a => a.ctxBounds)
    withoutHkTpePars(tpePars).flatMap(a => a.ctxBounds.diff(withoutBounds).map(b => ephemeralTpe(b.name+"["+quote(a)+"]", stage = now))).distinct
  }

  def makeImplicitArgsWithCtxBoundsWithType(tpePars: List[Rep[TypePar]], args: List[Rep[DSLArg]], implicitArgs: List[Rep[DSLArg]], without: List[Rep[TypePar]] = Nil, asVals: Boolean = false) = {
    val addArgs = implicitCtxBoundsWithType(tpePars, without).zipWithIndex.map(t => arg(implicitCtxBoundArgPrefix+t._2, t._1))
    makeImplicitArgsWithType(tpePars, args, addArgs ++ implicitArgs, asVals)
  }

  // typed implicit args without context bounds (but hkTpePar context bounds are passed explicitly)
  def makeImplicitArgsWithType(tpePars: List[Rep[TypePar]], args: List[Rep[DSLArg]], implicitArgs: List[Rep[DSLArg]], asVals: Boolean = false) = {
    val prefix = if (asVals == true) "val " else ""
    val hkInstantiations = getHkTpeParInstantiations(tpePars, args, implicitArgs)
    val allImplicitArgs = implicitArgs ++ hkInstantiations
    if (allImplicitArgs.length > 0) "(implicit " + allImplicitArgs.map(t => prefix + argify(t,repifySome)).mkString(",") + ")"
    else ""
  }

  def makeOpImplicitArgsWithType(o: Rep[DSLOp], asVals: Boolean = false) = {
    makeImplicitArgsWithType(o.tpePars, o.args, o.implicitArgs, asVals)
  }

  def makeOpImplicitArgsWithOverloadWithType(o: Rep[DSLOp], asVals: Boolean = false, useCanonical: Boolean = false) = {
    makeImplicitArgsWithType(o.tpePars, o.args, implicitArgsWithOverload(o, useCanonical), asVals)
  }


  /**
   * Op method names
   */

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
    Labels.getOrElse(o, {
      // adding the nameClashId is another way to avoid chaining the Overload implicit, but the weird method names that result are confusing
      val i = /*if (Config.fastCompile) nameClashId(canonical(o), nameClashesGrp) else*/ ""
      o.style match {
        case `staticMethod` => o.grp.name.toLowerCase + "_object_" + sanitize(o.name).toLowerCase + i
        case `compilerMethod` =>
          if (o.name != sanitize(o.name)) err("compiler op name has special characters that require reformatting: " + o.name)
          o.name // should be callable directly from impl code
        case _ => o.grp.name.toLowerCase + "_" + sanitize(o.name).toLowerCase + i
      }
    })
  }

  def makeOpMethodNameWithArgs(o: Rep[DSLOp]) = makeOpMethodName(o) + makeFullArgs(o, o => makeOpArgs(o))

  def makeOpMethodNameWithFutureArgs(o: Rep[DSLOp], makeArgName: Rep[DSLArg] => String = simpleArgName) = {
    if (Impls(o).isInstanceOf[Redirect]) {
      var call = "{ " + inline(o, Impls(o).asInstanceOf[Redirect].func, quoteLiteral) + " }"
      for (i <- 0 until o.args.length) {
        call = call.replaceAllLiterally(o.args.apply(i).name, makeArgName(o.args.apply(i)))
      }
      call
    }
    else {
      makeOpMethodName(o) + makeFullArgs(o, k => makeOpFutureArgs(k,makeArgName))
    }
  }

  def makeOpMethodSignature(o: Rep[DSLOp], withReturnTpe: Option[Boolean] = None) = {
    val addRet = withReturnTpe.getOrElse(Config.fastCompile)
    val ret = if (addRet || isRedirect(o)) ": " + repifySome(o.retTpe) else ""
    val implicitArgs = if (needOverload(o)) makeOpImplicitArgsWithOverloadWithType(o, useCanonical = true) else makeOpImplicitArgsWithType(o)
    // if (Config.fastCompile) {
    //   "def " + makeOpMethodName(o) + makeTpeParsWithBounds(o.tpePars) + makeOpArgsWithType(o) + makeOpImplicitArgsWithType(o) + ret
    // }
    // else {
      "def " + makeOpMethodName(o) + makeTpeParsWithBounds(o.tpePars) + makeOpArgsWithType(o) + implicitArgs + ret
    // }
  }

  def makeSyntaxMethod(o: Rep[DSLOp], prefix: String = "def ", withReturnTpe: Option[Boolean] = None) = {
    // adding the return type increases verbosity in the generated code, so we omit it by default
    val addRet = withReturnTpe.getOrElse(Config.fastCompile)
    val ret = if (addRet || isRedirect(o)) ": " + repifySome(o.retTpe) else ""
    val curriedArgs = o.curriedArgs.map(a => makeArgsWithNowType(a)).mkString("")
    prefix + o.name + makeTpeParsWithBounds(o.tpePars) + makeArgsWithNowType(o.firstArgs, o.effect != pure || o.name == "apply") + curriedArgs + makeOpImplicitArgsWithOverloadWithType(o) + ret + " = " + makeOpMethodNameWithFutureArgs(o)
  }

  def makeOpImplMethodName(o: Rep[DSLOp]) = makeOpMethodName(o) + "_impl" + nameClashId(o)

  def makeOpImplMethodNameWithArgs(o: Rep[DSLOp], postfix: String = "") = makeOpImplMethodName(o) + postfix + makeTpeParsAsArgs(o.tpePars, o.tpePars) + makeOpArgs(o) + makeOpImplicitArgs(o)

  def makeOpImplMethodSignature(o: Rep[DSLOp], postfix: String = "", returnTpe: Option[String] = None) = {
    "def " + makeOpImplMethodName(o) + postfix + makeTpeParsWithBounds(o.tpePars) + makeOpArgsWithType(o) + makeOpImplicitArgsWithType(o) + ": " + (returnTpe getOrElse repifySome(o.retTpe))
  }


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

    // arguments to DeliteOpType ops should always be Rep, List, or Seq (or the transformer in mirror will have a hard time)
    def validOpArgs(a: Rep[DSLArg]) = {
      a.tpe.stage == future || isFuncArg(a) || isThunk(a.tpe) || a.tpe.name.startsWith("List") || a.tpe.name.startsWith("Seq")
    }

    def isParallelCollection(t: Rep[DSLType]) = ForgeCollections.contains(t)
    def isParallelCollectionBuffer(t: Rep[DSLType]) = ForgeCollections.get(t).exists(_.isInstanceOf[ParallelCollectionBuffer])

    Impls(o) match {
      case _:DeliteOpType =>
        if (o.args.exists(a => !validOpArgs(a))) err("op " + o.name + " is a Delite op, but has non-Rep arguments")
      case _ =>
    }

    // op-specific checks
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
        if (!isParallelCollection(col)) err("map argument " + col.name + " is not a ParallelCollection")
        if (map.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("map op with undefined type par: " + o.name)
        if (map.argIndex < 0 || map.argIndex > o.args.length) err("map op with illegal arg parameter: " + o.name)
      case zip:Zip =>
        val colA = getHkTpe(o.args.apply(zip.argIndices._1).tpe)
        val colB = getHkTpe(o.args.apply(zip.argIndices._2).tpe)
        if (!isParallelCollection(colA)) err("zip argument " + colA.name + " is not a ParallelCollection")
        if (!isParallelCollection(colB)) err("zip argument " + colB.name + " is not a ParallelCollection")
        if (zip.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("zipWith op with undefined type parg: " + o.name)
        if (zip.argIndices.productIterator.asInstanceOf[Iterator[Int]].exists(a => a < 0 || a > o.args.length)) err("zipWith op with illegal arg parameter: " + o.name)
      case reduce:Reduce =>
        val col = getHkTpe(o.args.apply(reduce.argIndex).tpe)
        if (!isParallelCollection(col)) err("reduce argument " + col.name + " is not a ParallelCollection")
        if (!validTpePar(o,reduce.tpePar)) err("reduce op with undefined type par: " + o.name)
        if (reduce.argIndex < 0 || reduce.argIndex > o.args.length) err("reduce op with illegal arg parameter: " + o.name)
        // if (reduce.zero.retTpe != reduce.tpePar) err("reduce op with illegal zero parameter: " + o.name)
      case mapreduce:MapReduce =>
        val col = getHkTpe(o.args.apply(mapreduce.argIndex).tpe)
        if (!isParallelCollection(col)) err("mapReduce argument " + col.name + " is not a ParallelCollection")
        if (mapreduce.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("mapreduce op with undefined type par: " + o.name)
        if (mapreduce.argIndex < 0 || mapreduce.argIndex > o.args.length) err("mapreduce op with illegal arg parameter: " + o.name)
      case filter:Filter =>
        val col = getHkTpe(o.args.apply(filter.argIndex).tpe)
        if (!isParallelCollection(col)) err("filter argument " + col.name + " is not a ParallelCollection")
        if (!isParallelCollectionBuffer(getHkTpe(o.retTpe))) err("filter return type " + col.name + " is not a ParallelCollectionBuffer")
        if (filter.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("filter op with undefined type par: " + o.name)
        if (filter.argIndex < 0 || filter.argIndex > o.args.length) err("filter op with illegal arg parameter: " + o.name)
      case flatmap:FlatMap =>
        val col = getHkTpe(o.args.apply(flatmap.argIndex).tpe)
        if (!isParallelCollection(col)) err("flatmap argument " + col.name + " is not a ParallelCollection")
        if (!isParallelCollectionBuffer(getHkTpe(o.retTpe))) err("flatmap return type " + col.name + " is not a ParallelCollectionBuffer")
        if (flatmap.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("flatmap op with undefined type par: " + o.name)
        if (flatmap.argIndex < 0 || flatmap.argIndex > o.args.length) err("flatmap op with illegal arg parameter: " + o.name)
      case gb:GroupBy =>
        val col = getHkTpe(o.args.apply(gb.argIndex).tpe)
        if (!isParallelCollection(col)) err("groupBy argument " + col.name + " is not a ParallelCollection")
        val innerCol = getHkTpe(gb.tpePars._4)
        if (!isParallelCollectionBuffer(innerCol)) err("groupBy inner collection type " + innerCol.name + " is not a ParallelCollectionBuffer")
        if (gb.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("groupBy op with undefined type par: " + o.name)
        if (gb.argIndex < 0 || gb.argIndex > o.args.length) err("groupBy op with illegal arg parameter: " + o.name)
      case gbr:GroupByReduce =>
        val col = getHkTpe(o.args.apply(gbr.argIndex).tpe)
        if (!isParallelCollection(col)) err("groupByReduce argument " + col.name + " is not a ParallelCollection")
        if (gbr.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("groupByReduce op with undefined type par: " + o.name)
        if (gbr.argIndex < 0 || gbr.argIndex > o.args.length) err("groupByReduce op with illegal arg parameter: " + o.name)
      case foreach:Foreach =>
        val col = getHkTpe(o.args.apply(foreach.argIndex).tpe)
        if (!isParallelCollection(col)) err("foreach argument " + col.name + " is not a ParallelCollection")
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
    // FIXME: we get scalac internal crashes when using the default-implicit mode now
    // if (Config.fastCompile) {
    //   // default implicit mode (appears empirically slightly faster than infix)
    //   (!mustInfixList.contains(o.name)) && o.args.length > 0 && !o.args.exists(hasDefaultValue)
    // }
    // else {
      // default infix mode (slightly easier to understand what's happening, also fails to apply less than implicits)
      // blacklist or curried args or function args (in the latter two cases, infix doesn't always resolve correctly)
      !mustInfixList.contains(o.name) && (noInfixList.contains(o.name) || o.curriedArgs.length > 0 || hasFuncArgs(o))
    // }
  }

  def emitOpSyntax(opsGrp: DSLOps, stream: PrintWriter) {
    emitBlockComment("Operations", stream)
    stream.println()

    // implicits go in a base class for lower priority
    val implicitOps = opsGrp.ops.filter(e=>e.style==implicitMethod)
    if (!implicitOps.isEmpty) {
      if (unique(implicitOps).length != implicitOps.length) err("non-unique implicit op variants (e.g. Var, T args) are not yet supported")
      stream.println("trait " + opsGrp.name + "Base extends " + baseOpsCls(opsGrp.grp) + " {")
      stream.println("  this: " + dsl + " => ")
      stream.println()
      for (o <- implicitOps) {
        stream.println("  implicit " + makeSyntaxMethod(o, withReturnTpe = Some(true)))
      }
      stream.println()
      // we separate these just for generated code readability
      for (o <- implicitOps if !Impls(o).isInstanceOf[Redirect]) {
        stream.println("  " + makeOpMethodSignature(o, withReturnTpe = Some(true)))
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
    val objects = staticOps.groupBy(_.grp.name)
    for ((name, ops) <- objects) {
      stream.println("  object " + name + " {")
      for (o <- ops) {
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
      val ops = pimpOps.filterNot(o => getHkTpe(o.args.apply(0).tpe).name == "Var" ||
                                       (o.args.apply(0).tpe.stage == now && pimpOps.exists(o2 => o.args.apply(0).tpe.name == o2.args.apply(0).tpe.name && o2.args.apply(0).tpe.stage == future)))
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

        val opsClsName = opsGrp.grp.name + tpe.name.replaceAll("\\.","") + tpeArgs.map(_.name).mkString("") + "OpsCls"
        val implicitParams = if (tpePars.length > 0) makeImplicitCtxBoundsStringList(tpePars).mkString(",") + ",__pos" else "__pos"

        if (tpe.stage == compile) {
          stream.println("  implicit def liftTo" + opsClsName + makeTpeParsWithBounds(tpePars) + "(x: " + repify(tpe) + ")(implicit __pos: SourceContext) = new " + opsClsName + "(x)(" + implicitParams + ")")
        }
        else {
          stream.println("  implicit def repTo" + opsClsName + makeTpeParsWithBounds(tpePars) + "(x: " + repify(tpe) + ")(implicit __pos: SourceContext) = new " + opsClsName + "(x)(" + implicitParams + ")")
          if (pimpOps.exists(o => quote(o.args.apply(0).tpe) == quote(tpe) && o.args.apply(0).tpe.stage == now)) {
            stream.println("  implicit def liftTo" + opsClsName + makeTpeParsWithBounds(tpePars) + "(x: " + quote(tpe) + ")(implicit __pos: SourceContext) = new " + opsClsName + "(unit(x))(" + implicitParams + ")")
          }
          // we provide the Var conversion even if no lhs var is declared, since it is essentially a chained implicit with readVar
          if (Tpes.exists(t => getHkTpe(t).name == "Var")) {
            stream.println("  implicit def varTo" + opsClsName + makeTpeParsWithBounds(tpePars) + "(x: " + varify(tpe) + ")(implicit __pos: SourceContext) = new " + opsClsName + "(readVar(x))(" + implicitParams + ")")
          }
        }

        stream.println()
        stream.println("  class " + opsClsName + makeTpeParsWithBounds(tpePars) + "(val self: " + repify(tpe) + ")(implicit __pos: SourceContext) {")

        for (o <- ops if quote(o.args.apply(0).tpe) == quote(tpe)) {
          val otherArgs = makeArgsWithNowType(o.firstArgs.drop(1), o.effect != pure || o.name == "apply")
          val curriedArgs = o.curriedArgs.map(a => makeArgsWithNowType(a)).mkString("")
          val otherTpePars = o.tpePars.filterNot(p => tpePars.map(_.name).contains(p.name))
          val ret = if (Config.fastCompile) ": " + repifySome(o.retTpe) else ""
          stream.println("    def " + o.name + makeTpeParsWithBounds(otherTpePars) + otherArgs + curriedArgs
            + (makeImplicitArgsWithCtxBoundsWithType(o.tpePars diff otherTpePars, o.args, implicitArgsWithOverload(o), without = tpePars)) + ret + " = " + makeOpMethodNameWithFutureArgs(o, a => if (a.name ==  o.args.apply(0).name) "self" else simpleArgName(a)))
        }
        stream.println("  }")
        stream.println()
      }
      stream.println()
    }

    for (o <- infixOps) {
      stream.println(makeSyntaxMethod(o, prefix = "  def infix_"))
    }
    stream.println()

    // abstract methods
    for (o <- unique(opsGrp.ops.filter(e=>e.style != compilerMethod && e.style != implicitMethod && !Impls(e).isInstanceOf[Redirect]))) {
      stream.println("  " + makeOpMethodSignature(o, withReturnTpe = Some(true)))
    }

    stream.println("}")

    // compiler ops
    val compilerOps = opsGrp.ops.filter(e=>e.style == compilerMethod)
    if (!compilerOps.isEmpty) {
      stream.println("trait " + opsGrp.grp.name + "CompilerOps extends " + opsGrp.name + " {")
      stream.println("  this: " + dsl + " => ")
      stream.println()
      if (unique(compilerOps).length != compilerOps.length) err("non-unique compiler op variants (e.g. Var, T args) are not yet supported")
      for (o <- compilerOps) {
        if (Impls(o).isInstanceOf[Redirect]) {
          stream.println("  " + makeOpMethodSignature(o) + " = " + makeOpMethodNameWithFutureArgs(o, a => if (a.name ==  o.args.apply(0).name) "self" else simpleArgName(a)))
        }
        else {
          stream.println("  " + makeOpMethodSignature(o, withReturnTpe = Some(true)))
        }
      }
      stream.println("}")
      stream.println()
    }
  }
}

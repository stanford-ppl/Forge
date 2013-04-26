package ppl.dsl.forge
package templates
package compiler

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._

import core._
import shared.BaseGenOps
import Utilities._

trait DeliteGenOps extends BaseGenOps {  
  this: ForgeCodeGenDelite =>
  
  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  def baseOpsCls(opsGrp: DSLOps) = {
    if (opsGrp.ops.exists(_.style == compiler)) opsGrp.grp.name + "CompilerOps" 
    else opsGrp.name    
  }
  def baseExpCls(grp: Rep[DSLGroup]) = {
    // in order of decreasing inclusiveness
    if (grpIsTpe(grp) && DeliteCollections.contains(grpAsTpe(grp)) && DataStructs.contains(grpAsTpe(grp))) "DeliteCollectionOpsExp with DeliteStructsExp"
    else if (grpIsTpe(grp) && DeliteCollections.contains(grpAsTpe(grp))) "DeliteCollectionOpsExp"
    else if (grpIsTpe(grp) && DataStructs.contains(grpAsTpe(grp))) "DeliteStructsExp"
    else if (OpsGrp.exists(g => g._2.ops.exists(o => o.effect != pure))) "BaseFatExp with EffectExp"
    else "BaseFatExp"
  }
  
  override def quote(x: Exp[Any]): String = x match {
    case Def(PrintLines(p,lines)) if quoteLiterally => super.quote(x)
    case Def(PrintLines(p,lines)) =>
      val body = lines.flatMap(l => quote(l).split(nl))      
      // how do we decide whether to add stream.println?
      def addPrint(s: String) = {
        // hack! (or heuristic, if the glass is half full)
        !s.startsWith("emit")
      }      
      val body2 = body map { l => if (addPrint(l)) "stream.println("+l+")" else l }      
      val result = ("stream.println(\"val \"+quote(sym)+\" = {\")" :: body2) :+ "stream.println(\"}\")"
          
      // add indentation and newline
      nl + result.map(r => (" "*6)+r).mkString(nl)
      
    case Def(QuoteBlockResult(name,args,ret)) =>
      "emitBlock(" + name + ")" + nl + 
      "quote(getBlockResult(" + name + "))"
 
    case Def(QuoteSeq(argName)) => "Seq("+unquotes(argName+".map(quote).mkString("+quotes(",")+")")+")"
    
    case Const(s: String) if quoteLiterally => s  // no quotes, wildcards will be replaced later in inline
    case Const(s: String) => replaceWildcards(super.quote(s))  // quote first, then insert wildcards
    
    case _ => super.quote(x)
  }  
  
  // IR node names
  def makeOpNodeName(o: Rep[DSLOp]) = {
    val i = nameClashId(o)    
    o.style match {
      case `static` => o.grp.name + i + "Object_" + sanitize(o.name).capitalize
      case `compiler` => o.name.capitalize
      case _ => o.grp.name + i + "_" + sanitize(o.name).capitalize
    }
  }  
  
  def makeOpSimpleNodeNameWithArgs(o: Rep[DSLOp]) = makeOpNodeName(o) + makeOpArgs(o)
  def makeOpSimpleNodeNameWithAnonArgs(o: Rep[DSLOp]) = makeOpNodeName(o) + makeOpAnonArgs(o)  
  def makeOpNodeNameWithArgs(o: Rep[DSLOp], makeArgs: Rep[DSLOp] => String = makeOpArgs) = makeOpNodeName(o) + makeTpePars(o.tpePars) + makeArgs(o) + makeOpImplicitArgs(o)
  
  // TODO: tpeArg should be a List that is the same length as the tpePars in hkTpe
  def makeTpeInst(hkTpe: Rep[DSLType], tpeArg: Rep[DSLType]) = hkTpe match {
    case Def(Tpe(s,Nil,stage)) => s // rather lenient, might get strange results in an improperly specified dsl
    case Def(Tpe(s,List(z),stage)) => s + "[" + quote(tpeArg) + "]"
    case Def(Tpe(s,args,stage)) => err("tried to instantiate tpe " + hkTpe.name + " with arg " + tpeArg.name + ", but " + hkTpe.name + " requires " + args.length + " type parameters")
  }  
  
  def makeTpeClsPar(b: TypeClass, t: Rep[DSLType]) = {
    val body = opIdentifierPrefix + "." + b.prefix + t.name
    b.wrapper match {
      case Some(w) => w + "(" + body + ")"
      case None => body      
    }
  }
        
  def emitImpls(opsGrp: DSLOps, stream: PrintWriter) {
    emitBlockComment("SingleTask and Composite Impls", stream)   
    stream.println()
    stream.println("trait " + opsGrp.name + "Impl {")
    stream.println("  this: " + dsl + "Compiler with " + dsl + "Lift => ")
    stream.println()    
    emitSingleTaskImplMethods(opsGrp, stream, 2)
    emitCompositeImplMethods(opsGrp, stream, 2)
    stream.println("}")
  }
  
  def emitOpExp(opsGrp: DSLOps, stream: PrintWriter) {
    emitBlockComment("IR Definitions", stream)   
    stream.println()
    
    stream.println("trait " + opsGrp.name + "Exp extends " + baseOpsCls(opsGrp) + " with " + baseExpCls(opsGrp.grp) + " {")
    stream.println("  this: " + dsl + "Exp => ")
    stream.println()
       
    val uniqueOps = unique(opsGrp.ops)
    
    emitIRNodes(uniqueOps, stream)
    stream.println()
    emitNodeConstructors(uniqueOps, stream)
    stream.println()
    emitSyms(uniqueOps, stream)
    stream.println()
    emitAliasInfo(uniqueOps, stream)    
    stream.println()
    emitMirrors(uniqueOps, stream)
    stream.println()    
    emitDeliteCollection(opsGrp.grp, stream)   
    stream.println()
    emitStructMethods(opsGrp.grp, stream)   
    stream.println("}")      
  }
      
  def emitIRNodes(uniqueOps: List[Rep[DSLOp]], stream: PrintWriter) {
    def hasIRNode(o: Rep[DSLOp]) = Impls(o) match {
      case _:Composite | _:Getter | _:Setter => false
      case _ => true
    }
    
    def emitOpNodeHeader(o: Rep[DSLOp], opStr: String) {
      stream.println(" extends " + opStr + " {") 
      for (targ <- o.tpePars) {
        for (b <- targ.ctxBounds) {
          stream.println("   val " + b.prefix + targ.name + " = implicitly[" + b.name + "[" + targ.name + "]]")
        }
      }
    }
    
    def emitOpNodeFooter(o: Rep[DSLOp]) {
      stream.println("  }")        
    }
    
    // IR nodes
    for (o <- uniqueOps if hasIRNode(o)) { 
      stream.print("  case class " + makeOpNodeName(o) + makeTpeParsWithBounds(o.tpePars))
      if (Impls(o).isInstanceOf[CodeGen]) stream.print(makeOpArgsWithType(o, blockify))
      else stream.print(makeOpArgsWithType(o))    
      stream.print(makeOpImplicitArgsWithType(o,true))
      
      Impls(o) match {
        case codegen:CodeGen =>           
          emitOpNodeHeader(o, "Def[" + quote(o.retTpe) + "]") 
        case single:SingleTask =>
          emitOpNodeHeader(o, "DeliteOpSingleTask[" + quote(o.retTpe) + "](reifyEffectsHere("+makeOpImplMethodNameWithArgs(o)+"))")
        case Allocates(tpe,init) =>
          emitOpNodeHeader(o, "DeliteStruct[" + quote(o.retTpe) + "]")
          val data = DataStructs(tpe)
          val elemsPure = data.fields.zip(init) map { case ((name,t),i) => ("\""+name+"\"", inline(o,i,quoteLiteral)) }
          val elems = if (o.effect == mutable) elemsPure map { case (k,v) => (k, "var_new("+v+").e") } else elemsPure
          stream.println("    val elems = copyTransformedElems(collection.Seq(" + elems.mkString(",") + "))")
        case map:Map =>
          val colTpe = getHkTpe(o.retTpe)
          val dc = DeliteCollections(colTpe)
          emitOpNodeHeader(o, "DeliteOpMap[" + quote(map.tpePars._1) + "," + quote(map.tpePars._2) + "," + makeTpeInst(colTpe, map.tpePars._2) + "]")            
          stream.println()
          stream.println("    val in = " + o.args.apply(map.argIndex).name)
          stream.println("    def func = " + inline(o,map.func,quoteLiteral))
          // TODO: this isn't quite right. how do we know which of dc.allocs tpePars is the one that corresponds to our return tpe, e.g. map.tpePars._2?
          stream.println("    override def alloc(len: Exp[Int]) = " + makeOpMethodName(dc.alloc) + makeTpePars(instTpePar(dc.alloc.tpePars, map.tpePars._1, map.tpePars._2)) + "(len)")
          stream.println("    val size = copyTransformedOrElse(_.size)(" + makeOpMethodNameWithArgs(dc.size) + ")")          
        case zip:Zip => 
          val colTpe = getHkTpe(o.retTpe)
          val dc = DeliteCollections(colTpe)
          emitOpNodeHeader(o, "DeliteOpZipWith[" + quote(zip.tpePars._1) + "," + quote(zip.tpePars._2) + "," + quote(zip.tpePars._3) + "," + makeTpeInst(colTpe,zip.tpePars._3) + "]")            
          stream.println()
          stream.println("    val inA = " + o.args.apply(zip.argIndices._1).name)
          stream.println("    val inB = " + o.args.apply(zip.argIndices._2).name)
          stream.println("    def func = " + inline(o,zip.func,quoteLiteral))
          stream.println("    override def alloc(len: Exp[Int]) = " + makeOpMethodName(dc.alloc) + makeTpePars(instTpePar(dc.alloc.tpePars, zip.tpePars._1, zip.tpePars._3)) + "(len)")
          stream.println("    val size = copyTransformedOrElse(_.size)(" + makeOpMethodNameWithArgs(dc.size) + ")")
        case reduce:Reduce =>
          val colTpe = getHkTpe(reduce.tpePars._2)
          val dc = DeliteCollections(colTpe)
          emitOpNodeHeader(o, "DeliteOpReduce[" + quote(reduce.tpePars._1) + "]")            
          stream.println()
          stream.println("    val in = " + o.args.apply(reduce.argIndex).name)
          stream.println("    def func = " + inline(o,reduce.func,quoteLiteral))
          stream.println("    def zero = " + makeOpMethodNameWithArgs(reduce.zero))
          stream.println("    val size = copyTransformedOrElse(_.size)(" + makeOpMethodNameWithArgs(dc.size) + ")")
        case filter:Filter =>
          val colTpe = getHkTpe(o.retTpe)
          val dc = DeliteCollections(colTpe)
          emitOpNodeHeader(o, "DeliteOpFilter[" + quote(filter.tpePars._1) + "," + quote(filter.tpePars._2) + "," + quote(colTpe) + "]")            
          stream.println()
          stream.println("    val in = " + o.args.apply(filter.argIndex).name)
          stream.println("    def cond = " + inline(o,filter.cond,quoteLiteral))
          stream.println("    def func = " + inline(o,filter.func,quoteLiteral))
          stream.println("    override def alloc(len: Exp[Int]) = " + makeOpMethodName(dc.alloc) + makeTpePars(instTpePar(dc.alloc.tpePars, filter.tpePars._1, filter.tpePars._2)) + "(len)")
          stream.println("    val size = copyTransformedOrElse(_.size)(" + makeOpMethodNameWithArgs(dc.size) + ")")                          
        case foreach:Foreach =>
          val colTpe = getHkTpe(foreach.tpePars._2)
          val dc = DeliteCollections(colTpe)
          emitOpNodeHeader(o, "DeliteOpForeach[" + quote(foreach.tpePars._1) + "]")            
          stream.println()
          stream.println("    val in = " + o.args.apply(foreach.argIndex).name)
          stream.println("    def func = " + inline(o,foreach.func,quoteLiteral))
          stream.println("    def sync = n => unit(List())")
          stream.println("    val size = copyTransformedOrElse(_.size)(" + makeOpMethodNameWithArgs(dc.size) + ")")                  
      }
      emitOpNodeFooter(o)        
      stream.println()        
    }      
    stream.println()    
  }
  
  def emitNodeConstructors(uniqueOps: List[Rep[DSLOp]], stream: PrintWriter) {
    // methods that construct nodes
    for (o <- uniqueOps) { 
      stream.println("  " + makeOpMethodSignature(o) + " = {")
      val summary = scala.collection.mutable.ArrayBuffer[String]()
      // TODO: we need syms methods for func args..
      if (Impls(o).isInstanceOf[CodeGen]) {
        for (arg <- o.args) {
          arg match {
            case Def(Arg(name, Def(FTpe(args,ret,freq)), d2)) =>
              stream.println()
              emitWithIndent("val b_" + name + " = reifyEffects(" + name + ")", stream, 4)
              emitWithIndent("val sb_" + name + " = summarizeEffects(b_" + name + ")", stream, 4)
              summary += "sb_"+name
            case _ =>
          }
        }
      }
      
      def summarizeEffects(s: scala.collection.mutable.ArrayBuffer[String]): String = {
        if (s.length == 0) ""
        else {
          val rest = summarizeEffects(s.tail)
          if (rest == "") s.head            
          else s.head + " andThen ((" + rest + " andThen " + s.head + ").star)"             
        }
      }
      
      def makeOpNodeNameWithModifiedArgs(o: Rep[DSLOp]) = {
        makeOpNodeNameWithArgs(o, o => "(" + o.args.map(t => t match {
          case Def(Arg(name, Def(FTpe(args,ret,freq)), d2)) => "b_" + name
          case Def(Arg(name, _, _)) => name
          case _ => err("making an op name with modified args that isn't an arg") 
        }).mkString(",") + ")")
      }

      val hasEffects = summary.length > 0
      
      // composite ops, getters and setters are currently inlined
      // in the future, to support pattern matching and optimization, we should implement these as abstract IR nodes and use lowering transformers
      Impls(o) match {
        case c:Composite if hasEffects => err("cannot have effects with composite ops currently")
        case c:Composite => emitWithIndent(makeOpImplMethodNameWithArgs(o), stream, 4)
        case g@Getter(structArgIndex,field) => 
          val fieldTpe = DataStructs(o.args.apply(structArgIndex).tpe).fields.find(t => t._1 == field).get.tpe
          emitWithIndent("field["+quote(fieldTpe)+"]("+inline(o,quotedArg(structArgIndex),quoteLiteral)+",\""+field+"\")", stream, 4)        
        case s@Setter(structArgIndex,field,value) => 
          val fieldTpe = DataStructs(o.args.apply(structArgIndex).tpe).fields.find(t => t._1 == field).get.tpe
          emitWithIndent("field_update["+quote(fieldTpe)+"]("+inline(o,quotedArg(structArgIndex),quoteLiteral)+",\""+field+"\","+inline(o,value,quoteLiteral)+")", stream, 4)        
        case _ if hasEffects =>
          // if (o.effect != simple) { err("don't know how to generate non-simple effects with functions") }
          val prologue = if (o.effect == simple) " andAlso Simple()" else ""
          emitWithIndent(makeEffectAnnotation(simple,o) + "(" + makeOpNodeNameWithModifiedArgs(o) + ", " + summarizeEffects(summary) + prologue + ")", stream, 4)
        case _ => 
          stream.print("    " + makeEffectAnnotation(o.effect,o) + "(" + makeOpNodeNameWithArgs(o) + ")")        
      }
      
      emitWithIndent("}", stream, 2)
    }    
  }
  
  def emitSyms(uniqueOps: List[Rep[DSLOp]], stream: PrintWriter) {
    if (uniqueOps.exists(o => o.args.exists(t => t match { case Def(Arg(_, Def(FTpe(a,b,freq)), _)) => true; case _ => false}))) {
      emitBlockComment("Syms", stream, indent=2)
      
      var symsBuf      = "override def syms(e: Any): List[Sym[Any]] = e match {" + nl
      var boundSymsBuf = "override def boundSyms(e: Any): List[Sym[Any]] = e match {" + nl
      var symsFreqBuf  = "override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {" + nl 
      
      def makeSym(o: Rep[DSLOp], wrap: String, addFreq: Boolean = false) = {
        val symsArgs = o.args.collect { case Def(Arg(name, Def(FTpe(args,ret,freq)), d2)) => (freq,name) }
        if (symsArgs.length > 0) {
          val symsArgsStr = symsArgs.map { case (f,name) => wrap + (if (addFreq) makeFrequencyAnnotation(f) else "") + "(" + name + ")" }.mkString(":::")
          "    case " + makeOpSimpleNodeNameWithArgs(o) + " => " + symsArgsStr + nl
        }
        else ""
      }
            
      for (o <- uniqueOps if Impls(o).isInstanceOf[CodeGen]) { 
        symsBuf += makeSym(o, "syms") 
        boundSymsBuf += makeSym(o, "effectSyms") 
        symsFreqBuf += makeSym(o, "", addFreq = true) 
      }
    
      symsBuf      += "    case _ => super.syms(e)" + nl + "  }"
      boundSymsBuf += "    case _ => super.boundSyms(e)" + nl + "  }"
      symsFreqBuf  += "    case _ => super.symsFreq(e)" + nl + "  }"
      
      for (buf <- List(symsBuf,boundSymsBuf,symsFreqBuf)) emitWithIndent(buf,stream,2)                  
    }
  }
  
  def emitAliasInfo(uniqueOps: List[Rep[DSLOp]], stream: PrintWriter) {
    if (uniqueOps.exists(o => o.aliasHint != nohint)) {          
      emitBlockComment("Aliases / Sharing", stream, indent=2)
      
      var aliasBuf    = "override def aliasSyms(e: Any): List[Sym[Any]] = e match {" + nl
      var containBuf  = "override def containSyms(e: Any): List[Sym[Any]] = e match {" + nl
      var extractBuf  = "override def extractSyms(e: Any): List[Sym[Any]] = e match {" + nl
      var copyBuf     = "override def copySyms(e: Any): List[Sym[Any]] = e match {" + nl
      
      def makeAliasAnnotation(o: Rep[DSLOp], args: List[Int]) = {
        val rhs = if (args == Nil) "Nil" else args.map(i => "syms(" + o.args.apply(i).name + ")").mkString(":::")        // TODO
        "    case " + makeOpSimpleNodeNameWithArgs(o) + " => " + rhs + nl
      }
      
      def makeAllAliasAnnotations(o: Rep[DSLOp], aliasSyms: Option[List[Int]], containSyms: Option[List[Int]], extractSyms: Option[List[Int]], copySyms: Option[List[Int]]) = {
        aliasSyms.foreach   { l => aliasBuf   += makeAliasAnnotation(o,l) }
        containSyms.foreach { l => containBuf += makeAliasAnnotation(o,l) }
        extractSyms.foreach { l => extractBuf += makeAliasAnnotation(o,l) }
        copySyms.foreach    { l => copyBuf    += makeAliasAnnotation(o,l) }                    
      }
      
      for (o <- uniqueOps if o.aliasHint != nohint) {
        o.aliasHint match {
          case AliasCopies(z) =>
            if (o.args.length == z.length) makeAllAliasAnnotations(o, Some(Nil), Some(Nil), Some(Nil), Some(z)) // == aliasesNone
            else makeAllAliasAnnotations(o, None, None, None, Some(z))
            
          case AliasInfo(al,co,ex,cp) => makeAllAliasAnnotations(o,al,co,ex,cp)
        }
      }
              
      aliasBuf   += "    case _ => super.aliasSyms(e)" + nl + "  }"
      containBuf += "    case _ => super.containSyms(e)" + nl + "  }"
      extractBuf += "    case _ => super.extractSyms(e)" + nl + "  }"
      copyBuf    += "    case _ => super.copySyms(e)" + nl + "  }"
      
      for (buf <- List(aliasBuf,containBuf,extractBuf,copyBuf)) emitWithIndent(buf,stream,2)                  
    }
  }
  
  def emitMirrors(uniqueOps: List[Rep[DSLOp]], stream: PrintWriter) {
    emitBlockComment("Mirroring", stream, indent=2)
    stream.println("  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {")
          
    for (o <- uniqueOps) {
      // helpful identifiers
      val xformArgs = "(" + o.args.zipWithIndex.map(t => "f(" + opArgPrefix + t._2 + ")").mkString(",") + ")" 
      val implicits = (o.tpePars.flatMap(t => t.ctxBounds.map(b => makeTpeClsPar(b,t))) ++ o.implicitArgs.zipWithIndex.map(t => opIdentifierPrefix + "." + implicitOpArgPrefix + t._2)).mkString(",")
      
      Impls(o) match {
        case codegen:CodeGen =>
          stream.print("    case " + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithAnonArgs(o) + " => ")
          // pure version with no func args uses smart constructor
          if (!hasFuncArgs(o)) {
            stream.print(makeOpMethodName(o) + xformArgs)
            stream.print("(" + implicits)          
            // we may need to supply an explicit Overload parameter for the smart constructor
            // relies on conventions established in implicitArgsWithOverload (e.g., guaranteed to always be the last implicit)
            val id = nameClashId(o)
            if (id != "") stream.print(",implicitly[Overloaded" + id + "]")  
            stream.println(")")
          }
          else {
            stream.print("reflectPure(" + makeOpNodeName(o) + xformArgs + "(" + implicits + "))")
            stream.println("(mtype(manifest[A]), pos)")
          }
          
          // effectful version
          stream.print("    case Reflect(" + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithAnonArgs(o) + ", u, es) => reflectMirrored(Reflect(" + makeOpNodeName(o) + xformArgs + "(" + implicits + ")")
          stream.print(", mapOver(f,u), f(es)))")
          stream.println("(mtype(manifest[A]))")
        case _:DeliteOpType => 
          // pure delite op version
          stream.print("    case " + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithAnonArgs(o) + " => ")
          stream.print("reflectPure(new { override val original = Some(f," + opIdentifierPrefix + ") } with " + makeOpNodeName(o) + xformArgs + "(" + implicits + "))")
          stream.println("(mtype(manifest[A]), pos)")
          // effectful delite op version
          stream.print("    case Reflect(" + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithAnonArgs(o) + ", u, es) => reflectMirrored(Reflect(new { override val original = Some(f," + opIdentifierPrefix + ") } with " + makeOpNodeName(o) + xformArgs + "(" + implicits + ")")
          stream.print(", mapOver(f,u), f(es)))")
          stream.println("(mtype(manifest[A]))")
        case _ => // no mirror  
      }        
    }
    stream.println("    case _ => super.mirror(e, f)")
    stream.println("  }).asInstanceOf[Exp[A]]")
  }
  
  def emitDeliteCollection(grp: Rep[DSLGroup], stream: PrintWriter) {
    try {
      val tpe = grpAsTpe(grp)
      if (DeliteCollections.contains(tpe)) {
        emitBlockComment("Delite collection", stream, indent=2)
        val dc = DeliteCollections(tpe)        
        val isTpe = "is"+tpe.name
        def asTpe = "as"+tpe.name
        stream.println("  def " + isTpe + "[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf["+makeTpeInst(tpe, tpePar("A"))+"])")
        stream.println("  def " + asTpe + "[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp["+makeTpeInst(tpe, tpePar("A"))+"]]")
        stream.println()
        stream.println("  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = {")
        stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dc.size) + "(" + asTpe + "(x))") 
        stream.println("    else super.dc_size(x)")
        stream.println("  }")
        stream.println()
        stream.println("  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {")
        stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dc.apply) + "(" + asTpe + "(x), n).asInstanceOf[Exp[A]]")
        stream.println("    else super.dc_apply(x,n)")
        stream.println("  }")
        stream.println()
        stream.println("  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {")
        val a = if (dc.tpeArg.tp.runtimeClass == classOf[TypePar]) "A" else quote(dc.tpeArg) // hack!
        stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dc.update) + "(" + asTpe + "(x), n, y.asInstanceOf[Exp["+a+"]])") 
        stream.println("    else super.dc_update(x,n,y)")
        stream.println("  }")
        
        if (dc.isInstanceOf[DeliteCollectionBuffer]) {
          val dcb = dc.asInstanceOf[DeliteCollectionBuffer]
          stream.println()          
          stream.println("  override def dc_parallelization[A:Manifest](x: Exp[DeliteCollection[A]], hasConditions: Boolean)(implicit ctx: SourceContext) = {")
          // stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dcb.parallelization) + "(" + asTpe + "(x), hasConditions)") 
          stream.println("    if (" + isTpe + "(x)) { if (hasConditions) ParSimpleBuffer else ParFlat } // TODO: always generating this right now") 
          stream.println("    else super.dc_parallelization(x, hasConditions)")
          stream.println("  }")          
          stream.println()
          stream.println("  override def dc_set_logical_size[A:Manifest](x: Exp[DeliteCollection[A]], y: Exp[Int])(implicit ctx: SourceContext) = {")
          stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dcb.setSize) + "(" + asTpe + "(x), y)")
          stream.println("    else super.dc_set_logical_size(x,y)")
          stream.println("  }")
          stream.println()
          stream.println("  override def dc_appendable[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {")
          stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dcb.appendable) + "(" + asTpe + "(x), i, y)")
          stream.println("    else super.dc_appendable(x,i,y)")
          stream.println("  }")          
          stream.println()
          stream.println("  override def dc_append[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {")
          stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dcb.append) + "(" + asTpe + "(x), i, y)")
          stream.println("    else super.dc_append(x,i,y)")
          stream.println("  }")          
          stream.println()
          stream.println("  override def dc_alloc[A:Manifest,CA<:DeliteCollection[A]:Manifest](x: Exp[CA], size: Exp[Int])(implicit ctx: SourceContext) = {")
          stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dcb.alloc) + "[A](size).asInstanceOf[Exp[CA]]")
          stream.println("    else super.dc_alloc[A,CA](x,size)")
          stream.println("  }")          
          stream.println()
          stream.println("  override def dc_copy[A:Manifest](src: Exp[DeliteCollection[A]], srcPos: Exp[Int], dst: Exp[DeliteCollection[A]], dstPos: Exp[Int], size: Exp[Int])(implicit ctx: SourceContext) = {")
          stream.println("    if (" + isTpe + "(src) && " + isTpe + "(dst)) " + makeOpMethodName(dcb.copy) + "(" + asTpe + "(src), srcPos, " + asTpe + "(dst), dstPos, size)")
          stream.println("    else super.dc_copy(src,srcPos,dst,dstPos,size)")
          stream.println("  }")                    
        }
      }
    }
    catch { case _ : MatchError => }
  }
  
  def emitStructMethods(grp: Rep[DSLGroup], stream: PrintWriter) {
    def wrapManifest(t: Rep[DSLType]) = t match {
      case Def(Tpe("Array",args,stage)) => "darrayManifest(m.typeArguments(0))"
      case _ => "manifest[" + quote(t) + "]"
    }
    
    try {
      val tpe = grpAsTpe(grp)
      if (DataStructs.contains(tpe)) {
        val d = DataStructs(tpe)
        val fields = d.fields.map { case (fieldName,fieldType) => ("\""+fieldName+"\"", wrapManifest(fieldType)) }
        val erasureCls = makeTpeInst(tpe, tpePar("_"))        
        
        emitBlockComment("Delite struct", stream, indent=2)
        stream.println("  override def unapplyStructType[T:Manifest]: Option[(StructTag[T], List[(String,Manifest[_])])] = {")
        stream.println("    val m = manifest[T]")        
        stream.println("    if (m.erasure == classOf["+erasureCls+"]) Some((classTag(m), collection.immutable.List("+fields.mkString(",")+")))")  
        stream.println("    else super.unapplyStructType(m)")
        stream.println("  }")
      }
    }
    catch { case _ : MatchError => }
  }
    
  def emitOpCodegen(opsGrp: DSLOps, stream: PrintWriter) {        
    val rules = unique(opsGrp.ops).map(o => (o,Impls(o))).filter(_._2.isInstanceOf[CodeGen])
    if (rules.length > 0){
      emitBlockComment("Code generators", stream)   
      stream.println()
      for (g <- generators) { 
        val generatorRules = rules.flatMap{case (o,i) => i.asInstanceOf[CodeGen].decls.collect{case (k,r) if (k == g) => (o,r)}}
        if (generatorRules.length > 0) {
          stream.println("trait " + g.name + "Gen" + opsGrp.name + " extends " + g.name + "GenFat {")
          stream.println("  val IR: " + opsGrp.name + "Exp")
          stream.println("  import IR._")
          stream.println()
          stream.println("  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {")
          for ((op,r) <- generatorRules) {
            if (r.isSimple)
              stream.println("    case " + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithArgs(op) + " => emitValDef(sym, " + quote(r.decl) + ")")
            else 
              stream.println("    case " + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithArgs(op) + " => " + quote(r.decl))
          }
          stream.println("    case _ => super.emitNode(sym, rhs)")
          stream.println("  }")
          stream.println("}")
        }          
      }
    }      
  }  
}

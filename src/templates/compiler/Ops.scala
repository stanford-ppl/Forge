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

  def baseExpCls(grp: Rep[DSLGroup]) = {
    // in order of decreasing inclusiveness
    if (grp.isInstanceOf[Rep[DSLType]] && DeliteCollections.contains(grp.asInstanceOf[Rep[DSLType]])) "DeliteCollectionOpsExp"
    else if (OpsGrp.exists(g => g._2.ops.exists(o => o.effect != pure))) "BaseFatExp with EffectExp"
    else "BaseFatExp"
  }
  
  override def quote(x: Exp[Any]): String = x match {
    case Def(PrintLines(p,lines)) =>
      val body = lines.flatMap(l => quote(l).split(nl))      
      // how do we decide whether to add stream.println?
      def addPrint(s: String) = {
        // hack! (or heuristic, if the glass is half full)
        !s.startsWith("emit")
      }      
      val body2 = body map { l => if (addPrint(l)) "stream.println("+l+")" else l }      
      val result = ("stream.println(\"val " + unquotes("quote(sym)") + " = {\")" :: body2) :+ "stream.println(\"}\")"
          
      // add indentation and newline
      nl + result.map(r => (" "*6)+r).mkString(nl)
      
    case Def(QuoteBlockResult(name,args,ret)) =>
      "emitBlock(" + name + ")" + nl + 
      "quote(getBlockResult(" + name + "))" 
      
    case Const(s: String) => replaceWildcards(super.quote(s)) // quote first, then insert wildcards
    
    case _ => super.quote(x)
  }  
  
  // IR node names
  def makeOpNodeName(o: Rep[DSLOp]) = {
    val i = nameClashId(o)    
    o.style match {
      case `static` => o.grp.name + i + "Object_" + o.name.capitalize
      case _ => o.grp.name + i + "_" + o.name.capitalize
    }
  }  
  
  def makeOpSimpleNodeNameWithArgs(o: Rep[DSLOp]) = makeOpNodeName(o) + makeOpArgs(o)
  def makeOpNodeNameWithArgs(o: Rep[DSLOp], makeArgs: Rep[DSLOp] => String = makeOpArgs) = makeOpNodeName(o) + makeTpePars(o.tpePars) + makeArgs(o) + makeOpImplicitArgs(o)
    
  def emitOpExp(ops: DSLOps, stream: PrintWriter) {
    emitBlockComment("IR Definitions", stream)   
    stream.println()
    stream.println("trait " + ops.grp.name + "OpsExp extends " + ops.grp.name + "Ops with " + baseExpCls(ops.grp) + " {")
    stream.println("  this: " + dsl + "Exp => ")
    stream.println()
    
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
    for (o <- unique(ops.ops)) { 
      stream.print("  case class " + makeOpNodeName(o) + makeTpeParsWithBounds(o.tpePars))
      if (o.opTpe == codegenerated) stream.print(makeOpArgsWithType(o, blockify))
      else stream.print(makeOpArgsWithType(o))    
      stream.print(makeOpImplicitArgsWithType(o,true))
      
      o.opTpe match {
        case `codegenerated` =>           
          emitOpNodeHeader(o, "Def[" + quote(o.retTpe) + "]") 
        case zip:Zip => 
          check(zip, o)          
          val dc = DeliteCollections(o.args.apply(0))
          emitOpNodeHeader(o, "DeliteOpZipWith[" + quote(zip.tpePars._1) + "," + quote(zip.tpePars._2) + "," + quote(zip.tpePars._3) + "," + quote(zip.tpePars._4) + "]")            
          stream.println()
          stream.println("   val inA = " + opArgPrefix + zip.argIndices._1)
          stream.println("   val inB = " + opArgPrefix + zip.argIndices._2)
          stream.println("   def func = " + zip.func)
          stream.println("   override def alloc(len: Exp[Int]) = " + makeOpMethodName(dc.alloc) + "(len)")
          stream.println("   val size = copyTransformedOrElse(_.size)(" + makeOpMethodNameWithArgs(dc.size) + ")")
      }
      emitOpNodeFooter(o)        
      stream.println()        
    }      
    stream.println()
    
    // methods that construct nodes
    for (o <- unique(ops.ops)) { 
      stream.print("  def " + makeOpMethodName(o) + makeTpeParsWithBounds(o.tpePars))
      stream.print(makeOpArgsWithType(o))
      stream.print(makeOpImplicitArgsWithOverloadWithType(o))
      stream.print(" = {")
      val summary = scala.collection.mutable.ArrayBuffer[String]()
      // TODO: we need syms methods for func args..
      if (o.opTpe == codegenerated) {
        for ((arg,i) <- o.args.zipWithIndex) {
          arg match {
            case Def(FTpe(args, ret)) =>
              stream.println()
              emitWithIndent("val b_" + i + " = reifyEffects(" + opArgPrefix + i + ")", stream, 4)
              emitWithIndent("val sb_" + i + " = summarizeEffects(b_" + i + ")", stream, 4)
              summary += "sb_"+i
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
        makeOpNodeNameWithArgs(o, o => "(" + o.args.zipWithIndex.map(t => t._1 match {
          case Def(FTpe(args, ret)) => "b_" + t._2
          case _ => opArgPrefix + t._2
        }).mkString(",") + ")")
      }
      
      if (summary.length > 0) {
        if (o.effect != simple) { err("don't know how to generate non-simple effects with functions") }
        emitWithIndent(makeEffectAnnotation(o.effect) + "(" + makeOpNodeNameWithModifiedArgs(o) + ", " + summarizeEffects(summary) + ")", stream, 4)
      }
      else {
        stream.print("    " + makeEffectAnnotation(o.effect) + "(" + makeOpNodeNameWithArgs(o) + ")")
      }
      
      emitWithIndent("}", stream, 2)
    }
    
    stream.println()
    emitSyms(ops, stream)
    stream.println()
    emitMirrors(ops, stream)
    stream.println()    
    emitDeliteCollection(ops.grp, stream)      
    stream.println("}")      
  }
    
  def emitSyms(ops: DSLOps, stream: PrintWriter) {
    if (unique(ops.ops).exists(o => o.args.exists(t => t match { case Def(FTpe(a,b)) => true; case _ => false}))) {
      emitBlockComment("Syms", stream, indent=2)
      
      var symsBuf      = "override def syms(e: Any): List[Sym[Any]] = e match {" + nl
      var boundSymsBuf = "override def boundSyms(e: Any): List[Sym[Any]] = e match {" + nl
      var symsFreqBuf  = "override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {" + nl 
      
      def makeSym(o: Rep[DSLOp], wrap: String) = {
        val symsArgs = o.args.zipWithIndex.collect { case (Def(FTpe(args, ret)), i) => i }
        if (symsArgs.length > 0) {
          val symsArgsStr = symsArgs.map(i => wrap + "(" + opArgPrefix + i + ")").mkString(":::")
          "    case " + makeOpSimpleNodeNameWithArgs(o) + " => " + symsArgsStr + nl
        }
        else ""
      }
            
      for (o <- unique(ops.ops) if o.opTpe == codegenerated) { 
        symsBuf += makeSym(o, "syms") 
        boundSymsBuf += makeSym(o, "effectSyms") 
        symsFreqBuf += makeSym(o, "freqNormal") // TODO: how do we know?
      }
    
      symsBuf      += "    case _ => super.syms(e)" + nl + "  }"
      boundSymsBuf += "    case _ => super.boundSyms(e)" + nl + "  }"
      symsFreqBuf  += "    case _ => super.symsFreq(e)" + nl + "  }"
      
      for (buf <- List(symsBuf,boundSymsBuf,symsFreqBuf)) emitWithIndent(buf,stream,2)                  
    }
  }
  
  def emitMirrors(ops: DSLOps, stream: PrintWriter) {
    emitBlockComment("Mirroring", stream, indent=2)
    stream.println("  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {")
    for (o <- unique(ops.ops)) {
      // helpful identifiers
      val xformArgs = "(" + o.args.zipWithIndex.map(t => "f(" + opArgPrefix + t._2 + ")").mkString(",") + ")" 
      val implicits = (o.tpePars.flatMap(t => t.ctxBounds.map(b => opIdentifierPrefix + "." + b.prefix + t.name)) ++ /*implicitArgsWithOverload(o)*/o.implicitArgs.zipWithIndex.map(t => opIdentifierPrefix + "." + implicitOpArgPrefix + t._2)).mkString(",")
      
      o.opTpe match {
        case `codegenerated` =>
          stream.print("    case " + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithArgs(o) + " => ")
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
          stream.print("    case Reflect(" + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithArgs(o) + ", u, es) => reflectMirrored(Reflect(" + makeOpNodeName(o) + xformArgs + "(" + implicits + ")")
          stream.print(", mapOver(f,u), f(es)))")
          stream.println("(mtype(manifest[A]))")
        case _:DeliteOpType => 
          // pure delite op version
          stream.print("    case " + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithArgs(o) + " => ")
          stream.print("reflectPure(new { override val original = Some(f," + opIdentifierPrefix + ") } with " + makeOpNodeName(o) + xformArgs + "(" + implicits + "))")
          stream.println("(mtype(manifest[A]), pos)")
          // effectful delite op version
          stream.print("    case Reflect(" + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithArgs(o) + ", u, es) => reflectMirrored(Reflect(new { override val original = Some(f," + opIdentifierPrefix + ") } with " + makeOpNodeName(o) + xformArgs + "(" + implicits + ")")
          stream.print(", mapOver(f,u), f(es)))")
          stream.println("(mtype(manifest[A]))")
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
        stream.println("  def " + isTpe + "[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf["+tpe.name+"[A]])")
        stream.println("  def " + asTpe + "[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp["+tpe.name+"[A]]]")
        stream.println()
        stream.println("  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = {")
        stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dc.size) + "(" + asTpe + "(x))") 
        stream.println("    else super.dc_size(x)")
        stream.println("  }")
        stream.println()
        stream.println("  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {")
        stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dc.apply) + "(" + asTpe + "(x), n)")
        stream.println("    else super.dc_apply(x,n)")
        stream.println("  }")
        stream.println()
        stream.println("  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {")
        stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(dc.update) + "(" + asTpe + "(x), n, y)") 
        stream.println("    else super.dc_update(x,n,y)")
        stream.println("  }")
      }
    }
    catch { case _ => }
  }
  
  def emitOpCodegen(ops: DSLOps, stream: PrintWriter) {        
    if (CodeGenRules(ops.grp).length > 0){
      val uniqueOps = unique(ops.ops)
      emitBlockComment("Code generators", stream)   
      stream.println()
      for (g <- generators) { 
        val rules = CodeGenRules(ops.grp).filter(_.generator == g)
        if (rules.length > 0) {
          stream.println("trait " + g.name + "Gen" + ops.grp.name + "Ops extends " + g.name + "GenFat {")
          stream.println("  val IR: " + ops.grp.name + "OpsExp")
          stream.println("  import IR._")
          stream.println()
          stream.println("  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {")
          for (r <- rules if uniqueOps.contains(r.op)) {
            if (r.isSimple)
              stream.println("    case " + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithArgs(r.op) + " => emitValDef(sym, " + quote(r.rule) + ")")
            else 
              stream.println("    case " + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithArgs(r.op) + " => " + quote(r.rule))
          }
          stream.println("    case _ => super.emitNode(sym, rhs)")
          stream.println("  }")
          stream.println("}")
        }          
      }
    }      
  }  
}

package ppl.dsl.meta
package templates

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._
import core._
import Imports._
import Utilities._

object Ops {
   
  trait ScalaGenOps extends MetaDSLCodeGenBase with ScalaGenBase {  
    val IR: MetaDSLApplicationRunner with MetaDSLExp with MetaDSLOpsExp
    import IR._

    def baseOpsCls(tpe: Rep[DSLType]) = if (lmsAppOps contains variableOps) "Variables" else "Base"
    def baseExpCls(tpe: Rep[DSLType]) = {
      // in order of decreasing inclusiveness
      if (DeliteCollections.contains(tpe)) "DeliteCollectionOpsExp"
      else if (lmsAppOps contains variableOps) "VariablesExp with BaseFatExp" 
      else if (OpsGrp.exists(g => g._2.ops.exists(o => o.effect != pure))) "BaseFatExp with EffectExp"
      else "BaseFatExp"
    }
    
    def makeOpArgs(o: Rep[DSLOp]) = "(" + o.args.zipWithIndex.map(t => opArgPrefix + t._2).mkString(",") + ")"
    def makeOpArgsWithType(o: Rep[DSLOp]) = "(" + o.args.zipWithIndex.map(t => opArgPrefix + t._2 + ": " + repify(t._1)).mkString(",") + ")"
    def makeOpImplicitArgs(o: Rep[DSLOp]) = 
      if (o.implicitArgs.length > 0) "(" + o.implicitArgs.zipWithIndex.map(t => implicitOpArgPrefix + t._2).mkString(",") + ")"
      else ""
    def makeOpImplicitArgsWithType(o: Rep[DSLOp], asVals: Boolean = false) = {
      val prefix = if (asVals == true) "val " else ""
      if (o.implicitArgs.length > 0) "(implicit " + o.implicitArgs.zipWithIndex.map(t => prefix + implicitOpArgPrefix + t._2 + ": " + t._1.name).mkString(",") + ")"
      else ""
    }
    
    def makeOpMethodName(tpe: Rep[DSLType], o: Rep[DSLOp]) = o.style match {
      case `static` => tpe.name.toLowerCase + "_object_" + o.name.toLowerCase
      case _ => tpe.name.toLowerCase + "_" + o.name.toLowerCase
    }
    def makeOpMethodNameWithArgs(tpe: Rep[DSLType], o: Rep[DSLOp]) = makeOpMethodName(tpe,o) + makeOpArgs(o)
    def makeOpNodeName(tpe: Rep[DSLType], o: Rep[DSLOp]) = o.style match {
      case `static` => tpe.name + "Object_" + o.name.capitalize
      case _ => tpe.name + "_" + o.name.capitalize
    }    
    def makeOpNodeNameWithArgs(tpe: Rep[DSLType], o: Rep[DSLOp]) = makeOpNodeName(tpe,o) + makeOpArgs(o)
    
    def emitOpSyntax(dsl: String, tpe: Rep[DSLType], ops: DSLOps, stream: PrintWriter) {
      emitBlockComment("Operations", stream)
      stream.println()
      stream.println("trait " + tpe.name + "Ops extends " + baseOpsCls(tpe) + " {")
      stream.println("  this: " + dsl + " => ")
      stream.println()
      
      // static ops
      val staticOps = ops.ops.filter(e=>e.style==static)
      if (staticOps.length > 0) {
        stream.println("  object " + tpe.name + " {")
        for (o <- staticOps) {
          stream.print("    def " + o.name + makeTpeArgsWithBounds(o.tpeArgs))
          stream.print(makeOpArgsWithType(o))
          stream.print(makeOpImplicitArgsWithType(o))
          stream.print(" = " + makeOpMethodNameWithArgs(tpe,o))
          stream.println()
        }
        stream.println("  }")
      }
      stream.println()
      
      // infix ops
      val infixOps = ops.ops.filter(e=>e.style==infix)
      
      // certain ops (e.g. "apply" cannot be expressed with infix notation right now), so we use implicits as a workaround
      val needPimpClass = (noInfixList intersect infixOps.map(_.name)).nonEmpty
      val pimpStream = new StringBuilder()
      def infix_appendLine(x: StringBuilder, y: String) = x.append(y + System.getProperty("line.separator"))
      if (needPimpClass) {
        // set up a pimp-my-library style promotion
        pimpStream.appendLine("  implicit def repTo" + tpe.name + "Ops" + makeTpeArgsWithBounds(tpe.tpeArgs) + "(x: " + repify(tpe) + ") = new " + tpe.name + "OpsCls(x)")
        if (lmsAppOps.contains(variableOps)) {
          pimpStream.appendLine("  implicit def varTo" + tpe.name + "Ops" + makeTpeArgsWithBounds(tpe.tpeArgs) + "(x: " + varify(tpe) + ") = new " + tpe.name + "OpsCls(readVar(x))")
        }
        pimpStream.appendLine("")
        pimpStream.appendLine("  class " + tpe.name + "OpsCls" + makeTpeArgsWithBounds(tpe.tpeArgs) + "(val " + opArgPrefix + "0: " + repify(tpe) + ") {")
      }
      
      for (o <- infixOps) {
        if (noInfixList.contains(o.name)) {
          val otherArgs = "(" + o.args.drop(1).zipWithIndex.map(t => opArgPrefix + (t._2+1) + ": " + repify(t._1)).mkString(",") + ")"
          pimpStream.appendLine("    def " + o.name + makeTpeArgsWithBounds(o.tpeArgs.diff(tpe.tpeArgs)) + otherArgs + makeOpImplicitArgsWithType(o) + " = " + makeOpMethodNameWithArgs(tpe,o))
        }
        else {
          stream.print("  def infix_" + o.name + makeTpeArgsWithBounds(o.tpeArgs))
          stream.print(makeOpArgsWithType(o))
          stream.print(makeOpImplicitArgsWithType(o))
          stream.println(" = " + makeOpMethodNameWithArgs(tpe,o))        
        }
      }
      stream.println()      
      
      if (needPimpClass) {
        pimpStream.appendLine("  }")
        stream.println(pimpStream)
      }
      
      // abstract methods
      for (o <- ops.ops) {
        stream.print("  def " + makeOpMethodName(tpe,o) + makeTpeArgsWithBounds(o.tpeArgs))
        stream.print(makeOpArgsWithType(o))
        stream.print(makeOpImplicitArgsWithType(o) + ": " + repify(o.retTpe))
        stream.println()
      }
      
      stream.println("}")
    }
    
    def emitOpExp(dsl: String, tpe: Rep[DSLType], ops: DSLOps, stream: PrintWriter) {
      emitBlockComment("IR Definitions", stream)   
      stream.println()
      stream.println("trait " + tpe.name + "OpsExp extends " + baseExpCls(tpe) + " {")
      stream.println("  this: " + dsl + "Exp => ")
      stream.println()
      
      def emitOpNodeHeader(o: Rep[DSLOp], opStr: String) {
        stream.println(" extends " + opStr + " {") 
        for (targ <- o.tpeArgs) {
          for (b <- targ.ctxBounds) {
            stream.println("   val " + b.prefix + targ.name + " = implicitly[" + b.name + "[" + targ.name + "]]")
          }
        }
      }
      
      def emitOpNodeFooter(o: Rep[DSLOp]) {
        stream.println("  }")        
      }
      
      // IR nodes
      for (o <- ops.ops) { 
        stream.print("  case class " + makeOpNodeName(tpe,o) + makeTpeArgsWithBounds(o.tpeArgs))
        stream.print(makeOpArgsWithType(o))
        stream.print(makeOpImplicitArgsWithType(o,true))
        o.opTpe match {
          case `codegenerated` =>           
            emitOpNodeHeader(o, "Def[" + quote(o.retTpe) + "]") 
          case zip:Zip => 
            // sanity checking
            if (zip.tpePars.productIterator.exists(a => a.isInstanceOf[TypeArg] && !o.tpeArgs.contains(a))) err("zipWith op with undefined type arg: " + o.name)
            if (zip.argIndices.productIterator.asInstanceOf[Iterator[Int]].exists(a => a < 0 || a > o.args.length)) err("zipWith op with illegal arg parameter: " + o.name)
            
            emitOpNodeHeader(o, "DeliteOpZipWith[" + quote(zip.tpePars._1) + "," + quote(zip.tpePars._2) + "," + quote(zip.tpePars._3) + "," + quote(zip.tpePars._4) + "]")            
            stream.println()
            stream.println("   val inA = " + opArgPrefix + zip.argIndices._1)
            stream.println("   val inB = " + opArgPrefix + zip.argIndices._2)
            stream.println("   def func = " + zip.func)
            stream.println("   override def alloc(len: Exp[Int]) = " + makeOpMethodName(tpe, DeliteCollections(tpe).alloc) + "(len)")
            stream.println("   val size = copyTransformedOrElse(_.size)(" + makeOpMethodNameWithArgs(tpe, DeliteCollections(o.args.apply(0)).size) + ")")
        }
        emitOpNodeFooter(o)        
        stream.println()        
      }      
      stream.println()
      
      // methods that construct nodes
      for (o <- ops.ops) { 
        stream.print("  def " + makeOpMethodName(tpe,o) + makeTpeArgsWithBounds(o.tpeArgs))
        stream.print(makeOpArgsWithType(o))
        stream.print(makeOpImplicitArgsWithType(o))
        stream.print(" = " + makeEffectAnnotation(o.effect) + "(" + makeOpNodeNameWithArgs(tpe,o) + ")")
        stream.println()
      }
      
      stream.println()
      emitMirrors(tpe, ops, stream)
      stream.println()    
      emitDeliteCollection(tpe, stream)      
      stream.println("}")      
    }
    
    def emitMirrors(tpe: Rep[DSLType], ops: DSLOps, stream: PrintWriter) {
      emitBlockComment("Mirroring", stream, indent=2)
      stream.println("  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {")
      for (o <- ops.ops) {
        // helpful identifiers
        val xformArgs = "(" + o.args.zipWithIndex.map(t => "f(" + opArgPrefix + t._2 + ")").mkString(",") + ")" 
        val implicits = (o.tpeArgs.flatMap(t => t.ctxBounds.map(b => opIdentifierPrefix + "." + b.prefix + t.name)) ++ o.implicitArgs.zipWithIndex.map(t => opIdentifierPrefix + "." + implicitOpArgPrefix + t._2)).mkString(",")
        
        o.opTpe match {
          case `codegenerated` =>
            stream.print("    case " + makeOpNodeNameWithArgs(tpe,o) + " => ")
            // pure version uses smart constructor
            stream.print(makeOpMethodName(tpe,o) + xformArgs)
            stream.println("(mtype(manifest[A]), pos)")            
            // effectful version
            stream.print("    case Reflect(" + opIdentifierPrefix + "@" + makeOpNodeNameWithArgs(tpe,o) + ", u, es) => reflectMirrored(Reflect(" + makeOpNodeName(tpe,o) + xformArgs + "(" + implicits + ")")
            stream.print(", mapOver(f,u), f(es)))")
            stream.println("(mtype(manifest[A]))")
          case _:DeliteOpType => 
            // pure delite op version
            stream.print("    case " + opIdentifierPrefix + "@" + makeOpNodeNameWithArgs(tpe,o) + " => ")
            stream.print("reflectPure(new { override val original = Some(f," + opIdentifierPrefix + ") } with " + makeOpNodeName(tpe,o) + xformArgs + "(" + implicits + "))")
            stream.println("(mtype(manifest[A]), pos)")
            // effectful delite op version
            stream.print("    case Reflect(" + opIdentifierPrefix + "@" + makeOpNodeNameWithArgs(tpe,o) + ", u, es) => reflectMirrored(Reflect(new { override val original = Some(f," + opIdentifierPrefix + ") } with " + makeOpNodeName(tpe,o) + xformArgs + "(" + implicits + ")")
            stream.print(", mapOver(f,u), f(es)))")
            stream.println("(mtype(manifest[A]))")
        }        
      }
      stream.println("    case _ => super.mirror(e, f)")
      stream.println("  }).asInstanceOf[Exp[A]]")
    }
    
    def emitDeliteCollection(tpe: Rep[DSLType], stream: PrintWriter) {
      if (DeliteCollections.contains(tpe)) {
        emitBlockComment("Delite collection", stream, indent=2)
        val dc = DeliteCollections(tpe)        
        val isTpe = "is"+tpe.name
        def asTpe = "as"+tpe.name
        stream.println("  def " + isTpe + "[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf["+tpe.name+"[A]])")
        stream.println("  def " + asTpe + "[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp["+tpe.name+"[A]]]")
        stream.println()
        stream.println("  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = {")
        stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(tpe, dc.size) + "(" + asTpe + "(x))") 
        stream.println("    else super.dc_size(x)")
        stream.println("  }")
        stream.println()
        stream.println("  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {")
        stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(tpe, dc.apply) + "(" + asTpe + "(x), n)")
        stream.println("    else super.dc_apply(x,n)")
        stream.println("  }")
        stream.println()
        stream.println("  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {")
        stream.println("    if (" + isTpe + "(x)) " + makeOpMethodName(tpe, dc.update) + "(" + asTpe + "(x), n, y)") 
        stream.println("    else super.dc_update(x,n,y)")
        stream.println("  }")
      }
    }
    
    def emitOpCodegen(dsl: String, tpe: Rep[DSLType], ops: DSLOps, stream: PrintWriter) {        
      if (CodeGenRules(tpe).length > 0){
        emitBlockComment("Code generators", stream)   
        stream.println()
        for (g <- generators) { 
          val rules = CodeGenRules(tpe).filter(_.generator == g)
          if (rules.length > 0) {
            stream.println("trait " + g.name + "Gen" + tpe.name + "Ops extends " + g.name + "GenFat {")
            stream.println("  val IR: " + tpe.name + "OpsExp")
            stream.println("  import IR._")
            stream.println()
            stream.println("  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {")
            for (r <- rules) {
              stream.println("    case " + opIdentifierPrefix + "@" + makeOpNodeNameWithArgs(r.tpe, r.op) + " => emitValDef(sym, " + r.rule + ")")
            }
            stream.println("    case _ => super.emitNode(sym, rhs)")
            stream.println("  }")
            stream.println("}")
          }          
        }
      }      
    }
    
    def emitOps(dsl: String, path: String) {
      // 1 file per tpe, includes Ops, OpsExp, and Gen      
      for ((tpe,ops) <- OpsGrp) {
        val stream = new PrintWriter(new FileWriter(path+File.separator+tpe.name+"Ops"+".scala"))
        stream.println("package " + dsl.toLowerCase() + ".ops")
        stream.println()
        emitScalaReflectImports(stream)
        emitLMSImports(stream)
        emitDeliteOpsImports(stream)
        emitDSLBaseImports(dsl, stream)
        stream.println()
        emitOpSyntax(dsl, tpe, ops, stream)        
        stream.println()
        emitOpExp(dsl, tpe, ops, stream)
        stream.println()
        emitOpCodegen(dsl, tpe, ops, stream)        
        stream.close()
      }                
    }
  }
  
}

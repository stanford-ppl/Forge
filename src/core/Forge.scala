package ppl.dsl.forge
package core

import java.io.{File,PrintWriter,FileWriter}
import scala.tools.nsc.io.{Directory,Path}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import format._

trait ForgeApplication extends Forge with ForgeLift {
  def dslName: String
  def specification(): Rep[Unit]    
}

/**
 * These are the portions of Scala imported into Forge's scope.
 */
trait ForgeLift extends LiftString with LiftBoolean with LiftNumeric with LiftPrimitives {
  this: Forge =>
}

trait ForgeScalaOpsPkg extends Base
  with ImplicitOps with OrderingOps with StringOps with ArrayOps
  with BooleanOps with PrimitiveOps with TupleOps with CastingOps 

trait ForgeScalaOpsPkgExp extends ForgeScalaOpsPkg 
  with ImplicitOpsExp with OrderingOpsExp with StringOpsExp  with ArrayOpsExp 
  with BooleanOpsExp with PrimitiveOpsExp with TupleOpsExp with CastingOpsExp 

trait ForgeScalaCodeGenPkg extends ScalaGenEffect
  with ScalaGenImplicitOps with ScalaGenOrderingOps with ScalaGenStringOps  with ScalaGenArrayOps 
  with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenTupleOps with ScalaGenCastingOps 
  { val IR: ForgeScalaOpsPkgExp  }


/**
 * This the trait that every Forge application must extend.
 */
trait Forge extends ForgeScalaOpsPkg with Definitions with ForgeOps with FieldOps with QuoteOps with PrinterOps {
  this: ForgeApplication =>
}

/**
 * These are the corresponding IR nodes for Forge.
 */
trait ForgeExp extends Forge with ForgeUtilities with ForgeScalaOpsPkgExp with DefinitionsExp with ForgeOpsExp with FieldOpsExp with QuoteOpsExp with PrinterOpsExp {
  this: ForgeApplication =>
  
  // -- IR helpers
  
  def isPrimitiveType(t: Rep[DSLType]) = t match {
    case `MInt` | `MDouble` | `MBoolean` | `MString` | `MUnit` => true
    case _ => false
  }
  
  def grpIsTpe(grp: Rep[DSLGroup]) = grp match {
    case Def(Tpe(n,targs,s)) => true
    case Def(TpeInst(t,args,s)) => true
    case Def(TpePar(n,ctx)) => true
    case _ => false
  }
  def grpAsTpe(grp: Rep[DSLGroup]): Rep[DSLType] = grp match {
    case t@Def(Tpe(n,targs,s)) => t.asInstanceOf[Rep[DSLType]]
    case t@Def(TpeInst(hk,args,s)) => t.asInstanceOf[Rep[DSLType]]
    case t@Def(TpePar(n,ctx)) => t.asInstanceOf[Rep[DSLType]]
    // case _ => err(grp.name + " is not a DSLType")
  }
  
  def hasFuncArgs(o: Rep[DSLOp]) = o.args.exists(a => a match {
    case Def(FTpe(args,ret)) => true
    case _ => false
  })  
}

trait ForgeUtilities {  
  def err(s: String)(implicit ctx: SourceContext) = {
    println("[forge error]: " + s)
    println("  at " + (ctx.fileName.split("/").last + ":" + ctx.line)) 
    exit(1)
  }
  def warn(s: String) = println("[forge warning]: " + s)  
  def info(s: String) = println("[forge]: " + s)
}


/**
 * Forge code generators
 */
trait ForgeCodeGenBase extends GenericCodegen with ScalaGenBase {
  val IR: ForgeApplicationRunner with ForgeExp
  import IR._
    
  def buildDir: String
  lazy val dslDir = buildDir + File.separator + "src" + File.separator + dsl.toLowerCase() + File.separator
      
  // -- code gen helpers
  
  def varify(a: Exp[Any]): String = a match {
    case Def(FTpe(args,ret)) => err("variables in function tpe")
    case _ => "Var[" + quote(a) + "]" 
  }
  def repify(a: Exp[Any]): String = a match {
    case Def(FTpe(args,ret)) => 
      if (args == List(byName)) " => " + repify(ret)
      else "(" + args.map(repify).mkString(",") + ") => " + repify(ret)        
    case Def(Tpe("Var", arg, stage)) => repify(arg(0))
    case Def(TpeInst(Def(Tpe("Var",a1,s1)), a2, s2)) => repify(a2(0))
    case _ => "Rep[" + quote(a) + "]"           
  }
  def repifySome(a: Exp[Any]): String = a match {  
    case Def(Tpe(name, arg, `now`)) => quote(a)
    case Def(Tpe("Var", arg, stage)) => varify(arg(0))
    case Def(TpeInst(Def(Tpe("Var",a1,s1)), a2, s2)) => varify(a2(0))
    case _ => repify(a)
  }
  
  def makeTpeParsWithBounds(args: List[Rep[TypePar]]): String = {
    if (args.length < 1) return ""    
    val args2 = args.map { a => a.name + (if (a.ctxBounds != Nil) ":" + a.ctxBounds.map(_.name).mkString(":") else "") }
    "[" + args2.mkString(",") + "]"
  }  
  def makeTpePars(args: List[Rep[TypePar]]): String = {
    if (args.length < 1) return ""
    "[" + args.map(_.name).mkString(",") + "]"
  }
  
  override def quote(x: Exp[Any]) : String = x match {
    case Def(Tpe(s,args,stage)) => s + makeTpePars(args)
    case Def(TpeInst(t,args,s)) => t.name + "[" + args.map(quote).mkString(",") + "]"
    case Def(TpePar(s,ctx)) => s 
    case Sym(n) => err("could not resolve symbol " + findDefinition(n.asInstanceOf[Sym[Any]]).toString + ". All Forge symbols must currently be statically resolvable.")
    case _ => super.quote(x)
  }  
}

/**
 * This is the interface that all backends must implement to generate an implementation of the DSL
 * from the specification.
 */
trait ForgeCodeGenBackend extends ForgeCodeGenBase with ForgeScalaCodeGenPkg with ScalaGenForgeOps {
  val IR: ForgeApplicationRunner with ForgeExp  
  
  def emitDSLImplementation(): Unit
}

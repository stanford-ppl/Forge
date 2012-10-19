package ppl.dsl.meta
package core

import java.io._
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}
import templates.DataStructures._
import templates.Ops._
import templates.Packages._

trait MetaDSLApplication extends MetaDSL with MetaDSLLift {
  def dslName: String
  def lmsAppOps: List[LMSOps] = List()
  def lmsCompOps: List[LMSOps] = List()
  def specification(): Rep[Unit]    
}

/**
 * These are the portions of Scala imported into MetaDSL's scope.
 */
trait MetaDSLLift extends LiftString with LiftBoolean with LiftNumeric with LiftPrimitives {
  this: MetaDSL =>
}

trait MetaDSLScalaOpsPkg extends Base
  with ImplicitOps with OrderingOps with StringOps with ArrayOps
  with BooleanOps with PrimitiveOps with TupleOps with CastingOps 

trait MetaDSLScalaOpsPkgExp extends MetaDSLScalaOpsPkg 
  with ImplicitOpsExp with OrderingOpsExp with StringOpsExp  with ArrayOpsExp 
  with BooleanOpsExp with PrimitiveOpsExp with TupleOpsExp with CastingOpsExp 

trait MetaDSLScalaCodeGenPkg extends ScalaGenEffect
  with ScalaGenImplicitOps with ScalaGenOrderingOps with ScalaGenStringOps  with ScalaGenArrayOps 
  with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenTupleOps with ScalaGenCastingOps 
  { val IR: MetaDSLScalaOpsPkgExp  }


/**
 * This the trait that every MetaDSL application must extend.
 */
trait MetaDSL extends MetaDSLScalaOpsPkg with DerivativeTypes with Definitions with MetaDSLOps with SpecOps {
  this: MetaDSLApplication =>
}

/**
 * These are the corresponding IR nodes for MetaDSL.
 */
trait MetaDSLExp extends MetaDSL with MetaDSLUtilities with MetaDSLScalaOpsPkgExp with MetaDSLOpsExp with FieldOpsExp with SpecOpsExp {
  this: MetaDSLApplication =>
}

trait MetaDSLUtilities {  
  def err(s: String)(implicit ctx: SourceContext) = {
    println("[metadsl error]: " + s)
    println("  at " + (ctx.fileName.split("/").last + ":" + ctx.line)) 
    exit(1)
  }
  def warn(s: String) = println("[metadsl warning]: " + s)  
  def info(s: String) = println("[metadsl]: " + s)
}


/**
 * MetaDSL code generators
 */
trait MetaDSLCodeGenBase extends GenericCodegen {
  val IR: MetaDSLApplicationRunner with MetaDSLExp
  import IR._
    
  def makeEffectAnnotation(effect: EffectType) = effect match {
    case `pure` => "reflectPure"
    case `mutable` => "reflectMutable"
    case write(args @ _*) => "reflectWrite(" + args.map(a => opArgPrefix + a).mkString(",") + ")"
  }
  
  def makeTpeArgsWithBounds(args: List[Rep[TypeArg]]): String = {
    if (args.length < 1) return ""    
    val args2 = args.map { a => a.name + (if (a.ctxBounds != Nil) ":" + a.ctxBounds.map(_.name).mkString(":") else "") }
    "[" + args2.mkString(",") + "]"
  }
  
  def makeTpeArgs(args: List[Rep[TypeArg]]): String = {
    if (args.length < 1) return ""
    "[" + args.map(_.name).mkString(",") + "]"
  }
  
  def varify(a: Exp[Any]) = "Var[" + quote(a) + "]"
  def repify(a: Exp[Any]) = "Rep[" + quote(a) + "]"
  
  override def quote(x: Exp[Any]) : String = x match {
    case Def(Tpe(s, args)) => s + makeTpeArgs(args)
    case Def(TpeArg(s, ctx)) => s 
    case _ => super.quote(x)
  }  
}

trait MetaDSLCodeGenScala extends MetaDSLCodeGenBase with MetaDSLScalaCodeGenPkg with ScalaGenMetaDSLOps with ScalaGenPackages with ScalaGenDataStructures with ScalaGenOps {  
  val IR: MetaDSLApplicationRunner with MetaDSLExp
}


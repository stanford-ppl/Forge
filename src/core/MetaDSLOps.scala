package ppl.dsl.meta
package core

import java.io.{PrintWriter}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.collection.mutable.{ArrayBuffer,HashMap}

trait MetaDSLOps extends Base {
  this: MetaDSL =>
    
  def tpeArg(name: String, ctxBounds: List[TypeClass] = List()) = metadsl_tpearg(name, ctxBounds) // TODO: type bounds
  def tpe(name: String, tpeArgs: List[Rep[TypeArg]] = List()) = metadsl_tpe(name, tpeArgs)
  def data(tpe: Rep[DSLType], tpeArgs: List[Rep[TypeArg]], fields: (String, Rep[DSLType])*) = metadsl_data(tpe, tpeArgs, fields)
  def op(tpe: Rep[DSLType])(name: String, style: MethodType, tpeArgs: List[Rep[TypeArg]], args: List[Rep[DSLType]], retTpe: Rep[DSLType], opTpe: OpType, effect: EffectType = pure, implicitArgs: List[Rep[DSLType]] = List(MSourceContext)) = metadsl_op(tpe,name,style,tpeArgs,args,implicitArgs,retTpe,opTpe,effect)
  def codegen(op: Rep[DSLOp])(generator: CodeGenerator, rule: String) = metadsl_codegen(op,generator,rule)
    
  def infix_is(tpe: Rep[DSLType], dc: DeliteCollection) = metadsl_isdelitecollection(tpe, dc)  
  case class DeliteCollection(val tpeArg: Rep[TypeArg], val alloc: Rep[DSLOp], val size: Rep[DSLOp], val apply: Rep[DSLOp], val update: Rep[DSLOp])
  
  def metadsl_tpearg(name: String, ctxBounds: List[TypeClass]): Rep[TypeArg]
  def metadsl_tpe(name: String, tpeArgs: List[Rep[TypeArg]]): Rep[DSLType]    
  def metadsl_data(tpe: Rep[DSLType], tpeArgs: List[Rep[TypeArg]], fields: Seq[(String, Rep[DSLType])]): Rep[DSLData]  
  def metadsl_op(tpe: Rep[DSLType], name: String, style: MethodType, tpeArgs: List[Rep[TypeArg]], args: List[Rep[DSLType]], implicitArgs: List[Rep[DSLType]], retTpe: Rep[DSLType], opTpe: OpType, effect: EffectType): Rep[DSLOp]
  def metadsl_codegen(op: Rep[DSLOp], generator: CodeGenerator, rule: String): Rep[CodeGenRule]
  
  def metadsl_isdelitecollection(tpe: Rep[DSLType], dc: DeliteCollection): Rep[Unit]
}

trait MetaDSLOpsExp extends MetaDSLOps with BaseExp {
  this: MetaDSLExp  =>

  /*
   * Compiler state
   */
  val Tpes = ArrayBuffer[Exp[DSLType]]()
  val DataStructs = ArrayBuffer[Exp[DSLData]]()
  val OpsGrp = HashMap[Exp[DSLType],DSLOps]()
  val CodeGenRules = HashMap[Exp[DSLType],ArrayBuffer[Exp[CodeGenRule]]]()
  val DeliteCollections = HashMap[Exp[DSLType], DeliteCollection]()
       
  /**
   * IR Definitions
   */
  
  // no higher-kinded fun yet
  case class TpeArg(name: String, ctxBounds: List[TypeClass]) extends Def[TypeArg]
   
  def metadsl_tpearg(name: String, ctxBounds: List[TypeClass]) = TpeArg(name, ctxBounds)
     
  case class Tpe(name: String, tpeArgs: List[Rep[TypeArg]]) extends Def[DSLType]
  
  def metadsl_tpe(name: String, tpeArgs: List[Rep[TypeArg]]) = {
    val t = Tpe(name, tpeArgs)
    Tpes += t
    t
  }
  
  case class Data(tpe: Rep[DSLType], tpeArgs: List[Rep[TypeArg]], fields: Seq[(String, Exp[DSLType])]) extends Def[DSLData]
  
  def metadsl_data(tpe: Rep[DSLType], tpeArgs: List[Rep[TypeArg]], fields: Seq[(String, Exp[DSLType])]) = {
    val d = Data(tpe, tpeArgs, fields)
    DataStructs += d
    d
  }
  
  case class Op(tpe: Rep[DSLType], name: String, style: MethodType, tpeArgs: List[Rep[TypeArg]], args: List[Rep[DSLType]], implicitArgs: List[Rep[DSLType]], retTpe: Rep[DSLType], opTpe: OpType, effect: EffectType) extends Def[DSLOp]
  
  def metadsl_op(tpe: Rep[DSLType], name: String, style: MethodType, tpeArgs: List[Rep[TypeArg]], args: List[Rep[DSLType]], implicitArgs: List[Rep[DSLType]], retTpe: Rep[DSLType], opTpe: OpType, effect: EffectType) = {
    val o = Op(tpe, name, style, tpeArgs, args, implicitArgs, retTpe, opTpe, effect)
    val grp = OpsGrp.getOrElseUpdate(tpe, new DSLOps {
      def name = tpe.name + "Ops"
      override def targets: List[CodeGenerator] = if (opTpe == codegenerated) List($cala) else List() // TODO 
    })
    grp.ops ::= o
    o
  }
  
  case class CodeGenDecl(tpe: Rep[DSLType], op: Rep[DSLOp], generator: CodeGenerator, rule: String) extends Def[CodeGenRule]
  
  def metadsl_codegen(op: Rep[DSLOp], generator: CodeGenerator, rule: String) = {
    val c = CodeGenDecl(op.tpe, op, generator, rule)
    if (CodeGenRules.get(op.tpe).exists(_.exists(_.op == op))) err("multiple code generators declared for op " + op.name)
    
    val buf = CodeGenRules.getOrElseUpdate(op.tpe, new ArrayBuffer[Exp[CodeGenRule]]())
    buf += c
    c
  }
  
  def metadsl_isdelitecollection(tpe: Rep[DSLType], dc: DeliteCollection) = {
    // verify the dc functions match our expectations
    if ((dc.alloc.args != List(MInt) || dc.alloc.retTpe != tpe))
      // TODO: how should this work? alloc can really map to anything.. e.g can have other fields that get mapped from the inputs in arbitrary ways
      // needs to be specified in the zip in some context attached to the alloc method?      
      err("dcAlloc must take a single argument of type " + MInt.name + " and return an instance of " + tpe.name)
    if ((dc.size.args != List(tpe)) || (dc.size.retTpe != MInt))
      err("dcSize must take a single argument of type " + tpe.name + " and return an MInt")
    if ((dc.apply.args != List(tpe, MInt)) || (dc.apply.retTpe != dc.tpeArg))
      err("dcApply must take two arguments of type(" + tpe.name + ", " + MInt.name + ") and return a " + dc.tpeArg.name)
    if ((dc.update.args != List(tpe, MInt, dc.tpeArg)) || (dc.update.retTpe != MUnit))
      err("dcUpdate must take arguments of type (" + tpe.name + ", " + MInt.name + ", " + dc.tpeArg.name + ") and return " + MUnit.name)
    
    DeliteCollections += (tpe -> dc)
    ()
  }
}


trait ScalaGenMetaDSLOps extends ScalaGenBase {
  val IR: MetaDSLOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
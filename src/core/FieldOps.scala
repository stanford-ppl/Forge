package ppl.dsl.meta
package core

import java.io.{PrintWriter}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.virtualization.lms.util.OverloadHack

/** 
 * These are all just field accesses. Is there a simpler way to organize this or avoid it?
 * One obvious way is to bypass LMS and use only concrete structures in the DSL.
 * Another possible way: something like repProxy
 * 
 * The only real purpose of this file (and this organization) right now is to hide the
 * IR node structure from the application. Does it matter? 
 * 
 * Note that if we expose this trait to the application, and we make all the fields in the case
 * class accessible, we've accomplished nothing other than a lot of indirection. 
 * Currently it is only available to the compiler, but it's not clear that there is a good reason.
 * (It is also not clear that this separation will be practical as the spec expands..)
 */

trait FieldOps extends Base with OverloadHack {
  this: MetaDSL =>
  
  /**
   * DSLType
   */  
  // def infix_name(x: Rep[DSLType]): String
  // def infix_tpeArgs(x: Rep[DSLType]): List[TpeArg]

  /**
   * DSLOp
   */
  // def infix_args(x: Rep[DSLOp]): List[Rep[DSLType]]
  // def infix_tpe(x: Rep[DSLOp]): Rep[DSLType]
  // def infix_name(x: Rep[DSLOp])(implicit o: Overloaded1): String
  // def infix_style(x: Rep[DSLOp]): MethodType
  // def infix_tpeArgs(x: Rep[DSLOp])(implicit o: Overloaded1): List[TpeArg]
  // def infix_retTpe(x: Rep[DSLOp]): Rep[DSLType]
  // def infix_opTpe(x: Rep[DSLOp]): OpType 
  
  /**
   * CodeGenRule
   */
  // def infix_generator(x: Rep[CodeGenRule]): CodeGenerator 
  // def infix_tpe(x: Rep[CodeGenRule])(implicit o: Overloaded1): Rep[DSLType]
  // def infix_op(x: Rep[CodeGenRule]): Rep[DSLOp]
  // def infix_rule(x: Rep[CodeGenRule]): String
}

trait FieldOpsExp extends FieldOps {
  this: MetaDSLExp =>
 
  /**
   * TypeArg
   */
  def infix_ctxBounds(x: Rep[TypeArg]): List[TypeClass] = x match {
    case Def(TpeArg(name,ctx)) if (ctx.contains(TManifest)) => ctx
    case Def(TpeArg(name,ctx)) => TManifest :: ctx
  }
  def infix_name(x: Rep[TypeArg]) = x match {
    case Def(TpeArg(name,ctx)) => name
  }  
  
  /**
   * DSLType
   */
  def infix_name(x: Exp[DSLType])(implicit o: Overloaded1) = x match {
    case Def(Tpe(name,tpeArgs)) => name
    case Def(TpeArg(name,ctx)) => name
  }  
  def infix_tpeArgs(x: Exp[DSLType]) = x match {
    case Def(Tpe(s,tpeArgs)) => tpeArgs
    case Def(TpeArg(name,ctx)) => Nil
  }  
  
  /**
   * DSLOp
   */
  def infix_args(x: Exp[DSLOp]) = x match {
    case Def(Op(tpe,name,style,tpeArgs,args,implArgs,retTpe,opTpe,eff)) => args
  }
  def infix_implicitArgs(x: Exp[DSLOp]) = x match {
    case Def(Op(tpe,name,style,tpeArgs,args,implArgs,retTpe,opTpe,eff)) => implArgs
  }  
  def infix_tpe(x: Exp[DSLOp]) = x match {
    case Def(Op(tpe,name,style,tpeArgs,args,implArgs,retTpe,opTpe,eff)) => tpe
  }
  def infix_name(x: Exp[DSLOp])(implicit o: Overloaded2) = x match {
    case Def(Op(tpe,name,style,tpeArgs,args,implArgs,retTpe,opTpe,eff)) => name
  }  
  def infix_style(x: Exp[DSLOp]) = x match {
    case Def(Op(tpe,name,style,tpeArgs,args,implArgs,retTpe,opTpe,eff)) => style
  }  
  def infix_tpeArgs(x: Exp[DSLOp])(implicit o: Overloaded1) = x match {
    case Def(Op(tpe,name,style,tpeArgs,args,implArgs,retTpe,opTpe,eff)) => tpeArgs
  }  
  def infix_retTpe(x: Exp[DSLOp]) = x match {
    case Def(Op(tpe,name,style,tpeArgs,args,implArgs,retTpe,opTpe,eff)) => retTpe
  }  
  def infix_opTpe(x: Exp[DSLOp]) = x match {
    case Def(Op(tpe,name,style,tpeArgs,args,implArgs,retTpe,opTpe,eff)) => opTpe
  }  
  def infix_effect(x: Exp[DSLOp]) = x match {
    case Def(Op(tpe,name,style,tpeArgs,args,implArgs,retTpe,opTpe,eff)) => eff
  }    
  
  /**
   * CodeGenRule
   */
  def infix_generator(x: Exp[CodeGenRule]) = x match {
    case Def(CodeGenDecl(tpe,op,gen,rule)) => gen
  }  
  def infix_tpe(x: Exp[CodeGenRule])(implicit o: Overloaded1) = x match {
    case Def(CodeGenDecl(tpe,op,gen,rule)) => tpe
  }    
  def infix_op(x: Exp[CodeGenRule]) = x match {
    case Def(CodeGenDecl(tpe,op,gen,rule)) => op
  }      
  def infix_rule(x: Exp[CodeGenRule]) = x match {
    case Def(CodeGenDecl(tpe,op,gen,rule)) => rule
  }        
  
  /**
   * DSLData
   */
  def infix_tpe(x: Exp[DSLData])(implicit o: Overloaded2) = x match {
    case Def(Data(tpe,tpeArgs,fields)) => tpe
  }  
  def infix_tpeArgs(x: Exp[DSLData])(implicit o: Overloaded2) = x match {
    case Def(Data(tpe,tpeArgs,fields)) => tpeArgs
  }    
  def infix_fields(x: Exp[DSLData]) = x match {
    case Def(Data(tpe,tpeArgs,fields)) => fields
  }    
 
}



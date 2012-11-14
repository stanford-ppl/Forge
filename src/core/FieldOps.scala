package ppl.dsl.forge
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
  this: Forge =>
  
  /**
   * DSLType
   */  
  // def infix_name(x: Rep[DSLType]): String
  // def infix_tpePars(x: Rep[DSLType]): List[TpeArg]

  /**
   * DSLOp
   */
  // def infix_args(x: Rep[DSLOp]): List[Rep[DSLType]]
  // def infix_tpe(x: Rep[DSLOp]): Rep[DSLType]
  // def infix_name(x: Rep[DSLOp])(implicit o: Overloaded1): String
  // def infix_style(x: Rep[DSLOp]): MethodType
  // def infix_tpePars(x: Rep[DSLOp])(implicit o: Overloaded1): List[TpeArg]
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
  this: ForgeExp =>
 
  /**
   * TypePar
   */
  def infix_ctxBounds(x: Rep[TypePar]): List[TypeClass] = x match {
    case Def(TpeArg(name,ctx)) if (ctx.contains(TManifest)) => ctx
    case Def(TpeArg(name,ctx)) => TManifest :: ctx
  }
  def infix_name(x: Rep[TypePar]) = x match {
    case Def(TpeArg(name,ctx)) => name
  }  
  
  /**
   * DSLType
   */
  def infix_name(x: Exp[DSLType])(implicit o: Overloaded1): String = x match {
    case Def(Tpe(name,tpePars,stage)) => name
    case Def(TpeInst(t,args,stage)) => infix_name(t)(o)
    case Def(FTpe(args,ret)) => "Function"
    case Def(TpeArg(name,ctx)) => name        
  }  
  def infix_tpePars(x: Exp[DSLType]) = x match {
    case Def(Tpe(s,tpePars,stage)) => tpePars
    case Def(TpeInst(t,args,stage)) => Nil
    case Def(TpeArg(name,ctx)) => Nil
    case Def(FTpe(args,ret)) => Nil
  }
  def infix_stage(x: Exp[DSLType]) = x match {
    case Def(Tpe(s,tpePars,stage)) => stage
    case Def(TpeInst(t,args,stage)) => stage
    case Def(TpeArg(name,ctx)) => future
    case Def(FTpe(args,ret)) => future
  }  
    
  
  /**
   * DSLGroup
   */
  def infix_name(x: Exp[DSLGroup])(implicit o: Overloaded2): String = x match {
    case Def(Grp(name)) => name    
    case y: Exp[DSLType] => y.name
  }  
  
  
  /**
   * DSLOp
   */
  def infix_args(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,opTpe,eff)) => args
  }
  def infix_implicitArgs(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,opTpe,eff)) => implArgs
  }  
  def infix_grp(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,opTpe,eff)) => grp
  }
  def infix_name(x: Exp[DSLOp])(implicit o: Overloaded3) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,opTpe,eff)) => name
  }  
  def infix_style(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,opTpe,eff)) => style
  }  
  def infix_tpePars(x: Exp[DSLOp])(implicit o: Overloaded1) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,opTpe,eff)) => tpePars
  }  
  def infix_retTpe(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,opTpe,eff)) => retTpe
  }  
  def infix_opTpe(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,opTpe,eff)) => opTpe
  }  
  def infix_effect(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,opTpe,eff)) => eff
  }    
  
  /**
   * CodeGenRule
   */
  def infix_generator(x: Exp[CodeGenRule]) = x match {
    case Def(CodeGenDecl(op,gen,rule,s)) => gen
  }  
  def infix_op(x: Exp[CodeGenRule]) = x match {
    case Def(CodeGenDecl(op,gen,rule,s)) => op
  }      
  def infix_rule(x: Exp[CodeGenRule]) = x match {
    case Def(CodeGenDecl(op,gen,rule,s)) => rule
  }        
  def infix_isSimple(x: Exp[CodeGenRule]) = x match {
    case Def(CodeGenDecl(op,gen,rule,s)) => s
  }        
  
  /**
   * DSLData
   */
  def infix_tpe(x: Exp[DSLData])(implicit o: Overloaded2) = x match {
    case Def(Data(tpe,tpePars,fields)) => tpe
  }  
  def infix_tpePars(x: Exp[DSLData])(implicit o: Overloaded2) = x match {
    case Def(Data(tpe,tpePars,fields)) => tpePars
  }    
  def infix_fields(x: Exp[DSLData]) = x match {
    case Def(Data(tpe,tpePars,fields)) => fields
  }    
 
}



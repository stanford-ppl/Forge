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
}

trait FieldOpsExp extends FieldOps {
  this: ForgeExp =>
 
 /**
  *  TypeAlias
  */
  def infix_name(x: Exp[TypeAlias]): String = x match {
    case Def(TpeAlias(name,tpe)) => name
  }
  def infix_tpe(x: Exp[TypeAlias]): Exp[DSLType] = x match {
    case Def(TpeAlias(name,tpe)) => tpe
  }

  /**
   * TypePar
   */
  def infix_ctxBounds(x: Exp[TypePar]): List[TypeClass] = x match {
    case Def(TpePar(name,ctx)) if (ctx.contains(TManifest)) => ctx
    case Def(TpePar(name,ctx)) => TManifest :: ctx
  }
  def infix_name(x: Exp[TypePar])(implicit o: Overloaded1) = x match {
    case Def(TpePar(name,ctx)) => name
  }  
  
  /**
   * DSLType
   */
  def infix_name(x: Exp[DSLType])(implicit o: Overloaded2): String = x match {
    case Def(Tpe(name,tpePars,stage)) => name
    case Def(TpeInst(t,args,stage)) => infix_name(t)(o)
    case Def(FTpe(args,ret,freq)) => "Function"
    case Def(TpePar(name,ctx)) => name        
    case Def(VarArgs(t)) => "*" + infix_name(t)(o)
  }  
  def infix_tpePars(x: Exp[DSLType]) = x match {
    case Def(Tpe(s,tpePars,stage)) => tpePars
    case Def(TpeInst(t,args,stage)) => Nil
    case Def(TpePar(name,ctx)) => Nil
    case Def(FTpe(args,ret,freq)) => Nil
    case Def(VarArgs(t)) => Nil
  }
  def infix_stage(x: Exp[DSLType]) = x match {
    case Def(Tpe(s,tpePars,stage)) => stage
    case Def(TpeInst(t,args,stage)) => stage
    case Def(TpePar(name,ctx)) => future
    case Def(FTpe(args,ret,freq)) => future
    case Def(VarArgs(t)) => future
  }  
 
  /**
  * DSLArg
  */   
  def infix_name(x: Exp[DSLArg])(implicit o: Overloaded3): String = x match  {
    case Def(Arg(name,tpe,default)) => name
  }
  def infix_tpe(x: Exp[DSLArg])(implicit o: Overloaded1): Exp[DSLType] = x match  {
    case Def(Arg(name,tpe,default)) => tpe
  }
  def infix_default(x: Exp[DSLArg]) = x match  {
    case Def(Arg(name,tpe,default)) => default
  }

  /**
   * DSLGroup
   */
  def infix_name(x: Exp[DSLGroup])(implicit o: Overloaded4): String = x match {
    case Def(Grp(name)) => name    
    case _ if grpIsTpe(x) => grpAsTpe(x).name
  }  
  
  
  /**
   * DSLOp
   */
  def infix_args(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,eff,alias)) => args
  }
  def infix_implicitArgs(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,eff,alias)) => implArgs
  }  
  def infix_grp(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,eff,alias)) => grp
  }
  def infix_name(x: Exp[DSLOp])(implicit o: Overloaded5) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,eff,alias)) => name
  }  
  def infix_style(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,eff,alias)) => style
  }  
  def infix_tpePars(x: Exp[DSLOp])(implicit o: Overloaded1) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,eff,alias)) => tpePars
  }  
  def infix_retTpe(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,eff,alias)) => retTpe
  }  
  def infix_effect(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,eff,alias)) => eff
  }    
  def infix_aliasHint(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,implArgs,retTpe,eff,alias)) => alias
  }      
  
  /**
   * DeliteRule
   */
  def infix_generator(x: Exp[DeliteRule]) = x match {
    case Def(CodeGenDecl(gen,rule,s)) => gen
  }  
  def infix_rule(x: Exp[DeliteRule]) = x match {
    case Def(CodeGenDecl(gen,rule,s)) => rule
  }        
  def infix_isSimple(x: Exp[DeliteRule]) = x match {
    case Def(CodeGenDecl(gen,rule,s)) => s
  }        
  
  /**
  * DeliteRule
  * gibbons4 - should this be split into each rule
  * also - have to return same type. disregard errors?
  */
  def structArgIndex(x: Exp[DeliteRule]) = x match {
    case Def(Getter(structArgIndex, field)) => structArgIndex
    case Def(Setter(structArgIndex, field, value)) => structArgIndex
    //case _ => err("Called a DeliteRule with non-existant field : structArgIndex")
  }

  def field(x: Exp[DeliteRule]) = x match {
    case Def(Getter(structArgIndex, field)) => field
    case Def(Setter(structArgIndex, field, value)) => field 
    //case _ => err("Called a DeliteRule with non-existant field : field") // todo - equivocaion gibbons4
  }

  def value(x: Exp[DeliteRule]) = x match {
    case Def(Setter(structArgIndex, field, value)) => value
    //case _ => err("Called a DeliteRule with non-existant field : ")
  }

  def func(x: Exp[DeliteRule]) = x match {
    case Def(Composite(func)) => func
    case Def(SingleTask(func)) => func
    case Def(Map(tpePars, argIndex, func)) => func
    case Def(Zip(tpePars, argIndices, func)) => func
    case Def(Reduce(tpePars, argIndex, zero, func)) => func
    case Def(Filter(tpePars, argIndex, cond, func)) => func
    case Def(Foreach(tpePars, argIndex, func)) => func
    //case _ => err("Called a DeliteRule with non-existant field : ")
  }

  def data(x: Exp[DeliteRule]) = x match {
    case Def(Allocates(data, init)) => data
    //case _ => err("Called a DeliteRule with non-existant field : ")
  }

  def init(x: Exp[DeliteRule]) = x match {
    case Def(Allocates(data, init)) => init
    //case _ => err("Called a DeliteRule with non-existant field : ")
  }

  def tpePars(x: Exp[DeliteRule]) = x match {
    case Def(Map(tpePars, argIndex, func)) => tpePars
    case Def(Zip(tpePars, argIndices, func)) => tpePars
    case Def(Reduce(tpePars, argIndex, zero, func)) => tpePars
    case Def(Filter(tpePars, argIndex, cond, func)) => tpePars
    case Def(Foreach(tpePars, argIndex, func)) => tpePars
    //case _ => err("Called a DeliteRule with non-existant field : ")
  }

  def argIndex(x: Exp[DeliteRule]) = x match {
    case Def(Map(tpePars, argIndex, func)) => argIndex
    case Def(Reduce(tpePars, argIndex, zero, func)) => argIndex
    case Def(Filter(tpePars, argIndex, cond, func)) => argIndex
    case Def(Foreach(tpePars, argIndex, func)) => argIndex
    //case _ => err("Called a DeliteRule with non-existant field : ")
  }

  def argIndices(x: Exp[DeliteRule]) = x match {
    case Def(Zip(tpePars, argIndices, func)) => argIndices
    //case _ => err("Called a DeliteRule with non-existant field : ")
  }

  def zero(x: Exp[DeliteRule]) = x match {
    case Def(Reduce(tpePars, argIndex, zero, func)) => zero
    //case _ => err("Called a DeliteRule with non-existant field : ")
  }

  def cond(x: Exp[DeliteRule]) = x match {
    case Def(Filter(tpePars, argIndex, cond, func)) => cond
    //case _ => err("Called a DeliteRule with non-existant field : ")
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



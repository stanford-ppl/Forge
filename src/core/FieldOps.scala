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

  // TODO: hide these behind a forge cmd in ForgeOps

  // needed for lookup
  def infix_name(x: Rep[DSLType])(implicit o: Overloaded2): String
  // needed for is
  def infix_tpePars(x: Rep[DSLType]): List[Rep[TypePar]]
}

trait FieldOpsExp extends FieldOps {
  this: ForgeExp =>

 /**
  * TypeAlias
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
  def infix_ctxBounds(x: Exp[TypePar]): List[TypeClassSignature] = x match {
    case Def(TpePar(name,ctx,s)) => ctx
  }
  def infix_name(x: Exp[TypePar])(implicit o: Overloaded1) = x match {
    case Def(TpePar(name,ctx,s)) => name
  }

  /**
   * DSLType
   */
  def infix_name(x: Exp[DSLType])(implicit o: Overloaded2): String = x match {
    case Def(Tpe(name,tpePars,stage)) => name
    case Def(TpeInst(t,args)) => infix_name(t)(o)
    case Def(TpeClass(name,sig,tpePars)) => name
    case Def(TpeClassInst(name,tpePars,t)) => name
    case Def(FTpe(args,ret,freq)) => "Function"
    case Def(TpePar(name,ctx,s)) => name
    case Def(VarArgs(t)) => "*" + infix_name(t)(o)
  }
  def infix_tpePars(x: Exp[DSLType]) = x match {
    case Def(Tpe(s,tpePars,stage)) => tpePars
    case Def(TpeInst(t,args)) => Nil
    case Def(TpePar(name,ctx,s)) => Nil
    case Def(TpeClass(name,sig,tpePars)) => tpePars
    case Def(TpeClassInst(name,tpePars,t)) => tpePars
    case Def(FTpe(args,ret,freq)) => Nil
    case Def(VarArgs(t)) => Nil
  }
  def infix_stage(x: Exp[DSLType]) = x match {
    case Def(Tpe(s,tpePars,stage)) => stage
    case Def(TpeInst(Def(Tpe(s,tpePars,stage)),args)) => stage
    case Def(TpePar(name,ctx,stage)) => stage
    case Def(TpeClass(name,sig,pars)) => future
    case Def(TpeClassInst(name,tpePars,t)) => future
    case Def(FTpe(args,ret,freq)) => future
    case Def(VarArgs(t)) => future
  }

  /**
   * DSLTypeClass
   */
   def infix_signature(x: Exp[DSLTypeClass]): TypeClassSignature = x match {
     case Def(TpeClass(name,sig,tpePars)) => sig
   }

  /**
   * DSLTypeClassInst
   */
  def infix_tpe(x: Exp[DSLTypeClassInst])(implicit o: Overloaded1): Exp[DSLType] = x match {
    case Def(TpeClassInst(name,tpePars,t)) => t
  }

  /**
  * DSLArg
  */
  def infix_name(x: Exp[DSLArg])(implicit o: Overloaded3): String = x match  {
    case Def(Arg(name,tpe,default)) => name
  }
  def infix_tpe(x: Exp[DSLArg])(implicit o: Overloaded2): Exp[DSLType] = x match  {
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
    case Def(Op(grp,name,style,tpePars,args,Nil,implArgs,retTpe,eff,alias)) => args
    case Def(Op(grp,name,style,tpePars,args,curArgs,implArgs,retTpe,eff,alias)) => args ++ curArgs.flatten
  }
  def infix_firstArgs(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,curArgs,implArgs,retTpe,eff,alias)) => args
  }
  def infix_curriedArgs(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,curArgs,implArgs,retTpe,eff,alias)) => curArgs
  }
  def infix_implicitArgs(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,curArgs,implArgs,retTpe,eff,alias)) => implArgs
  }
  def infix_grp(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,curArgs,implArgs,retTpe,eff,alias)) => grp
  }
  def infix_name(x: Exp[DSLOp])(implicit o: Overloaded5) = x match {
    case Def(Op(grp,name,style,tpePars,args,curArgs,implArgs,retTpe,eff,alias)) => name
  }
  def infix_style(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,curArgs,implArgs,retTpe,eff,alias)) => style
  }
  def infix_tpePars(x: Exp[DSLOp])(implicit o: Overloaded1) = x match {
    case Def(Op(grp,name,style,tpePars,args,curArgs,implArgs,retTpe,eff,alias)) => tpePars
  }
  def infix_retTpe(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,curArgs,implArgs,retTpe,eff,alias)) => retTpe
  }
  def infix_effect(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,curArgs,implArgs,retTpe,eff,alias)) => eff
  }
  def infix_aliasHint(x: Exp[DSLOp]) = x match {
    case Def(Op(grp,name,style,tpePars,args,curArgs,implArgs,retTpe,eff,alias)) => alias
  }

  /**
   * DSLData
   */
  def infix_tpe(x: Exp[DSLData])(implicit o: Overloaded3) = x match {
    case Def(Data(tpe,fields)) => tpe
  }
  def infix_fields(x: Exp[DSLData]) = x match {
    case Def(Data(tpe,fields)) => fields
  }

 /**
  * Identifier
  */
  def infix_name(x: Exp[DSLIdentifier])(implicit o: Overloaded6): String = x match {
    case Def(Identifier(name,tpe)) => name
  }
  def infix_tpe(x: Exp[DSLIdentifier])(implicit o: Overloaded4): Exp[DSLType] = x match {
    case Def(Identifier(name,tpe)) => tpe
  }


}



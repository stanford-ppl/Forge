package ppl.dsl.forge
package core

import java.io.{PrintWriter}
import org.scala_lang.virtualized.SourceContext
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
  implicit def DSLTypeOpsClsFO(x: Rep[DSLType]): AbsDSLTypeOpsCls
  abstract class AbsDSLTypeOpsCls {
    def name(implicit o: Overloaded1): String
    // needed for is
    def tpePars: List[Rep[TypePar]]
    // needed to generate primitive math combinations as type classes
    def stage: StageTag
    def tpeArgs: List[Rep[DSLType]]
  }
}


trait FieldOpsExp extends FieldOps {
  this: ForgeExp =>

 /**
  * TypeAlias
  */
 implicit class TypeAliasOpsCls(val x: Exp[TypeAlias]) {
   def name: String = x match {
     case Def(TpeAlias(name, tpe)) => name
   }

   def tpe: Exp[DSLType] = x match {
     case Def(TpeAlias(name, tpe)) => tpe
   }
 }

  /**
   * TypePar
   */
  implicit class TypeParOpsCls(val x: Exp[TypePar]) {
    def ctxBounds: List[TypeClassSignature] = x match {
      case Def(TpePar(name,ctx,s)) => ctx
      case Def(HkTpePar(name,tpePars,ctx,s)) => ctx
    }
  }

  /**
   * DSLType
   */
  implicit def DSLTypeOpsClsFO(x: Rep[DSLType]) = new DSLTypeOpsCls(x)
  class DSLTypeOpsCls(val x: Exp[DSLType]) extends AbsDSLTypeOpsCls {
    def name(implicit o: Overloaded1): String = x match {
      case Def(Tpe(name,tpePars,stage)) => name
      case Def(TpeInst(t,args)) => t.name(o)
      case Def(TpeClass(name,sig,tpePars)) => name
      case Def(TpeClassEvidence(name,sig,tpePars)) => name
      case Def(TpeClassInst(name,tpePars,t)) => name
      case Def(FTpe(args,ret,freq)) => "Function"
      case Def(TpePar(name,ctx,s)) => name
      case Def(HkTpePar(name,tpePars,ctx,s)) => name
      case Def(VarArgs(t)) => "*" + t.name(o)
    }
    def tpePars = x match {
      case Def(Tpe(s,tpePars,stage)) => tpePars
      case Def(TpeInst(t,args)) => Nil
      case Def(TpePar(name,ctx,s)) => Nil
      case Def(HkTpePar(name,tpePars,ctx,s)) => tpePars
      case Def(TpeClass(name,sig,tpePars)) => tpePars
      case Def(TpeClassEvidence(name,sig,tpePars)) => tpePars collect { case t: Exp[TypePar] if t.tp == manifest[TypePar] => t }
      case Def(TpeClassInst(name,tpePars,t)) => tpePars
      case Def(FTpe(args,ret,freq)) => Nil
      case Def(VarArgs(t)) => Nil
    }
    def stage = x match {
      case Def(Tpe(s,tpePars,stage)) => stage
      case Def(TpeInst(Def(Tpe(s,tpePars,stage)),args)) => stage
      case Def(TpePar(name,ctx,stage)) => stage
      case Def(HkTpePar(name,tpePars,ctx,stage)) => stage
      case Def(TpeInst(Def(HkTpePar(name,tpePars,ctx,stage)),args)) => stage
      case Def(TpeClass(name,sig,pars)) => future
      case Def(TpeClassEvidence(name,sig,tpePars)) => future
      case Def(TpeClassInst(name,tpePars,t)) => future
      case Def(FTpe(args,ret,freq)) => future
      case Def(VarArgs(t)) => future
    }
    def tpeArgs = x match {
      case Def(TpeInst(t,args)) => args
      case _ => Nil
    }
  }

  /**
   * DSLTypeClass
   */
  implicit class DSLTypeClassOpsCls(val x: Exp[DSLTypeClass]) {
    def signature: TypeClassSignature = x match {
      case Def(TpeClass(name,sig,tpePars)) => sig
      case Def(TpeClassEvidence(name,sig,tpePars)) => sig
    }
  }

  /**
   * DSLTypeClassInst
   */
  implicit class DSLTypeClassInstOpsCls(val x: Exp[DSLTypeClassInst]) {
    def tpe(implicit o: Overloaded1): Exp[DSLType] = x match {
      case Def(TpeClassInst(name,tpePars,t)) => t
    }
  }

  /**
  * DSLArg
  */
  implicit class DSLArgOpsCls(val x: Exp[DSLArg]) {
    def name(implicit o: Overloaded3): String = x match  {
      case Def(Arg(name,tpe,default)) => name
    }
    def tpe(implicit o: Overloaded2): Exp[DSLType] = x match  {
      case Def(Arg(name,tpe,default)) => tpe
    }
    def default = x match  {
      case Def(Arg(name,tpe,default)) => default
    }
  }

  /**
   * DSLGroup
   */
  implicit class DSLGroupOpsCls(val x: Exp[DSLGroup]) {
    def name(implicit o: Overloaded4): String = x match {
      case Def(Grp(name)) => name
      case _ if grpIsTpe(x) => grpAsTpe(x).name
    }
  }

  /**
   * DSLOp
   */
  implicit class DSLOpOpsCls(val x: Exp[DSLOp]) {
    def args = x match {
      case Def(Op(grp, name, style, tpePars, args, Nil, implArgs, retTpe, eff, alias)) => args
      case Def(Op(grp, name, style, tpePars, args, curArgs, implArgs, retTpe, eff, alias)) => args ++ curArgs.flatten
    }

    def firstArgs = x match {
      case Def(Op(grp, name, style, tpePars, args, curArgs, implArgs, retTpe, eff, alias)) => args
    }

    def curriedArgs = x match {
      case Def(Op(grp, name, style, tpePars, args, curArgs, implArgs, retTpe, eff, alias)) => curArgs
    }

    def implicitArgs = x match {
      case Def(Op(grp, name, style, tpePars, args, curArgs, implArgs, retTpe, eff, alias)) => implArgs
    }

    def grp = x match {
      case Def(Op(grp, name, style, tpePars, args, curArgs, implArgs, retTpe, eff, alias)) => grp
    }

    def name(implicit o: Overloaded5) = x match {
      case Def(Op(grp, name, style, tpePars, args, curArgs, implArgs, retTpe, eff, alias)) => name
    }

    def style = x match {
      case Def(Op(grp, name, style, tpePars, args, curArgs, implArgs, retTpe, eff, alias)) => style
    }

    def tpePars(implicit o: Overloaded1) = x match {
      case Def(Op(grp, name, style, tpePars, args, curArgs, implArgs, retTpe, eff, alias)) => tpePars
    }

    def retTpe = x match {
      case Def(Op(grp, name, style, tpePars, args, curArgs, implArgs, retTpe, eff, alias)) => retTpe
    }

    def effect = x match {
      case Def(Op(grp, name, style, tpePars, args, curArgs, implArgs, retTpe, eff, alias)) => eff
    }

    def aliasHint = x match {
      case Def(Op(grp, name, style, tpePars, args, curArgs, implArgs, retTpe, eff, alias)) => alias
    }
  }

  /**
   * DSLData
   */
  implicit class DSLDataOpsCls(val x: Exp[DSLData]) {
    def tpe(implicit o: Overloaded3) = x match {
      case Def(Data(tpe, fields)) => tpe
    }

    def fields = x match {
      case Def(Data(tpe, fields)) => fields
    }
  }

 /**
  * Identifier
  */
  implicit class DSLIdentifierOpsCls(x: Exp[DSLIdentifier]) {
     def name(implicit o: Overloaded6): String = x match {
       case Def(Identifier(name,tpe)) => name
     }
     def tpe(implicit o: Overloaded4): Exp[DSLType] = x match {
       case Def(Identifier(name,tpe)) => tpe
     }
  }
}



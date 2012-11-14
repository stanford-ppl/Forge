package ppl.dsl.forge
package core

import java.io.{PrintWriter}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._

/**
 * These are user-facing ops that provide additional syntax and 
 * functionality required to specify a DSL.
 */
trait SpecOps extends Base {
  this: Forge =>

  /**
   * DSLOp
   */
  implicit def dslOpToPublicOps(x: Rep[DSLOp]) = new {
    def tpeInstance = new TpeInstanceAccessor(x)
    def quotedArg = new ArgInstanceAccessor(x)
  }
  
  class TpeInstanceAccessor(x: Rep[DSLOp]) {
    def apply(i: Int) = dslop_tpeinstance_access(x,i)
  }  
  def dslop_tpeinstance_access(x: Rep[DSLOp], i: Int): String
  
  class ArgInstanceAccessor(x: Rep[DSLOp]) {
    def apply(i: Int) = dslop_quotedarginstance_access(x,i)
  }
  def dslop_quotedarginstance_access(x: Rep[DSLOp], i: Int): String
  
  def infix_tpeName(x: Rep[DSLOp]): String

  /**
   * TypePar
   */
  def infix_withBound(a: Rep[TypePar], b: TypeClass): Rep[TypePar]
  
  /**
   * Misc
   */
  def quote(s: String) = "\""+s+"\""  
  def unquote(s: String) = "\"+"+s+"+\""
}

trait SpecOpsExp extends SpecOps {
  this: ForgeExp =>

  /**
   * DSLOp
   */
  def dslop_tpeinstance_access(x: Rep[DSLOp], i: Int) = x match {
    case Def(Op(tpe,name,style,tpePars,args,implArgs,retTpe,opTpe,eff)) => unquote("remap(" + opIdentifierPrefix + "." + TManifest.prefix + tpePars(i).name + ")")
  }  
  
  def dslop_quotedarginstance_access(x: Rep[DSLOp], i: Int) = x match {
    case Def(Op(tpe,name,style,tpePars,args,implArgs,retTpe,opTpe,eff)) => unquote("quote(" + opArgPrefix + i + ")")
  }
  
  def infix_tpeName(x: Rep[DSLOp]) = x match {
    case Def(Op(tpe,name,style,tpePars,args,implArgs,retTpe,opTpe,eff)) => tpe.name
  }  
  
  /**
   * TypePar
   */
  def infix_withBound(a: Rep[TypePar], b: TypeClass) = tpePar(a.name, b :: a.ctxBounds)
}
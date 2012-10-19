package ppl.dsl.meta
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
  this: MetaDSL =>

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
   * TypeArg
   */
  def infix_withBound(a: Rep[TypeArg], b: TypeClass): Rep[TypeArg]
  
  /**
   * Misc
   */
  def quote(s: String) = "\""+s+"\""  
  def unquote(s: String) = "\"+"+s+"+\""
}

trait SpecOpsExp extends SpecOps {
  this: MetaDSLExp =>

  /**
   * DSLOp
   */
  def dslop_tpeinstance_access(x: Rep[DSLOp], i: Int) = x match {
    case Def(Op(tpe,name,style,tpeArgs,args,implArgs,retTpe,opTpe,eff)) => unquote("remap(" + opIdentifierPrefix + "." + TManifest.prefix + tpeArgs(i).name + ")")
  }  
  
  def dslop_quotedarginstance_access(x: Rep[DSLOp], i: Int) = x match {
    case Def(Op(tpe,name,style,tpeArgs,args,implArgs,retTpe,opTpe,eff)) => unquote("quote(" + opArgPrefix + i + ")")
  }
  
  def infix_tpeName(x: Rep[DSLOp]) = x match {
    case Def(Op(tpe,name,style,tpeArgs,args,implArgs,retTpe,opTpe,eff)) => tpe.name
  }  
  
  /**
   * TypeArg
   */
  def infix_withBound(a: Rep[TypeArg], b: TypeClass) = tpeArg(a.name, b :: a.ctxBounds)
}
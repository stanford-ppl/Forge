package ppl.dsl.forge
package format

import java.io.{PrintWriter}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import core._

/**
 * These are helpers for formatting strings (codegen) nodes for a DSL. 
 * 
 * They also provide pass-throughs for some user-facing field accesses that are
 * required for codegen formatting.
 */
trait QuoteOps extends Base {
  this: Forge =>

  /**
   * DSLOp accessors
   */
  implicit def dslOpToPublicOps(x: Rep[DSLOp]) = new {
    def tpeInstance(i: Int) = quote_tpeinstance(x,i)
    def tpeName = quote_tpename(x)
    def quotedArg(i: Int) = quote_quotedarginstance(x,i)
    // def arg(i: Int) = quote_arginstance(x,i)
  }
  
  def quote_tpeinstance(x: Rep[DSLOp], i: Int): String
  def quote_tpename(x: Rep[DSLOp]): String  
  // def quote_arginstance(x: Rep[DSLOp], i: Int): Rep[DSLType]
  def quote_quotedarginstance(x: Rep[DSLOp], i: Int): String
       
  /**
   * Function tpes
   */
  def blockResult(x: Rep[DSLOp], argIndex: Int) = quote_blockresult(x,argIndex)
  
  def quote_blockresult(x: Rep[DSLOp], argIndex: Int): Rep[String]

  /**
   * Util
   */ 
  def quotes(s: String) = "\""+s+"\""  
  def unquotes(s: String) = "\"+"+s+"+\""
}

trait QuoteOpsExp extends QuoteOps {
  this: ForgeExp =>

  /**
   * DSLOp accessors
   */
  def quote_tpeinstance(x: Rep[DSLOp], i: Int) = unquotes("remap(" + opIdentifierPrefix + "." + TManifest.prefix + x.tpePars.apply(i).name + ")")
  def quote_tpename(x: Rep[DSLOp]) = x.grp.name
  // def quote_arginstance(x: Rep[DSLOp], i: Int) = x.args.apply(i)
  def quote_quotedarginstance(x: Rep[DSLOp], i: Int) = unquotes("quote(" + opArgPrefix + i + ")")
  
  /**
   * Function types
   */
    
  case class QuoteBlockResult(name: String, args: List[Rep[DSLType]], ret: Rep[DSLType]) extends Def[String]
  
  def quote_blockresult(x: Rep[DSLOp], argIndex: Int) = x.args.apply(argIndex) match {
    case Def(FTpe(args,ret)) => QuoteBlockResult(opArgPrefix + argIndex,args,ret)
    case _ => err("cannot quote block result of non-function type " + x.name)
  }
}
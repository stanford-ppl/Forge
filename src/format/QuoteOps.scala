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
 * 
 * There is a general issue with this architecture in terms of mixing staged and
 * unstaged strings: since Forge does not currently schedule program ops (i.e. through LMS),
 * only statically resolvable Exps can be used (if we cannot statically resolve a string
 * during quoting, we throw an error). The Exps are still useful for specifying different
 * implementations for different code generators (e.g. via quote), as opposed to everything
 * being a concrete string up front.
 */
trait QuoteOps extends Base {
  this: Forge =>

  /**
   * DSLOp accessors
   */
  implicit def dslOpToPublicOps(x: Rep[DSLOp]) = new {
    def tpeInstance(i: Int) = quote_tpeinstance(x,i)
    def tpeName = quote_tpename(x)
    def quotedArg(i: Int) = quote_quotedarginstance(/*x,*/i)
    def quotedArg(name: String) = quote_quotedarginstance(name)
  }

  def quotedArg(i: Int) = quote_quotedarginstance(i)
  def quotedArg(name: String) = quote_quotedarginstance(name)
  def quote_tpeinstance(x: Rep[DSLOp], i: Int): String
  def quote_tpename(x: Rep[DSLOp]): String  
  // def quote_arginstance(x: Rep[DSLOp], i: Int): Rep[DSLType]
  // def quote_quotedarginstance(x: Rep[DSLOp], i: Int): String
  def quote_quotedarginstance(i: Int): String
  def quote_quotedarginstance(name: String): String

  // convenience method for handling Seq[_] in code generators
  // TODO: instead of providing multiple methods, e.g. quotedArg and quotedSeq, should we have a more generic quote?
  //  quote(i) as Seq
  //  quote(anew.args(0)) // type is VarArgs
  def quotedSeq(i: Int): Rep[String]
  
  /**
   * Function tpes
   */
  def blockResult(x: Rep[DSLOp], argIndex: Int) = quote_blockresult(x,argIndex)
  
  def quote_blockresult(x: Rep[DSLOp], argIndex: Int): Rep[String]

  /**
   * Util
   */ 
  
  // def quotes(s: String) = "\""+s+"\""  
  // def unquotes(s: String) = "\"+"+s+"+\""
  
  // wildcards are used to insert the quotations *after* quoting, since quoting can actually change \"
  // it would be nice if, instead of using string wildcards, we could use symbols here, e.g. Quotes(s) and Unquotes(s)
  // the reason we don't is that it's harder to process these symbols outside-in, i.e. what we want to do is run quote on the entire string and then replace the wildcards
  def quotes(s: String) = qu+s+qu
  def unquotes(s: String) = qu+"+"+s+"+"+qu  
}

trait QuoteOpsExp extends QuoteOps {
  this: ForgeExp =>

  /**
   * DSLOp accessors
   */
  def quote_tpeinstance(x: Rep[DSLOp], i: Int) = unquotes("remap(" + opIdentifierPrefix + "." + TManifest.prefix + x.tpePars.apply(i).name + ")")
  def quote_tpename(x: Rep[DSLOp]) = x.grp.name
  def quote_quotedarginstance(i: Int) = unquotes("quote(" + opArgPrefix + i + ")")
  def quote_quotedarginstance(name: String) = unquotes("quote(" + name + ")")
  
  /**
   * Sequences
   */  
  case class QuoteSeq(arg: String) extends Def[String]
  def quotedSeq(arg: Int) = QuoteSeq(opArgPrefix + arg)
  def quotedSeq(arg: String) = QuoteSeq(arg)
  
  /**
   * Function types
   */    
  case class QuoteBlockResult(name: String, args: List[Rep[DSLArg]], ret: Rep[DSLType]) extends Def[String]
  
  def quote_blockresult(x: Rep[DSLOp], argIndex: Int) = x.args.apply(argIndex) match {
    case Def(Arg(name, Def(FTpe(args,ret,freq)), d2)) => QuoteBlockResult(name,args,ret)
    case _ => err("cannot quote block result of non-function type " + x.name)
  }
}

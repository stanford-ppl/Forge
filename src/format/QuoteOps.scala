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
    def tpeName = quote_tpename(x)
  }

  def quotedArg(i: Int) = quote_quotedarginstance(opArgPrefix+i)
  def quotedArg(name: String) = quote_quotedarginstance(name)
  def quotedTpe(i: Int, op: Rep[DSLOp]) = quote_tpeinstance(i, op)
  def quotedTpe(name: String, op: Rep[DSLOp]) = quote_tpeinstance(name, op)
  def quotedBlock(i: Int, op: Rep[DSLOp], capturedArgs: List[String]) = quote_quotedblock(opArgPrefix+i, op, capturedArgs)
  def quotedBlock(name: String, op: Rep[DSLOp], capturedArgs: List[String]) = quote_quotedblock(name, op, capturedArgs)

  // these need to return String and use wildcards instead of Rep[String] in order to interoperate properly with string interpolation
  def quote_tpename(x: Rep[DSLOp]): String
  def quote_quotedarginstance(name: String): String
  def quote_tpeinstance(i: Int, op: Rep[DSLOp]): String
  def quote_tpeinstance(name: String, op: Rep[DSLOp]): String
  def quote_quotedblock(name: String, op: Rep[DSLOp], capturedArgs: List[String]): String

  // convenience method for handling Seq[_] in code generators
  // TODO: instead of providing multiple methods, e.g. quotedArg and quotedSeq, should we have a more generic quote?
  //  quote(i) as Seq
  //  quote(anew.args(0)) // type is VarArgs
  def quotedSeq(i: Int): Rep[String]


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

  def quote_tpename(x: Rep[DSLOp]) = x.grp.name

  def quote_tpeinstance(i: Int, op: Rep[DSLOp]) = unquotes("remap(" + opIdentifierPrefix + "." + TManifest.prefix + op.tpePars.apply(i).name + ")")
  def quote_tpeinstance(name: String, op: Rep[DSLOp]) = {
    val i = op.tpePars.indexWhere(_.name == name)
    if (i < 0) err("no tpe par " + name + " in op " + op.name)
    quote_tpeinstance(i, op)
  }

  def quote_quotedarginstance(name: String) = unquotes("quote(" + name + ")")

  // Function block args require the op to be passed (in order to extract the arguments to the function) to be quoted correctly
  // investigate: can we remove this dependency to simplify the preprocessor?
  case class QuoteBlockResult(func: Rep[DSLArg], args: List[Rep[DSLArg]], ret: Rep[DSLType], capturedArgs: List[String]) extends Def[String]

  def quote_quotedblock(name: String, op: Rep[DSLOp], capturedArgs: List[String]) = op.args.find(_.name == name) match {
    case None => err("could not quote arg - no arg name " + name + " in op " + op.name)
    case Some(a@Def(Arg(name, f@Def(FTpe(args,ret,freq)), d2))) =>
      if (!isThunk(f) && args.length != capturedArgs.length) err("wrong number of captured args for quoted block in op " + op.name + " (expected " + args.length + ")")
      symMarker + toAtom(QuoteBlockResult(a,args,ret,capturedArgs)).asInstanceOf[Sym[Any]].id + symMarker
    case _ => quote_quotedarginstance(name)
  }

  /**
   * Sequences
   */
  case class QuoteSeq(arg: String) extends Def[String]
  def quotedSeq(arg: Int) = QuoteSeq(opArgPrefix + arg)
  def quotedSeq(arg: String) = QuoteSeq(arg)
}

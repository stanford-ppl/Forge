package ppl.dsl.forge
package templates
package ident

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.util.matching._
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._

import core._
import shared.{BaseGenOps,BaseGenDataStructures}
import Utilities._

trait IdentGenOps extends BaseGenOps with BaseGenDataStructures {
  this: ForgeCodeGenIdent =>

  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  // TODO: cleanup
  def xx[T](x:Rep[T]): Def[T] = Def.unapply(x).get
  def xxx[T](x:List[Rep[T]]): List[Def[T]] = x.map(xx)
  def zzz(x:Any): String = x match {
    case x: String => "\"" + x + "\""
    case x: Rep[Any] => "toAtom("+zzz(Def.unapply(x).get)+")"
    case Nil => "Nil"
    case xs: List[Any] => "List(" + xs.map(zzz).mkString(",") + ")"
    case xs: Seq[Any] => "Seq(" + xs.map(zzz).mkString(",") + "):_*"
    case xs: Product if xs.productArity == 0 => xs.productPrefix
    case xs: Product => xs.productPrefix + "(" + xs.productIterator.map(zzz).mkString(",") + ")"
    case _ => ""+x
  }

  def emitClass(opsGrp: DSLOps, stream: PrintWriter) {
    var scope = false
    val baseIndent = 4
    if (grpIsTpe(opsGrp.grp)) {
      val tpe = grpAsTpe(opsGrp.grp)
      val d = DataStructs.get(tpe)
      d.foreach { data =>
        emitWithIndent("val " + data.tpe.name + " = " + zzz(data.tpe), stream, baseIndent)
        stream.println()
        emitWithIndent("data(" + data.tpe.name + ", " + zzz(data.fields) + ")", stream, baseIndent)
        stream.println()
      }
      emitWithIndent("val " + opsGrp.name + " = withTpe (" + opsGrp.grp.name + ")", stream, baseIndent)
      emitWithIndent(opsGrp.name + " {", stream, baseIndent)
      scope = true
    } else {
      emitWithIndent("val " + opsGrp.grp.name + " = grp (\"" + opsGrp.grp.name + "\")", stream, baseIndent)
    }

    stream.println()

    val scopeIndent = if (scope) baseIndent+2 else baseIndent

    for (o <- unique(opsGrp.ops)) {
      emitWithIndent("// " + o.name, stream, scopeIndent)
      emitWithIndent("// " + zzz(o), stream, scopeIndent)

      def forge_op0(style: String, hg: Boolean)(grp: Rep[DSLGroup])(name: String, tpePars: List[Rep[TypePar]],
        args: List[Rep[DSLArg]], curriedArgs: List[List[Rep[DSLArg]]], implicitArgs: List[Rep[DSLArg]], retTpe: Rep[DSLType],
        effect: EffectType = pure, aliasHint: AliasHint = nohint) = {

        // TODO: reverse engineer method signatures

        val signature = if (curriedArgs.isEmpty) MethodSignature(args,retTpe)
                        else CurriedMethodSignature(curriedArgs,retTpe)

        forge_op1(style,hg)(grp.name)(zzz(name),zzz(tpePars),zzz(signature),zzz(implicitArgs),zzz(effect),zzz(aliasHint))
      }
      def forge_op1(style: String, hg: Boolean)(grp: String)(name: String, tpePars: String, signature: String, implicitArgs: String, effect: String, aliasHint: String) =
          if (!scope) emitWithIndent(s"$style ($grp)($name, $tpePars, $signature, $implicitArgs, $effect, $aliasHint)", stream, scopeIndent)
          else emitWithIndent(s"$style ($name)($signature, $implicitArgs, $tpePars, $effect, $aliasHint)", stream, scopeIndent)
          // is tpePars == addTpePars ??

      val Def(op: Op) = o

      val (key,hg) = op.style match {
        case `staticMethod`   => ("static",true)
        case `infixMethod`    => ("infix",true)
        case `directMethod`   => ("direct",true)
        case `compilerMethod` => ("compiler",false)
        case `implicitMethod` => ("fimplicit",false)
      }

      forge_op0(key,hg)(op.grp)(op.name, op.tpePars, op.args, op.curriedArgs, op.implicitArgs, op.retTpe, op.effect, op.aliasHint)

      // TODO: implements clauses

      stream.println()
    }

    if (scope) emitWithIndent("}", stream, baseIndent)

  }
}

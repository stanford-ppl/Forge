package ppl.dsl.forge
package templates
package shared

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._
import org.scala_lang.virtualized.SourceContext
import core._

import Utilities._

trait BaseGenTypeClasses extends BaseGenOps {
  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  def checkTpeClassInst(tpeClass: Rep[DSLType], inst: Rep[DSLType]) {
    val classOps = OpsGrp(tpeClass).ops
    val instOps = OpsGrp(inst).ops

    if (instOps.exists(o => !Impls.contains(o))) err("type class instance " + inst.name + " has op with no corresponding impl")
    // tpeClass instance ops must be implemented as 'composite', because we only generate them in shared
    // the reasoning here is that type classes are really a front-end dispatch mechanism, not a set of implementation ops
    if (instOps.exists(o => !Impls(o).isInstanceOf[Composite])) err("type class instance " + inst.name + " has op impl that is not composite, which is not allowed")
    if (instOps.map(_.name) != classOps.map(_.name)) err("type class instance " + inst.name + " does not implement all members of type class " + tpeClass.name)
  }

  def emitTpeClass(grp: Rep[DSLGroup], opsGrp: DSLOps, stream: PrintWriter) {
    if (opsGrp.ops.exists(o => Impls.contains(o))) warn("type class " + grp.name + " contains ops with defined impls - these will be ignored")
    val tpeCls = asTpeClass(grp)

    // mixing in ExtraImplicits is a bit of a hack to prioritize any implicits in this type class over Numeric.. but it's really only relevant for Arith
    stream.println("trait " + opsGrp.name + " extends " + baseOpsCls(opsGrp.grp) + " with scala.math.Numeric.ExtraImplicits {")
    stream.println("  this: " + dsl + " => ")
    stream.println()
    emitBlockComment("Type class", stream, indent=2)
    stream.println("  trait " + quote(tpeCls) + " {")
    for (o <- opsGrp.ops) {
      stream.println("    def " + o.name + makeTpeParsWithBounds(o.tpePars.filterNot(p => tpeCls.tpePars.map(_.name).contains(p.name)))
        + makeOpArgsWithType(o, addParen = o.effect != pure) + makeImplicitArgsWithCtxBoundsWithType(withoutHkTpePars(o.tpePars), o.args, o.implicitArgs, without = tpeCls.tpePars) + ": " + repify(o.retTpe))
    }
    stream.println("  }")
    stream.println()

    // generate the hack we need for Delite mirroring
    if (tpeCls.tpePars.length == 0) {
      stream.println("  def " + tpeCls.signature.wrapper.get + "(x: " + tpeCls.name + ") = x")
    }
    else if (tpeCls.tpePars.length == 1) {
      stream.println("  def " + tpeCls.signature.wrapper.get + "[A,B](x: " + tpeCls.name + "[A]) = x.asInstanceOf["+tpeCls.name+"[B]]")
    }
    else {
      warn("could not auto-generate type class wrapper " + tpeCls.signature.wrapper + " for " + tpeCls.name + " -- too many type parameters")
    }
    stream.println()

    emitBlockComment("Type class instances", stream, indent=2)
    stream.println()

    val instances = OpsGrp filter { case (grp,ops) => isTpeClassInst(grp) && getHkTpe(asTpeClassInst(grp).tpe).name == tpeCls.name }
    for ((i, instOpsGrp) <- instances) {
      val inst = asTpeClassInst(i)
      checkTpeClassInst(tpeCls, inst)
      stream.println("  implicit def can" + inst.name + makeTpeParsWithBounds(inst.tpePars) + ": " + quote(inst) + " = new " + quote(inst) + " {")
      // val instantiatedTpes = inst.tpe match { case Def(TpeInst(_, args)) => args }
      for (o <- instOpsGrp.ops) {
        // add context bounds we might have lost in the instantiation
        // val missingTpePars = opsGrp.ops.find(_.name == o.name).get.tpePars diff o.tpePars
        // if (instantiatedTpes.length != missingTpePars.length) err("in type class " + tpe.name + ", expected " + missingTpePars.length + " type pars but found " + instantiatedTpes.length)
        // val implicits = implicitCtxBoundsWithType(missingTpePars.zip(instantiatedTpes) map { case (mtp, itp) => tpePar(itp.name, mtp.ctxBounds) }) ::: o.implicitArgs
        val implicits = o.implicitArgs

        stream.println("    def " + o.name + makeTpeParsWithBounds(o.tpePars.filterNot(p => inst.tpePars.map(_.name).contains(p.name))) + makeOpArgsWithType(o, addParen = o.effect != pure)
          + (makeImplicitArgsWithCtxBoundsWithType(o.tpePars, o.args, implicits, without = inst.tpePars)) + " = {")
        inline(o, Impls(o).asInstanceOf[Composite].func, quoteLiteral).split(nl).foreach { line => emitWithIndent(line, stream, 6) }
        stream.println("    }")
      }
      stream.println("  }")
      stream.println()
    }
    stream.println()

    // We may want to add an option to disable this (or make not generating anything in this case the non-default behavior.)
    if (tpeCls.tpePars.length > 1) {
      warn("type class " + tpeCls.name + " has more than 1 type parameter; generating forwarders assuming, " +
           "by convention, that the last parameter is the promotion type")
    }

    emitBlockComment("Forwarders - these allow infix notation to be used when the type class is available", stream, indent=2)
    // Manually construct implicit parameters to forwarder
    val tpeClsArgs = arg("__tc", tpeCls.signature.apply(tpeCls.tpePars: _*))
    // This is normally handled by hkInstantiations, but here we are faking the self instance using 'repify' below
    val tpeClsPars = tpeCls.tpePars.flatMap(a => a.ctxBounds.map(b => ephemeralTpe(b.name+"["+quote(a)+"]", stage = now))).distinct
    val tpeClsBounds = tpeClsPars.zipWithIndex.map(t => arg(implicitCtxBoundArgPrefix+t._2, t._1))

    stream.println("  implicit class " + tpeCls.name + "2" + tpeCls.name + "Ops" + makeTpePars(tpeCls.tpePars)
      + "(self: " + repify(tpeCls.tpePars.last) + ")" + makeImplicitArgsWithType(tpeCls.tpePars, arg("self",tpeCls.tpePars.last), tpeClsBounds ++ tpeClsArgs) + "{")
    for (o <- opsGrp.ops) {
      // again, quite redundant with shared/Ops.scala.
      // We currently don't support tpeCls methods requiring additional type parameters than are available in the type class. (Not clear this is necessary)
      val otherArgs = makeArgsWithNowType(o.firstArgs.drop(1))
      stream.println("    def " + o.name + otherArgs
        + (makeImplicitArgsWithType(withoutHkTpePars(o.tpePars), o.args, o.implicitArgs))
        + " = " + makeOpMethodName(o) + makeTpeParsAsArgs(tpeCls.tpePars, tpeCls.tpePars) + makeArgs(o.args, a => if (a.name == o.args.apply(0).name) "self" else simpleArgName(a)))
    }
    stream.println("  }")
    stream.println()
    for (o <- opsGrp.ops) {
      stream.println("  def " + makeOpMethodName(o) + makeTpePars(tpeCls.tpePars)
        + makeOpArgsWithType(o) + makeImplicitArgsWithCtxBoundsWithType(tpeCls.tpePars, o.args, o.implicitArgs :+ tpeClsArgs)
        + ": " + repify(o.retTpe) + " = __tc." + o.name + makeOpArgs(o, addParen = o.effect != pure))
    }

    stream.println("}")
  }
}

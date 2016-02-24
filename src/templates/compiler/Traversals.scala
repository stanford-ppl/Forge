package ppl.dsl.forge
package templates
package compiler

import java.io.PrintWriter
import scala.virtualization.lms.common._
import core._
import Utilities._

trait BaseGenTraversals extends ForgeCodeGenBase {
  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  def hasIR(t: Rep[DSLTraversal]) = t match {
    case Def(Traverse(_,_)) => false
    case _ => true
  }

  def makeTraversalName(t: Rep[DSLTraversal]) = t match {
    case Def(Traverse(name,isExtern)) => name
    case Def(Transform(name,isExtern)) => name + "Transformer"
    case Def(Analyze(name,isExtern)) => name + "Analyzer"
  }
  def makeTraversalIRName(t: Rep[DSLTraversal]) = t match {
    case Def(Transform(name,isExtern)) => name + "TransformExp"
    case Def(Analyze(name,isExtern)) => name + "AnalysisExp"
  }
}

trait DeliteGenTraversals extends BaseGenTraversals {
  this: ForgeCodeGenDelite =>
  import IR._

  // --- Traversals
  def emitTraversalDefs(t: Rep[DSLTraversal], stream: PrintWriter) = t match {
    case Def(Analyze(_,_)) =>
      val az = t.asInstanceOf[Rep[DSLAnalyzer]]
      emitAnalyzer(az, stream)
      stream.println()
      emitAnalyzerExp(az, stream)
    case Def(Transform(_,_)) =>
      val tr = t.asInstanceOf[Rep[DSLTransformer]]
      emitTransformer(tr, stream)
      stream.println()
      emitTransformerExp(tr, stream)
  }

  // --- Transformers
  def makeLowerMethodName(o: Rep[DSLOp]) = "lower_" + makeOpMethodName(o)
  def makeLowerMethodCall(o: Rep[DSLOp]) = {
    val xformArgs = makeTransformedArgs(o)
    val implicits = makeTransformedImplicits(o)  // TODO: Need to transform these further to allow type changes
    val implicitsWithParens = if (implicits.isEmpty) "" else implicits.mkString("(",",",")")

    makeLowerMethodName(o) + xformArgs + implicitsWithParens
  }

  // TODO: Doesn't handle methods which require implicit overload arguments
  def makeLowerMethodSignature(o: Rep[DSLOp]) = {
    val implicitArgs = makeOpImplicitArgsWithType(o)
    "def " + makeLowerMethodName(o) + makeTpeParsWithBounds(o.tpePars) + makeOpArgsWithType(o) + implicitArgs
  }

  def emitTransformer(t: Rep[DSLTransformer], stream: PrintWriter) {
    // TODO: May not want to always extend TunnelingTransformer or have this behavior?
    stream.println("trait " + makeTraversalName(t) + " extends TunnelingTransformer {")
    stream.println("  val IR: " + makeTraversalIRName(t))
    stream.println("  import IR._")
    stream.println("  override val name = \"" + makeTraversalName(t) + "\"")
    stream.println("  override val debugMode = false")        // TODO: Should be able to change this
    stream.println()
    stream.println("  override def transformTP[A](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = rhs match {")

    val patterns = Transformers(t).rules
    for ((op,rules) <- patterns) {
      if (!hasIRNode(op) || hasMultipleIRNodes(op)) {
        err("Cannot create transformation rule for op " + op.name + ": Op must be represented by exactly one IR node")
      }
      emitWithIndent("case " + opIdentifierPrefix + "@" + makeOpSimpleNodeNameWithAnonArgs(op) + " => " + makeLowerMethodCall(op), stream, 4)
    }
    stream.println("    case _ => super.transformTP(lhs, rhs)") // TODO: Should be able to change this
    stream.println("  }")

    for ((op,rules) <- patterns) {
      emitTraversalRules(op, rules, true, stream, 2, makeLowerMethodName)
    }

    stream.println("}")
  }

  // TODO: Is an IR trait for every transformer really needed?
  def emitTransformerExp(t: Rep[DSLTransformer], stream: PrintWriter) {
    stream.println("trait " + makeTraversalIRName(t) + " extends " + dsl + "Transforming {")
    stream.println("  this: " + dsl + "Compiler with " + dsl + "Application with DeliteApplication =>")
    stream.println("}")
  }

  // --- Analyzers
  // TODO: Eventually want to handle TTP as well, but doesn't make sense to until after fusion changes
  def emitAnalyzer(az: Rep[DSLAnalyzer], stream: PrintWriter) {
    stream.println("trait " + makeTraversalName(az) + " extends AnalyzerBase {")
    stream.println("  val IR: " + makeTraversalIRName(az))
    stream.println("  import IR._")
    stream.println("  override val name = \"" + makeTraversalName(az) + "\"")
    stream.println("  override val debugMode = false")        // TODO: Should be able to change this
    stream.println("  override val autopropagate = true")     // TODO: Should be able to change this
    stream.println()
    stream.println("  override def processTP(lhs: Exp[Any], rhs: Def[Any])(implicit ctx: SourceContext) = rhs match {")

    val patterns = Analyzers(az).rules
    for ((op,rules) <- patterns) {
      if (!hasIRNode(op) || hasMultipleIRNodes(op)) {
        err("Cannot create analysis rule for op " + op.name + ": Op must be represented by exactly one IR node")
      }
      emitTraversalRules(op, rules, false, stream, 4, makeOpMethodName)
    }

    stream.println("    case _ => super.processTP(lhs, rhs)") // TODO: Should be able to change this
    stream.println("  }")
    stream.println("}")
  }

  // TODO: Is an IR trait for every analyzer needed?
  def emitAnalyzerExp(az: Rep[DSLAnalyzer], stream: PrintWriter) {
    stream.println("trait " + makeTraversalIRName(az) + " extends " + dsl + "Exp {")
    stream.println("  this: " + dsl + "Compiler with " + dsl + "Application with DeliteApplication =>")
    stream.println("}")
  }
}

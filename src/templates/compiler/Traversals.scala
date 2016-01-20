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

  def makeTraversalName(t: Rep[DSLTraversal]) = t match {
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

  // --- Metadata
  def emitMetadataDefs(m: Rep[DSLMetadata], stream: PrintWriter) {
    stream.println("trait " + m.name + "MetadataOps extends MetadataOps {")
    stream.println()
    stream.println("}")
  }

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
  def emitTransformer(t: Rep[DSLTransformer], stream: PrintWriter) {
    // TODO: May not want to always extend TunnelingTransformer?
    stream.println("trait " + makeTraversalName(t) + " extends TunnelingTransformer {")
    stream.println("  val IR: " + makeTraversalIRName(t))
    stream.println("  import IR._")
    stream.println()
    stream.println("  override def transformTP[A](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = rhs match {")
    stream.println("    case _ => super.transformTP(lhs, rhs)") // TODO: Should be able to change this
    stream.println("  }")
    stream.println("}")
  }
  def emitTransformerExp(t: Rep[DSLTransformer], stream: PrintWriter) {
    stream.println("trait " + makeTraversalIRName(t) + " extends " + dsl + "OpsExp {")
    stream.println("  this: DeliteApplication with " + dsl + "Application =>")
    stream.println("}")
  }

  // --- Analyzers
  // TODO: Eventually want to handle TTP as well, but doesn't make sense to until after fusion changes
  def emitAnalyzer(az: Rep[DSLAnalyzer], stream: PrintWriter) {
    stream.println("trait " + makeTraversalName(az) + " extends AnalyzerBase {")
    stream.println("  val IR: " + makeTraversalIRName(az))
    stream.println("  import IR._")
    stream.println("  override val autopropagate = true")     // TODO: Should be able to change this
    stream.println("  override def processTP(lhs: Exp[Any], rhs: Def[Any])(implicit ctx: SourceContext) = rhs match {")
    stream.println("    case _ => super.processTP(lhs, rhs)") // TODO: Should be able to change this
    stream.println("  }")
    stream.println("}")
  }

  def emitAnalyzerExp(az: Rep[DSLAnalyzer], stream: PrintWriter) {
    stream.println("trait " + makeTraversalIRName(az) + " extends " + dsl + "OpsExp {")
    stream.println("  this: DeliteApplication with " + dsl + "Application =>")
    stream.println("}")
  }
}
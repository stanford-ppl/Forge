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

  def quoteFunc(f: MetaMeet) = f match {
    case `metaUpdate` => "MetaOverwrite"
    case `branch` => "BranchAlias"
    case `mutate` => "UpdateAlias"
    case `any` => "_"
  }

  // --- Metadata
  def emitMetadataClasses(stream: PrintWriter) {

    val meetFuncs = List(metaUpdate,branch,mutate,any)

    val MetaTpes = Tpes.filter(t => !isForgePrimitiveType(t) && DataStructs.contains(t) && isMetaType(t))
    if (MetaTpes.nonEmpty) {
      stream.println("trait " + dsl + "MetadataOps {")
      stream.println("  this: " + dsl + "Compiler =>")
      stream.println()
      emitBlockComment("DSL metadata types", stream, 2)
      for (tpe <- MetaTpes) {
        val m = tpe.asInstanceOf[Rep[DSLMetadata]]
        val struct = DataStructs(tpe)
        val meetRules = MetaImpls(m).meet
        val matchesRule = MetaImpls(m).matches
        val canMeetRules = MetaImpls(m).canMeet
        val completeRule = MetaImpls(m).complete

        stream.print("  case class " + m.name)
        stream.print(makeTpeParsWithBounds(struct.tpe.tpePars))
        stream.print("(")
        stream.print(struct.fields.map{case (name,tpe) => name + ": " + quote(tpe) }.mkString(", "))
        stream.println(") extends Metadata { self =>")
        stream.println("    def _meet(that: self.type)(implicit t: MeetFunc): Metadata = t match {")
        if (meetRules.contains(metaUpdate)) {
          stream.println("      case " + quoteFunc(metaUpdate) + " =>")
          quoteLiteral(meetRules(metaUpdate)).split(nl).foreach{ line => emitWithIndent(line, stream, 8) }
        }
        else stream.println("      case " + quoteFunc(metaUpdate) + " => that")

        if (!meetRules.contains(any))
          err("No generic meet function defined on metadata type " + m.name + ". Define one using 'alias = any'")

        for (func <- meetFuncs.drop(1) if meetRules.contains(func)) {
          stream.println("      case " + quoteFunc(func) + " =>")
          quoteLiteral(meetRules(func)).split(nl).foreach{ line => emitWithIndent(line, stream, 8) }
        }

        stream.println("    }")

        // Matching tests
        stream.println("    def _matches(that: self.type): Boolean = {")
        if (matchesRule.isEmpty)
          stream.println("      this == that")
        else
          quoteLiteral(matchesRule.get).split(nl).foreach{ line => emitWithIndent(line, stream, 6) }
        stream.println("    }")

        // Compatibility tests
        stream.println("    def _canMeet(that: self.type)(implicit t: MeetFunc): Boolean = t match {")
        for (func <- meetFuncs if canMeetRules.contains(func)) {
          stream.println("      case " + quoteFunc(func) + " => ")
          quoteLiteral(canMeetRules(func)).split(nl).foreach{ line => emitWithIndent(line, stream, 8) }
        }
        if (!canMeetRules.contains(any)) {
          stream.println("      case _ => true")
        }
        stream.println("    }")
        // Incompatibility error strings
        stream.println("    def _incompatibilities(that: self.type)(implicit t: MeetFunc): List[String] = {")
        stream.println("      if (_canMeet(that)) Nil else List(\"Incompatible metadata\")")
        stream.println("    }")

        // Completeness check
        if (completeRule.isDefined) {
          stream.println("    def _isComplete: Boolean = {")
          quoteLiteral(completeRule.get).split(nl).foreach{ line => emitWithIndent(line, stream, 6) }
          stream.println("    }")
        }
        stream.println("    }")

        stream.println("  }")
      }
    }
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
    stream.println("  this: " + dsl + "Compiler =>")
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
    stream.println("  this: " + dsl + "Compiler =>")
    stream.println("}")
  }
}
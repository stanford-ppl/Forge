package ppl.dsl.forge
package core

import java.io.{PrintWriter}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.collection.mutable.{ArrayBuffer,HashMap}

trait ForgeTraversalOps extends Base {
  this: Forge =>

  def transformer(name: String, isExtern: Boolean = false) = forge_transformer(name,isExtern)
  def analyzer(name: String, isExtern: Boolean = false) = forge_analyzer(name,isExtern)
  def schedule(traversal: Rep[DSLTraversal]) = forge_schedule(traversal)

  def meta(name: String) = forge_metadata(name)
  def lookupMeta(name: String) = forge_lookup_metadata(name)

  def forge_transformer(name: String, isExtern: Boolean): Rep[DSLTransformer]
  def forge_analyzer(name: String, isExtern: Boolean): Rep[DSLAnalyzer]
  def forge_schedule(traversal: Rep[DSLTraversal]): Rep[Unit]
  def forge_metadata(name: String): Rep[DSLMetadata]
  def forge_lookup_metadata(name: String): Rep[DSLMetadata]
  def forge_metadata_fields(meta: Rep[DSLMetadata], fields: Seq[(String, Rep[DSLType])]): Rep[Unit]

  def rewrite(op: Rep[DSLOp]) = forge_rewrite(op)
  def lower(xf: Rep[DSLTransformer])(op: Rep[DSLOp]) = forge_lower(xf, op)

  object rule {
    def apply(rule: Rep[String]) = forge_rule(rule)
  }
  object pattern {
    def apply(rule: (List[String], String)) = forge_pattern(rule._1, unit(rule._2), false)
  }
  object commutative {
    def apply(rule: (List[String], String)) = forge_pattern(rule._1, unit(rule._2), true)
  }

  def forge_rewrite(op: Rep[DSLOp]): Rep[DSLPattern]
  def forge_lower(t: Rep[DSLTransformer], op: Rep[DSLOp]): Rep[DSLPattern]
  def forge_rule(rule: Rep[String]): DSLRule
  def forge_pattern(pattern: List[String], rule: Rep[String], commutative: Boolean): DSLRule

  def forge_using(pattern: Rep[DSLPattern], rule: DSLRule)(implicit ctx: SourceContext): Rep[Unit]
}

trait ForgeTraversalSugarLowPriority extends ForgeTraversalOps {
  this: Forge =>

  implicit def singleStrToList(t: (String,String)): (List[String],String) = (List(t._1),t._2)
  implicit def tuple2StrToList(t: ((String,String),String)): (List[String],String) = (t._1,t._2) match {
    case (a,b) => (List(a._1,a._2), b)
  }
  implicit def tuple3StrToList(t: ((String,String,String),String)): (List[String],String) = (t._1,t._2) match {
    case (a,b) => (List(a._1,a._2,a._3), b)
  }
  implicit def tuple4StrToList(t: ((String,String,String,String),String)): (List[String],String) = (t._1,t._2) match {
    case (a,b) => (List(a._1,a._2,a._3,a._4), b)
  }
  implicit def tuple5StrToList(t: ((String,String,String,String,String),String)): (List[String],String) = (t._1,t._2) match {
    case (a,b) => (List(a._1,a._2,a._3,a._4,a._5), b)
  }
}

trait ForgeTraversalSugar extends ForgeTraversalSugarLowPriority with ForgeTraversalOps {
  this: Forge =>

  def infix_using(pattern: Rep[DSLPattern], rule: DSLRule)(implicit ctx: SourceContext) = forge_using(pattern, rule)

  /**
   * Transformer and analysis sugar scopes
   * Mimics type scopes (Scala-Virtualized scopes for sugar)
   */
  var _xFormScope: Option[Rep[DSLTransformer]] = None
  var _analysisScope: Option[Rep[DSLAnalyzer]] = None

  implicit class TransformOpsCls(xf: Rep[DSLTransformer]) {
    def apply[R](block: => R) {
      if (_xFormScope.isDefined) forge_err("Cannot create nested transformer scopes!")
      _xFormScope = Some(xf)   // set transformer scope
      new Scope[TransformScope, TransformScopeRunner[R], R](block)
      _xFormScope = None // reset transformer scope
    }
  }
  implicit class AnalysisOpsCls(az: Rep[DSLAnalyzer]) {
    def apply[R](block: => R) {
      if (_analysisScope.isDefined) forge_err("Cannot create nested analysis scopes!")
      _analysisScope = Some(az)
      new Scope[AnalysisScope, AnalysisScopeRunner[R], R](block)
      _analysisScope = None
    }
  }


  // --- Transformer scope sugar
  // TODO: Add other transformer methods here
  trait TransformScope {
    def lower(op: Rep[DSLOp]) = forge_lower(_xFormScope.get, op)
  }
  trait TransformScopeRunner[R] extends TransformScope {
    def apply: R
    val result = apply
  }

  // --- Analysis scope sugar
  trait AnalysisScope {

  }
  trait AnalysisScopeRunner[R] extends AnalysisScope {
    def apply: R
    val result = apply
  }
}

trait ForgeTraversalOpsExp extends ForgeTraversalSugar with BaseExp {
  this: ForgeExp =>

  /**
   * Compiler state
   */
  val Traversals = ArrayBuffer[Exp[DSLTraversal]]()
  val Transformers = HashMap[Exp[DSLTransformer], TraversalRules]()
  val Analyzers = HashMap[Exp[DSLAnalyzer], AnalysisRules]()
  val TraversalSchedule = ArrayBuffer[Exp[DSLTraversal]]()
  val Metadatas = ArrayBuffer[Exp[DSLMetadata]]()
  val MetaStructs = HashMap[Exp[DSLMetadata], Exp[DSLMetaFields]]()

  val Rewrites = HashMap[Exp[DSLOp], List[DSLRule]]()

  /**
   * Traversal op patterns
   */
  case class RewritePattern(op: Rep[DSLOp]) extends Def[DSLPattern]
  case class LowerPattern(xf: Rep[DSLTransformer], op: Rep[DSLOp]) extends Def[DSLPattern]

  def forge_rewrite(op: Rep[DSLOp]): Rep[DSLPattern] = RewritePattern(op)
  def forge_lower(xf: Rep[DSLTransformer], op: Rep[DSLOp]): Rep[DSLPattern] = LowerPattern(xf, op)


  /**
   * Analysis (metadata propagation) rules
   */
  abstract class AnalysisRule
  case class PropagationRule(rule: Rep[String]) extends AnalysisRule
  case class UpdateRule(index: Int, rule: Rep[String]) extends AnalysisRule

  def forge_propagates(rule: Rep[String]) = PropagationRule(rule)
  def forge_updates(index: Int, rule: Rep[String]) = UpdateRule(index, rule)

  case class AnalysisRules(patterns: HashMap[Rep[DSLOp],List[AnalysisRule]])
  object AnalysisRules {
    def empty = AnalysisRules(HashMap[Rep[DSLOp],List[AnalysisRule]]())
  }

  /**
   * Lowering/Rewrite (transformation) rules
   */
  case class SimpleRule(rule: Rep[String]) extends DSLRule
  case class PatternRule(pattern: List[String], rule: Rep[String], commutative: Boolean) extends DSLRule

  def forge_rule(rule: Rep[String]) = SimpleRule(rule)
  def forge_pattern(pattern: List[String], rule: Rep[String], commutative: Boolean)
    = PatternRule(pattern, rule, commutative)

  case class TraversalRules(rules: HashMap[Exp[DSLOp], List[DSLRule]]) {
    def contains(op: Rep[DSLOp]) = rules.contains(op)
  }
  object TraversalRules {
    def empty = TraversalRules(HashMap[Rep[DSLOp],List[DSLRule]]())
  }

  // Create or append rules. Note that definition ordering must be maintained for pattern matching!
  // TODO: Should this return something besides Rep[Unit]?
  def forge_using(pattern: Rep[DSLPattern], rule: DSLRule)(implicit ctx: SourceContext): Rep[Unit] = {

    // Check that the user doesn't give two simple rules for one op pattern
    def append_rule(oldRules: List[DSLRule]) = rule match {
      case rule: PatternRule => oldRules :+ rule
      case rule: SimpleRule =>
        // TODO: Better stringify method for patterns
        if (oldRules.exists(_.isInstanceOf[SimpleRule])) err("Pattern " + pattern + " already has a simple rule")
        else oldRules :+ rule
    }

    pattern match {
      case Def(RewritePattern(op)) if !Rewrites.contains(op) => Rewrites(op) = List(rule) // Create
      case Def(RewritePattern(op)) => Rewrites(op) = append_rule(Rewrites(op))
      case Def(LowerPattern(xf, op)) =>
        val rules = Transformers(xf).rules
        if (!rules.contains(op)) rules(op) = List(rule)
        else rules(op) = append_rule(rules(op))
    }
    ()
  }

  /**
   * IR Definitions
   **/
  case class Transform(name: String, isExtern: Boolean) extends Def[DSLTransformer]
  def forge_transformer(name: String, isExtern: Boolean) = {
    val xf: Exp[DSLTransformer] = Transform(name,isExtern)
    if (!Transformers.contains(xf)) {
      Transformers += xf -> TraversalRules.empty
      Traversals += xf
    }
    xf
  }

  case class Analyze(name: String, isExtern: Boolean) extends Def[DSLAnalyzer]
  def forge_analyzer(name: String, isExtern: Boolean) = {
    val az: Exp[DSLAnalyzer] = Analyze(name,isExtern)
    if (!Analyzers.contains(az)) {
      Analyzers += az -> AnalysisRules.empty
      Traversals += az
    }
    az
  }

  def forge_schedule(traversal: Rep[DSLTraversal]) = {
    TraversalSchedule += traversal
    ()
  }

  case class MetaDef(name: String) extends Def[DSLMetadata]
  def forge_metadata(name: String) = {
    val md: Exp[DSLMetadata] = MetaDef(name)
    if (!Metadatas.exists(_.name == md.name)) Metadatas += md
    md
  }
  def forge_lookup_metadata(name: String): Rep[DSLMetadata] = {
    val md = Metadatas.find(t => t.name == name)
    if (md.isEmpty)
      err("No metadata found with name " + name)
    md.get
  }

  case class MetaFields(fields: Seq[(String, Rep[DSLType])]) extends Def[DSLMetaFields]
  def forge_metadata_fields(meta: Rep[DSLMetadata], fields: Seq[(String, Rep[DSLType])]) = {
    val data = MetaFields(fields)
    if (MetaStructs.contains(meta)) err("Data fields already defined for metadata " + meta.name)
    else MetaStructs(meta) = data
    ()
  }

}
package ppl.dsl.forge
package core

import java.io.{PrintWriter}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.collection.mutable.{ArrayBuffer,HashMap}

trait ForgeTraversalOps extends Base {
  this: Forge =>

  //--------
  //--- API
  //--------

  /**
   * Methods for defining IR traversals and traversal rules
   * Note that Analyzers and Transformers are subclasses of Traversals.
   *
   * Ex:
   *   val MaxAnalysis = analyzer("Max")
   *   analyze (MaxAnalysis)(int_plus) using rule ${ [annotate max of lhs as function of maxes of inputs] }
   *
   * TODO: Support additional settings/methods for traversals
   * TODO: Modify preprocessor to support named arguments in rewrite rules
   **/

  def traversal(name: String, isExtern: Boolean = false): Rep[DSLTraversal] = forge_traversal(name, isExtern)
  def transformer(name: String, isExtern: Boolean = false): Rep[DSLTransformer] = forge_transformer(name,isExtern)
  def analyzer(name: String, isExtern: Boolean = false, isIterative: Boolean = false): Rep[DSLAnalyzer] = forge_analyzer(name,isExtern,isIterative)

  // Add given traversal to compiler's IR traversal schedule
  def schedule(traversal: Rep[DSLTraversal]): Rep[Unit] = forge_schedule(traversal)

  def rewrite(op: Rep[DSLOp]*): Rep[DSLPattern] = forge_rewrite(op)
  def rewrite(grp: Rep[DSLGroup], name: String): Rep[DSLPattern] = forge_rewrite(lookupOp(grp, name))

  def lower(xf: Rep[DSLTransformer])(op: Rep[DSLOp]): Rep[DSLPattern] = forge_lower(xf, op)
  def analyze(az: Rep[DSLAnalyzer])(op: Rep[DSLOp]): Rep[DSLPattern] = forge_analyze(az, op)

  // TODO: API for this?
  def propagate(op: Rep[DSLOp]): Rep[DSLPattern] = forge_propagate(op)
  def propagate(grp: Rep[DSLGroup], name: String): Rep[DSLPattern] = forge_propagate(lookupOp(grp, name))

  // --- Traversal rules
  // In analysis and traversal rules, the symbol representing the IR node can be referred to
  // using the variable name "lhs". Note that in rewrite rules there is no symbol yet.
  // Input arguments to the node are referred to using $0...$N
  //
  // Note: Forwarding typically should only be used as a rewrite rule
  object pattern {
    def apply(rule: (List[String], String)): DSLRule = forge_pattern(rule._1, unit(rule._2), false)
  }
  object commutative {
    def apply(rule: (List[String], String)): DSLRule = forge_pattern(rule._1, unit(rule._2), true)
  }
  object rule {
    def apply(rule: Rep[String]): DSLRule = forge_rule(rule)
  }
  object forwarding {
    def apply(rule: Rep[String]): DSLRule = forge_forwarding(rule)
  }

  /**
   * Methods for defining metadata types and methods
   * All metadata types must have the meet function defined on aliasing type 'any'
   * All other methods are optional
   * Creating metadata also creates a constructor of the same name and functions for
   * getting all fields of metadata instances by name
   *
   * Ex:
   *    val MaxVal = metadata("MaxVal", (max, SInt))  // Short form
   *    meet (MaxVal) ${ MaxVal(Math.max(this.max, that.max)) }
   *
   * TODO: Add sugar for other metadata methods
   * FIXME: Should users be allowed to specify type parameters for metadata?
   * FIXME: Should metadata even have type parameters?
   **/

  // Create a metadata type. Use data() to define fields
  def metadata(name: String): Rep[DSLMetadata] = forge_metadata(name, Nil)
  // Alternate shorter form with fields inlined
  def metadata(name: String, field1: (String, Rep[DSLType]), fields: (String, Rep[DSLType])*): Rep[DSLMetadata] = {
    val m = metadata(name)
    data(m, (field1 +: fields):_*)
    m
  }
  def lookupMeta(name: String): Rep[DSLMetadata] = forge_lookup_metadata(name)

  // Creates a meet rule for given metadata type for given aliasing type
  // Rule is always on exactly two arguments of given metadata type: this and that
  // Meet rule should return the same type of metadata
  def onMeet(meta: Rep[DSLMetadata], alias: MetaMeet = any)(rule: Rep[String]): Rep[Unit] = forge_meet(meta,alias,rule)
  def canMeet(meta: Rep[DSLMetadata], alias: MetaMeet = any)(rule: Rep[String]): Rep[Unit] = forge_canMeet(meta,alias,rule)
  def matches(meta: Rep[DSLMetadata])(rule: Rep[String]): Rep[Unit] = forge_matches(meta, rule)
  def isComplete(meta: Rep[DSLMetadata])(rule: Rep[String]): Rep[Unit] = forge_complete(meta, rule)

  def defaultMetadata(tpe: Rep[DSLType])(rule: Rep[String]): Rep[Unit] = forge_default_metadata(tpe, rule)

  // --- Others
  def disableFusion(): Unit
  def disableSoA(): Unit
  def disableStructUnwrapping(): Unit

  //----------
  //--- Stubs
  //----------
  def forge_traversal(name: String, isExtern: Boolean): Rep[DSLTraversal]
  def forge_transformer(name: String, isExtern: Boolean): Rep[DSLTransformer]
  def forge_analyzer(name: String, isExtern: Boolean,isIterative:Boolean): Rep[DSLAnalyzer]
  def forge_propagate(op: Rep[DSLOp]): Rep[DSLPattern]

  def forge_schedule(traversal: Rep[DSLTraversal]): Rep[Unit]
  def forge_metadata(name: String, tpePars: List[Rep[TypePar]]): Rep[DSLMetadata]
  def forge_lookup_metadata(name: String): Rep[DSLMetadata]

  def forge_rewrite(op: Seq[Rep[DSLOp]]): Rep[DSLPattern]
  def forge_lower(t: Rep[DSLTransformer], op: Rep[DSLOp]): Rep[DSLPattern]
  def forge_analyze(az: Rep[DSLAnalyzer], op: Rep[DSLOp]): Rep[DSLPattern]
  def forge_rule(rule: Rep[String]): DSLRule
  def forge_forwarding(rule: Rep[String]): DSLRule
  def forge_pattern(pattern: List[String], rule: Rep[String], commutative: Boolean): DSLRule

  def forge_using(pattern: Rep[DSLPattern], rule: DSLRule)(implicit ctx: SourceContext): Rep[Unit]

  def forge_meet(m: Rep[DSLMetadata], func: MetaMeet, rule: Rep[String]): Rep[Unit]
  def forge_canMeet(m: Rep[DSLMetadata], func: MetaMeet, rule: Rep[String]): Rep[Unit]
  def forge_matches(m: Rep[DSLMetadata], rule: Rep[String]): Rep[Unit]
  def forge_complete(m: Rep[DSLMetadata], rule: Rep[String]): Rep[Unit]
  def forge_default_metadata(tpe: Rep[DSLType], rule: Rep[String]): Rep[Unit]
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

  def withTransformer(xf: Rep[DSLTransformer]) = {
    if (_xFormScope.isDefined) forge_err("Cannot create nested transformer scopes!")
    _xFormScope = Some(xf)   // set transformer scope
    new TransformOps(xf)
  }
  def withAnalyzer(az: Rep[DSLAnalyzer]) = {
    if (_analysisScope.isDefined) forge_err("Cannot create nested analysis scopes!")
    _analysisScope = Some(az)
    new AnalysisOps(az)
  }

  class TransformOps(xf: Rep[DSLTransformer]) {
    def apply[R](block: => R) = new Scope[TransformScope, TransformScopeRunner[R], R](block)
  }
  class AnalysisOps(az: Rep[DSLAnalyzer]) {
    def apply[R](block: => R) = new Scope[AnalysisScope, AnalysisScopeRunner[R], R](block)
  }

  // --- Transformer scope sugar
  // TODO: Add other transformer methods here
  trait TransformScope {
    def lower(op: Rep[DSLOp]) = forge_lower(_xFormScope.get, op)
    def lower(grp: Rep[DSLGroup], name: String) = {
      val op = lookupOp(grp, name)
      forge_lower(_xFormScope.get, op)
    }
  }
  trait TransformScopeRunner[R] extends TransformScope {
    def apply: R
    val result = apply
    _xFormScope = None
  }

  // --- Analysis scope sugar
  trait AnalysisScope {
    def analyze(op: Rep[DSLOp]) = forge_analyze(_analysisScope.get, op)
    def analyze(grp: Rep[DSLGroup], name: String) = {
      val op = lookupOp(grp, name)
      forge_analyze(_analysisScope.get, op)
    }
  }
  trait AnalysisScopeRunner[R] extends AnalysisScope {
    def apply: R
    val result = apply
    _analysisScope = None
  }
}

trait ForgeTraversalOpsExp extends ForgeTraversalSugar with BaseExp {
  this: ForgeExp =>

  // Override these to change Config settings across all DSL applications
  var enableFusion = true
  var enableSoA = true
  var enableStructUnwrapping = true
  def disableFusion() { enableFusion = false }
  def disableSoA() { enableSoA = false }
  def disableStructUnwrapping() { enableStructUnwrapping = false }

  /**
   * Compiler state
   */
  val Traversals = ArrayBuffer[Exp[DSLTraversal]]()
  val Analyzers = HashMap[Exp[DSLAnalyzer], TraversalRules]()
  val Transformers = HashMap[Exp[DSLTransformer], TraversalRules]()
  val TraversalSchedule = ArrayBuffer[Exp[DSLTraversal]]()
  val MetaImpls = HashMap[Exp[DSLMetadata], MetaOps]()

  val PropagationRules = HashMap[Exp[DSLOp], List[DSLRule]]()
  val Rewrites = HashMap[Exp[DSLOp], List[DSLRule]]()
  val TypeMetadata = HashMap[Exp[DSLType], List[Rep[String]]]()

  /**
   * Traversal op patterns
   */
  case class RewritePattern(op: Rep[DSLOp]) extends Def[DSLPattern]
  case class RewriteSetPattern(op: List[Rep[DSLOp]]) extends Def[DSLPattern]
  case class LowerPattern(xf: Rep[DSLTransformer], op: Rep[DSLOp]) extends Def[DSLPattern]
  case class AnalysisPattern(az: Rep[DSLAnalyzer], op: Rep[DSLOp]) extends Def[DSLPattern]
  case class PropagationPattern(op: Rep[DSLOp]) extends Def[DSLPattern]

  def forge_rewrite(ops: Seq[Rep[DSLOp]]): Rep[DSLPattern] = {
    if (ops.length == 1)
      RewritePattern(ops.head)
    else
      RewriteSetPattern(ops.toList)
  }
  def forge_lower(xf: Rep[DSLTransformer], op: Rep[DSLOp]): Rep[DSLPattern] = LowerPattern(xf, op)
  def forge_analyze(az: Rep[DSLAnalyzer], op: Rep[DSLOp]): Rep[DSLPattern] = AnalysisPattern(az, op)
  def forge_propagate(op: Rep[DSLOp]): Rep[DSLPattern] = PropagationPattern(op)

  /**
   * Lowering/Rewrite (transformation) rules
   */
  case class SimpleRule(rule: Rep[String]) extends DSLRule
  case class PatternRule(pattern: List[String], rule: Rep[String], commutative: Boolean) extends DSLRule
  case class ForwardingRule(rule: Rep[String]) extends DSLRule

  def forge_rule(rule: Rep[String]) = SimpleRule(rule)
  def forge_forwarding(rule: Rep[String]) = ForwardingRule(rule)
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
    // TODO: Better stringify method for patterns for warnings/errors
    def append_rule_to(oldRules: List[DSLRule]) = {
      if (oldRules.exists(_.isInstanceOf[ForwardingRule])) {
        warn("Cannot define rewrite rule on pattern " + pattern + " - pattern already has a forwarding rule")
        oldRules
      }
      else rule match {
        case rule: ForwardingRule =>
          warn("Pattern " + pattern + " has a forwarding rule and other rewrite rules defined on it")
          List(rule)
        case rule: PatternRule => oldRules :+ rule
        case rule: SimpleRule =>
          if (oldRules.exists(_.isInstanceOf[SimpleRule])) err("Pattern " + pattern + " already has a simple rule")
          else oldRules :+ rule
      }
    }

    pattern match {
      case Def(RewriteSetPattern(ops)) => ops.foreach(op => forge_using(forge_rewrite(op), rule))
      case Def(RewritePattern(op)) =>
        if (!Rewrites.contains(op)) Rewrites(op) = List(rule) // Create
        else Rewrites(op) = append_rule_to(Rewrites(op))      // Append

      case Def(LowerPattern(xf, op)) =>
        val rules = Transformers(xf).rules
        if (!rules.contains(op)) rules(op) = List(rule)
        else rules(op) = append_rule_to(rules(op))

      case Def(AnalysisPattern(az,op)) =>
        val rules = Analyzers(az).rules
        if (!rules.contains(op)) rules(op) = List(rule)
        else rules(op) = append_rule_to(rules(op))

      case Def(PropagationPattern(op)) =>
        if (!PropagationRules.contains(op)) PropagationRules(op) = List(rule) // Create
        else PropagationRules(op) = append_rule_to(PropagationRules(op))      // Append
    }
    ()
  }

  /**
   * IR Definitions
   **/
  case class Traverse(name: String, isExtern: Boolean) extends Def[DSLTraversal]
  def forge_traversal(name: String, isExtern: Boolean) = {
    val t: Exp[DSLTraversal] = Traverse(name,isExtern)
    if (!Traversals.contains(t))
      Traversals += t
    t
  }

  case class Transform(name: String, isExtern: Boolean) extends Def[DSLTransformer]
  def forge_transformer(name: String, isExtern: Boolean) = {
    val xf: Exp[DSLTransformer] = Transform(name,isExtern)
    if (!Transformers.contains(xf)) {
      Transformers += xf -> TraversalRules.empty
      Traversals += xf
    }
    xf
  }

  case class Analyze(name: String, isExtern: Boolean, isIterative: Boolean) extends Def[DSLAnalyzer]
  def forge_analyzer(name: String, isExtern: Boolean, isIterative: Boolean) = {
    val az: Exp[DSLAnalyzer] = Analyze(name,isExtern,isIterative)
    if (!Analyzers.contains(az)) {
      Analyzers += az -> TraversalRules.empty
      Traversals += az
    }
    az
  }

  def forge_schedule(traversal: Rep[DSLTraversal]) = {
    TraversalSchedule += traversal
    ()
  }

  case class Meta(name: String, tpePars: List[Rep[TypePar]]) extends Def[DSLMetadata]
  def forge_metadata(name: String, tpePars: List[Rep[TypePar]]) = {
    val md: Exp[DSLMetadata] = Meta(name,tpePars)
    if (!Tpes.exists(_.name == md.name)) Tpes += md
    md
  }
  // TODO: Needed?
  def forge_lookup_metadata(name: String): Rep[DSLMetadata] = {
    val md = Tpes.find(t => t.name == name && isMetaType(t))
    if (md.isEmpty)
      err("No metadata type found with name " + name)
    md.get.asInstanceOf[Rep[DSLMetadata]]
  }

  case class MetaOps(
    val meet: HashMap[MetaMeet,Rep[String]],
    val canMeet: HashMap[MetaMeet,Rep[String]],
    var matches: Option[Rep[String]],
    var complete: Option[Rep[String]]
  )
  object MetaOps {
    def empty = MetaOps(HashMap[MetaMeet,Rep[String]](), HashMap[MetaMeet,Rep[String]](), None, None)
  }

  def forge_meet(m: Rep[DSLMetadata], func: MetaMeet, rule: Rep[String]): Rep[Unit] = {
    if (!isMetaType(m))
      err("Meet operations can only be defined on metadata types")

    if (!MetaImpls.contains(m))
      MetaImpls(m) = MetaOps.empty

    if (MetaImpls(m).meet.contains(func))
      warn("Overwriting meet rule on metadata type " + m.name + " for meet function " + func)

    MetaImpls(m).meet += func -> rule
    ()
  }
  def forge_canMeet(m: Rep[DSLMetadata], func: MetaMeet, rule: Rep[String]): Rep[Unit] = {
    if (!isMetaType(m))
      err("canMeet rules can only be defined on metadata types")

    if (!MetaImpls.contains(m))
      MetaImpls(m) = MetaOps.empty

    if (MetaImpls(m).canMeet.contains(func))
      warn("Overwriting canMeet rule on metadata type " + m.name + " for meet function " + func)

    MetaImpls(m).canMeet += func -> rule
    ()
  }
  def forge_matches(m: Rep[DSLMetadata], rule: Rep[String]): Rep[Unit] = {
    if (!isMetaType(m))
      err("matches rules can only be defined on metadata types")

    if (!MetaImpls.contains(m))
      MetaImpls(m) = MetaOps.empty

    if (MetaImpls(m).matches.isDefined)
      warn("Overwriting matches rule on metadata type " + m.name)

    MetaImpls(m).matches = Some(rule)
    ()
  }
  def forge_complete(m: Rep[DSLMetadata], rule: Rep[String]): Rep[Unit] = {
    if (!isMetaType(m))
      err("isComplete rules can only be defined on metadata types")

    if (!MetaImpls.contains(m))
      MetaImpls(m) = MetaOps.empty

    if (MetaImpls(m).complete.isDefined)
      warn("Overwriting isComplete rule on metadata type " + m.name)

    MetaImpls(m).complete = Some(rule)
    ()
  }


  def forge_default_metadata(tpe: Rep[DSLType], rule: Rep[String]): Rep[Unit] = {
    if (TypeMetadata.contains(tpe))
      TypeMetadata(tpe) = TypeMetadata(tpe) :+ rule
    else
      TypeMetadata(tpe) = List(rule)
    ()
  }

}

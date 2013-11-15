package ppl.dsl.forge
package core

import java.io.{PrintWriter}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.collection.mutable.{ArrayBuffer,HashMap}

trait ForgeOps extends Base {
  this: Forge =>

  def infix_withBound(a: Rep[TypePar], b: TypeClassSignature) = forge_withbound(a,b)
  implicit class TpeClassOps(a: TypeClassSignature) {
    def apply(b: Rep[TypePar]*) = forge_typeclasson(a,b.toList)
  }

  def grp(name: String) = forge_grp(name)
  def tpeAlias(name: String, tpe: Rep[DSLType]) = forge_tpealias(name, tpe)
  def tpePar(name: String, ctxBounds: List[TypeClassSignature] = List(TManifest), stage: StageTag = future) = forge_tpepar(name, ctxBounds, stage) // TODO: type bounds
  def ephemeralTpe(name: String, tpePars: List[Rep[TypePar]] = List(), stage: StageTag = future) = forge_ephemeral_tpe(name, tpePars, stage)
  def tpe(name: String, tpePars: List[Rep[TypePar]] = List(), stage: StageTag = future) = forge_tpe(name, tpePars, stage)
  def tpeInst(hkTpe: Rep[DSLType], tpeArgs: List[Rep[DSLType]] = List()) = forge_tpeinst(hkTpe, tpeArgs)
  def tpeClass(name: String, signature: TypeClassSignature, tpePars: List[Rep[TypePar]] = List()) = forge_tpeclass(name, signature, tpePars)
  def tpeClassInst(name: String, tpePars: List[Rep[TypePar]], tpeClass: Rep[DSLType]) = forge_tpeclassinst(name, tpePars, tpeClass)
  def ftpe(args: List[Rep[DSLArg]], ret: Rep[DSLType], freq: Frequency) = forge_ftpe(args, ret, freq)
  def identifier(tpe: Rep[DSLType])(name: String) = forge_identifier(name,tpe)
  def arg(name: String, tpe: Rep[DSLType], default: Option[String] = None) = forge_arg(name, tpe, default)
  def lift(grp: Rep[DSLGroup])(tpe: Rep[DSLType]) = forge_lift(grp, tpe)
  def data(tpe: Rep[DSLType], fields: (String, Rep[DSLType])*) = forge_data(tpe, fields)

  implicit def namedTpeToArg(arg: (String, Rep[DSLType])): Rep[DSLArg] = forge_arg(arg._1, arg._2, None)
  implicit def namedTpeWithDefaultToArg(arg: (String, Rep[DSLType], String)): Rep[DSLArg] = forge_arg(arg._1, arg._2, Some(arg._3))
  def anyToArg(a: (Any, Int)): Rep[DSLArg] = forge_anyToArg(a)
  def listToArgs(args: List[Rep[Any]]) = args.zipWithIndex.map(anyToArg).asInstanceOf[List[Rep[DSLArg]]]
  def anyToImplicitArg(a: (Any, Int)): Rep[DSLArg] = forge_anyToImplicitArg(a)
  def listToImplicitArgs(args: List[Rep[Any]]) = args.zipWithIndex.map(anyToImplicitArg).asInstanceOf[List[Rep[DSLArg]]]

  def listToCurriedArgs(args: List[List[Rep[Any]]]) = {
    // if (args.drop(1).exists(_.exists { case Def(Arg(_,_,Some(x))) => true; case _ => false })) forge_err("curried arguments cannot have default values")
    val indices = args.scanLeft(0)((a,b) => a+b.length)
    args.zip(indices).drop(1).map(t => t._1.zipWithIndex.map(z => (z._1, z._2+t._2)).map(anyToArg).asInstanceOf[List[Rep[DSLArg]]])
  }

  case class MethodSignature(args: List[Rep[Any]], retTpe: Rep[DSLType]) extends MethodSignatureType
  case class CurriedMethodSignature(args: List[List[Rep[Any]]], retTpe: Rep[DSLType]) extends MethodSignatureType

  def infix_args(signature: MethodSignatureType) = signature match {
    case MethodSignature(args, _) => args
    case CurriedMethodSignature(args, _) => args(0)
  }

  def infix_allArgs(signature: MethodSignatureType) = signature match {
    case MethodSignature(_, _) => Nil.asInstanceOf[List[List[Rep[DSLArg]]]]
    case CurriedMethodSignature(args, _) => args // includes actual args because we need to know the length to get the arg number right in listToCurriedArgs
  }

  def infix_retTpe(signature: MethodSignatureType) = signature match {
    case MethodSignature(_, r) => r
    case CurriedMethodSignature(_, r) => r
  }

  def static(grp: Rep[DSLGroup])(name: String, tpePars: List[Rep[TypePar]], signature: MethodSignatureType, implicitArgs: List[Rep[Any]] = List(), effect: EffectType = pure, aliasHint: AliasHint = nohint) =
      forge_op(grp,name,staticMethod,tpePars,listToArgs(signature.args),listToCurriedArgs(signature.allArgs),listToImplicitArgs(implicitArgs),signature.retTpe,effect,aliasHint)
  def infix(grp: Rep[DSLGroup])(name: String, tpePars: List[Rep[TypePar]], signature: MethodSignatureType, implicitArgs: List[Rep[Any]] = List(), effect: EffectType = pure, aliasHint: AliasHint = nohint) =
      forge_op(grp,name,infixMethod,tpePars,listToArgs(signature.args),listToCurriedArgs(signature.allArgs),listToImplicitArgs(implicitArgs),signature.retTpe,effect,aliasHint)
  def direct(grp: Rep[DSLGroup])(name: String, tpePars: List[Rep[TypePar]], signature: MethodSignatureType, implicitArgs: List[Rep[Any]] = List(), effect: EffectType = pure, aliasHint: AliasHint = nohint) =
      forge_op(grp,name,directMethod,tpePars,listToArgs(signature.args),listToCurriedArgs(signature.allArgs),listToImplicitArgs(implicitArgs),signature.retTpe,effect,aliasHint)
  def compiler(grp: Rep[DSLGroup])(name: String, tpePars: List[Rep[TypePar]], signature: MethodSignature, implicitArgs: List[Rep[Any]] = List(), effect: EffectType = pure, aliasHint: AliasHint = nohint) =
      forge_op(grp,name,compilerMethod,tpePars,listToArgs(signature.args),Nil,listToImplicitArgs(implicitArgs),signature.retTpe,effect,aliasHint)
  def fimplicit(grp: Rep[DSLGroup])(name: String, tpePars: List[Rep[TypePar]], signature: MethodSignature, implicitArgs: List[Rep[Any]] = List(), effect: EffectType = pure, aliasHint: AliasHint = nohint) =
      forge_op(grp,name,implicitMethod,tpePars,listToArgs(signature.args),Nil,listToImplicitArgs(implicitArgs),signature.retTpe,effect,aliasHint)

  def impl(op: Rep[DSLOp])(rule: OpType) = forge_impl(op,rule)
  def extern(grp: Rep[DSLGroup], withLift: Boolean = false, targets: List[CodeGenerator] = generators) = forge_extern(grp, withLift, targets)

  case class ParallelizeKey(tpe: Rep[DSLType])
  object parallelize {
    def apply(tpe: Rep[DSLType]) = ParallelizeKey(tpe)
  }
  def infix_as(p: ParallelizeKey, dc: ParallelCollection) = forge_isparallelcollection(p.tpe, dc)
  def infix_as(p: ParallelizeKey, dc: ParallelCollectionBuffer) = forge_isparallelcollection_buffer(p.tpe, dc)

  def lookupTpe(tpeName: String, stage: StageTag = future) = forge_lookup_tpe(tpeName,stage)
  def lookupGrp(grpName: String) = forge_lookup_grp(grpName)
  def lookupOp(grp: Rep[DSLGroup], opName: String) = forge_lookup_op(grp,opName,0)
  def lookupOp(grpName: String, opName: String) = forge_lookup_op(lookupGrp(grpName),opName,0)
  def lookupOverloaded(grpName: String, opName: String, index: Int) = forge_lookup_op(lookupGrp(grpName),opName,index)

  def label(op: Rep[DSLOp], name: String) = forge_label(op,name)

  def forge_grp(name: String): Rep[DSLGroup]
  def forge_tpealias(name: String, tpe: Rep[DSLType]): Rep[TypeAlias]
  def forge_tpepar(name: String, ctxBounds: List[TypeClassSignature], stage: StageTag): Rep[TypePar]
  def forge_withbound(a: Rep[TypePar], b: TypeClassSignature): Rep[TypePar]
  def forge_typeclasson(a: TypeClassSignature, b: List[Rep[TypePar]]): Rep[DSLType]
  def forge_tpe(name: String, tpePars: List[Rep[TypePar]], stage: StageTag): Rep[DSLType]
  def forge_ephemeral_tpe(name: String, tpePars: List[Rep[TypePar]], stage: StageTag): Rep[DSLType]
  def forge_tpeinst(hkTpe: Rep[DSLType], tpeArgs: List[Rep[DSLType]]): Rep[DSLType]
  def forge_tpeclass(name: String, signature: TypeClassSignature, tpePars: List[Rep[TypePar]]): Rep[DSLTypeClass]
  def forge_tpeclassinst(name: String, tpePars: List[Rep[TypePar]], tpe: Rep[DSLType]): Rep[DSLTypeClassInst]
  def forge_arg(name: String, tpe: Rep[DSLType], default: Option[String]): Rep[DSLArg]
  def forge_anyToArg(a: (Any, Int)): Rep[DSLArg]
  def forge_anyToImplicitArg(a: (Any, Int)): Rep[DSLArg]
  def forge_ftpe(args: List[Rep[DSLArg]], ret: Rep[DSLType], freq: Frequency): Rep[DSLType]
  def forge_identifier(name: String, tpe: Rep[DSLType]): Rep[DSLIdentifier]
  def forge_lift(grp: Rep[DSLGroup], tpe: Rep[DSLType]): Rep[Unit]
  def forge_data(tpe: Rep[DSLType], fields: Seq[(String, Rep[DSLType])]): Rep[DSLData]
  def forge_op(tpe: Rep[DSLGroup], name: String, style: MethodType, tpePars: List[Rep[TypePar]], args: List[Rep[DSLArg]], curriedArgs: List[List[Rep[DSLArg]]], implicitArgs: List[Rep[DSLArg]], retTpe: Rep[DSLType], effect: EffectType, aliasHint: AliasHint): Rep[DSLOp]
  def forge_impl(op: Rep[DSLOp], rule: OpType): Rep[DSLOp]
  def forge_extern(grp: Rep[DSLGroup], withLift: Boolean, targets: List[CodeGenerator]): Rep[Unit]
  def forge_isparallelcollection(tpe: Rep[DSLType], dc: ParallelCollection): Rep[Unit]
  def forge_isparallelcollection_buffer(tpe: Rep[DSLType], dc: ParallelCollectionBuffer): Rep[Unit]
  def forge_lookup_tpe(tpeName: String, stage: StageTag): Rep[DSLType]
  def forge_lookup_grp(grpName: String): Rep[DSLGroup]
  def forge_lookup_op(grp: Rep[DSLGroup], opName: String, overloadedIndex: Int): Rep[DSLOp]
  def forge_label(op: Rep[DSLOp], name: String): Rep[Unit]
}

trait ForgeSugarLowPriority extends ForgeOps {
  this: Forge =>

  /**
   * These allow users to specify argument lists as tuples instead of lists (for a small enough number of arguments)
   */
  implicit def singleToList[T](t: Rep[T]): List[Rep[T]] = List(t)
  implicit def tuple2ToList[T](t: (Rep[T],Rep[T])): List[Rep[T]] = List(t._1,t._2)
  implicit def tuple3ToList[T](t: (Rep[T],Rep[T],Rep[T])): List[Rep[T]] = List(t._1,t._2,t._3)
  implicit def tuple4ToList[T](t: (Rep[T],Rep[T],Rep[T],Rep[T])): List[Rep[T]] = List(t._1,t._2,t._3,t._4)
  implicit def tuple5ToList[T](t: (Rep[T],Rep[T],Rep[T],Rep[T],Rep[T])): List[Rep[T]] = List(t._1,t._2,t._3,t._4,t._5)

  implicit def namedArgToList(t: (String,Rep[DSLType])): List[Rep[DSLArg]] = List(namedTpeToArg(t))
  implicit def namedArg2ToList(t: ((String,Rep[DSLType]),(String,Rep[DSLType]))): List[Rep[DSLArg]] = List(namedTpeToArg(t._1),namedTpeToArg(t._2))
  implicit def namedArg3ToList(t: ((String,Rep[DSLType]),(String,Rep[DSLType]),(String,Rep[DSLType]))): List[Rep[DSLArg]] = List(namedTpeToArg(t._1),namedTpeToArg(t._2),namedTpeToArg(t._3))
  implicit def namedArg4ToList(t: ((String,Rep[DSLType]),(String,Rep[DSLType]),(String,Rep[DSLType]),(String,Rep[DSLType]))): List[Rep[DSLArg]] = List(namedTpeToArg(t._1),namedTpeToArg(t._2),namedTpeToArg(t._3),namedTpeToArg(t._4))
  implicit def namedArg5ToList(t: ((String,Rep[DSLType]),(String,Rep[DSLType]),(String,Rep[DSLType]),(String,Rep[DSLType]),(String,Rep[DSLType]))): List[Rep[DSLArg]] = List(namedTpeToArg(t._1),namedTpeToArg(t._2),namedTpeToArg(t._3),namedTpeToArg(t._4),namedTpeToArg(t._5))

  implicit def defaultArgToList(t: (String,Rep[DSLType],String)): List[Rep[DSLArg]] = List(namedTpeWithDefaultToArg(t))
  implicit def defaultArg2ToList(t: ((String,Rep[DSLType],String),(String,Rep[DSLType],String))): List[Rep[DSLArg]] = List(namedTpeWithDefaultToArg(t._1),namedTpeWithDefaultToArg(t._2))
  implicit def defaultArg3ToList(t: ((String,Rep[DSLType],String),(String,Rep[DSLType],String),(String,Rep[DSLType],String))): List[Rep[DSLArg]] = List(namedTpeWithDefaultToArg(t._1),namedTpeWithDefaultToArg(t._2),namedTpeWithDefaultToArg(t._3))
  implicit def defaultArg4ToList(t: ((String,Rep[DSLType],String),(String,Rep[DSLType],String),(String,Rep[DSLType],String),(String,Rep[DSLType],String))): List[Rep[DSLArg]] = List(namedTpeWithDefaultToArg(t._1),namedTpeWithDefaultToArg(t._2),namedTpeWithDefaultToArg(t._3),namedTpeWithDefaultToArg(t._4))
  implicit def defaultArg5ToList(t: ((String,Rep[DSLType],String),(String,Rep[DSLType],String),(String,Rep[DSLType],String),(String,Rep[DSLType],String),(String,Rep[DSLType],String))): List[Rep[DSLArg]] = List(namedTpeWithDefaultToArg(t._1),namedTpeWithDefaultToArg(t._2),namedTpeWithDefaultToArg(t._3),namedTpeWithDefaultToArg(t._4),namedTpeWithDefaultToArg(t._5))
}

trait ForgeSugar extends ForgeSugarLowPriority {
  this: Forge =>

  /**
   * Sugar available everywhere inside Forge
   */

  def infix_::(retTpe: Rep[DSLType], args: List[Rep[Any]]) = MethodSignature(args, retTpe)
  def infix_implements(o: Rep[DSLOp], rule: OpType) = forge_impl(o,rule)
  def infix_==>(args: List[Rep[Any]], ret: Rep[DSLType]) = MFunction(args,ret)

  implicit class TpeOps(hkTpe: Rep[DSLType]) {
    def apply(tpeArgs: Rep[DSLType]*) = tpeInst(hkTpe, tpeArgs.toList)
  }

  /**
   * Uses Scala-Virtualized scopes to enable sugar for ops scoped on a particular DSLType
   */
  var _tpeScopeBox: Rep[DSLType] = null
  def withTpe(tpe: Rep[DSLType]) = {
    _tpeScopeBox = tpe
    new ChainTpe(tpe)
  }
  class ChainTpe(tpe: Rep[DSLType]) {
    def apply[R](block: => R) = new Scope[TpeScope, TpeScopeRunner[R], R](block)
  }

  trait TpeScope {
    // it appears that shadowing the Forge op with the simpler version makes the general version inaccessible unless we explicitly define it

    def data(fields: (String, Rep[DSLType])*)
      = forge_data(_tpeScopeBox, fields)

    private def tpeOpArgs(addTpePars: List[Rep[TypePar]], args: List[Rep[Any]], allArgs: List[List[Rep[Any]]]) = {
      val t = _tpeScopeBox
      val amendedTpePars = (t.tpePars ::: addTpePars).distinct
      val amendedArgs = namedTpeToArg("self",t) :: args.zipWithIndex.map(t => anyToArg(t._1,t._2+1)).asInstanceOf[List[Rep[DSLArg]]] // arg numbering starts at 1
      val amendedCurriedArgs = listToCurriedArgs(amendedArgs :: allArgs.drop(1))
      (amendedTpePars, amendedArgs, amendedCurriedArgs)
    }

    // sugared version of op declarations
    // automatically imports the enclosing type as a type parameter and an instance of that type as the first argument, exported under 'self'
    def static(name: String)(signature: MethodSignatureType, implicitArgs: List[Rep[Any]] = List(), addTpePars: List[Rep[TypePar]] = List(), effect: EffectType = pure, aliasHint: AliasHint = nohint) = {
      forge_warn("declaring static method inside tpe scope - argument of type " + _tpeScopeBox.name + " is added as first parameter. is this what you meant?")
      val (amendedTpePars, amendedArgs, amendedCurriedArgs) = tpeOpArgs(addTpePars, signature.args, signature.allArgs)
      forge_op(_tpeScopeBox,name,staticMethod,amendedTpePars,amendedArgs,amendedCurriedArgs,listToImplicitArgs(implicitArgs),signature.retTpe,effect,aliasHint)
    }
    def infix(name: String)(signature: MethodSignatureType, implicitArgs: List[Rep[Any]] = List(), addTpePars: List[Rep[TypePar]] = List(), effect: EffectType = pure, aliasHint: AliasHint = nohint) = {
      val (amendedTpePars, amendedArgs, amendedCurriedArgs) = tpeOpArgs(addTpePars, signature.args, signature.allArgs)
      forge_op(_tpeScopeBox,name,infixMethod,amendedTpePars,amendedArgs,amendedCurriedArgs,listToImplicitArgs(implicitArgs),signature.retTpe,effect,aliasHint)
    }
    def direct(name: String)(signature: MethodSignatureType, implicitArgs: List[Rep[Any]] = List(), addTpePars: List[Rep[TypePar]] = List(), effect: EffectType = pure, aliasHint: AliasHint = nohint) = {
      val (amendedTpePars, amendedArgs, amendedCurriedArgs) = tpeOpArgs(addTpePars, signature.args, signature.allArgs)
      forge_op(_tpeScopeBox,name,directMethod,amendedTpePars,amendedArgs,amendedCurriedArgs,listToImplicitArgs(implicitArgs),signature.retTpe,effect,aliasHint)
    }
    def compiler(name: String)(signature: MethodSignature, implicitArgs: List[Rep[Any]] = List(), addTpePars: List[Rep[TypePar]] = List(), effect: EffectType = pure, aliasHint: AliasHint = nohint) = {
      val (amendedTpePars, amendedArgs, amendedCurriedArgs) = tpeOpArgs(addTpePars, signature.args, signature.allArgs)
      forge_op(_tpeScopeBox,name,compilerMethod,amendedTpePars,amendedArgs,Nil,listToImplicitArgs(implicitArgs),signature.retTpe,effect,aliasHint)
    }
    def fimplicit(name: String)(signature: MethodSignature, implicitArgs: List[Rep[Any]] = List(), addTpePars: List[Rep[TypePar]] = List(), effect: EffectType = pure, aliasHint: AliasHint = nohint) = {
      val (amendedTpePars, amendedArgs, amendedCurriedArgs) = tpeOpArgs(addTpePars, signature.args, signature.allArgs)
      forge_op(_tpeScopeBox,name,implicitMethod,amendedTpePars,amendedArgs,Nil,listToImplicitArgs(implicitArgs),signature.retTpe,effect,aliasHint)
    }

    val parallelize = ParallelizeKey(_tpeScopeBox)

    def lookupOp(opName: String) = forge_lookup_op(_tpeScopeBox,opName,0)
    def lookupOp(grp: Rep[DSLGroup], opName: String) = forge_lookup_op(grp,opName,0)
    def lookupOp(grpName: String, opName: String) = forge_lookup_op(lookupGrp(grpName),opName,0)
    def lookupOverloaded(opName: String, index: Int) = forge_lookup_op(_tpeScopeBox,opName,index)
    def lookupOverloaded(grpName: String, opName: String, index: Int) = forge_lookup_op(lookupGrp(grpName),opName,index)
  }

  trait TpeScopeRunner[R] extends TpeScope {
    def apply: R
    val result = apply
    _tpeScopeBox = null // reset
  }
}

trait ForgeOpsExp extends ForgeSugar with BaseExp {
  this: ForgeExp  =>

  /*
   * Compiler state
   */
  val Lifts = HashMap[Exp[DSLGroup],ArrayBuffer[Exp[DSLType]]]()
  val TpeAliases = ArrayBuffer[Exp[TypeAlias]]()
  val Tpes = ArrayBuffer[Exp[DSLType]]()
  val Identifiers = ArrayBuffer[Exp[DSLIdentifier]]()
  val DataStructs = HashMap[Exp[DSLType],Exp[DSLData]]()
  val OpsGrp = HashMap[Exp[DSLGroup],DSLOps]()
  val Impls = HashMap[Exp[DSLOp],OpType]()
  val ForgeCollections = HashMap[Exp[DSLType], ForgeCollectionType]()
  val Externs = ArrayBuffer[Extern]()
  val Labels = HashMap[Exp[DSLOp],String]()

  /**
   * Convenience method providing access to defined ops in other modules
   */

  def forge_lookup_tpe(tpeName: String, stage: StageTag): Rep[DSLType] = {
    val t = Tpes.find(t => t.name == tpeName && t.stage == stage)
    if (t.isEmpty) {
      err("lookup failed: no tpe exists with name " + tpeName + " and stage " + stage)
    }
    t.get
  }

  def forge_lookup_grp(grpName: String): Rep[DSLGroup] = {
    val t = Tpes.find(_.name == grpName).orElse(OpsGrp.find(t => t._1.name == grpName).map(_._1))
    if (t.isEmpty) {
      err("lookup failed: no grp exists with name " + grpName)
    }
    t.get
  }

  def forge_lookup_op(grp: Rep[DSLGroup], opName: String, overloadedIndex: Int): Rep[DSLOp] = {
    val matches = OpsGrp.get(grp).map(_.ops.filter(_.name == opName))
    if (matches.isEmpty || (matches.isDefined && overloadedIndex > matches.get.length-1)) {
      warn("lookup failed: no op exists in grp " + grp.name + " with name " + opName + " and index " + overloadedIndex)
      val opsGrp = OpsGrp.get(grp)
      if (opsGrp.isEmpty) {
        println("  grp " + grp.name + " has no ops defined")
      }
      else {
        println("  ops in grp " + grp.name + " with name " + opName + " are: ")
        for ((o,i) <- opsGrp.get.ops.filter(_.name == opName).zipWithIndex) {
          println("    " + o.name + o.args.map(_.tpe.name).mkString("(",",",")") + " (index " + i + ")")
        }
      }
      err("lookup failed")
    }

    matches.get(overloadedIndex)
  }

  /**
   * IR Definitions
   */

  /* A group represents a collection of ops which become an op trait in the generated DSL */
  case class Grp(name: String) extends Def[DSLGroup]

  def forge_grp(name: String) = Grp(name)

  /* TpeAlias is a DSL-time type alias for a DSLType */
  case class TpeAlias(name: String, tpe: Rep[DSLType]) extends Def[TypeAlias]

  def forge_tpealias(name: String, tpe: Rep[DSLType]) = {
    val ta = TpeAlias(name,tpe)
    if (!TpeAliases.contains(ta)) TpeAliases += ta
    ta
  }

  /* TpePar represents a named type parameter for another DSLType */
  // no higher-kinded fun yet
  case class TpePar(name: String, ctxBounds: List[TypeClassSignature], stage: StageTag) extends Def[TypePar]

  def forge_tpepar(name: String, ctxBounds: List[TypeClassSignature], stage: StageTag) = TpePar(name, ctxBounds, stage)

  /* Adds a bound to a type parameter by constructing a new type parameter */
  def forge_withbound(a: Rep[TypePar], b: TypeClassSignature) = tpePar(a.name, b :: a.ctxBounds)

  def forge_typeclasson(a: TypeClassSignature, b: List[Rep[TypePar]]) = tpeClass(a.name, a, b)

  /* A DSLType */
  case class Tpe(name: String, tpePars: List[Rep[TypePar]], stage: StageTag) extends Def[DSLType]

  def forge_tpe(name: String, tpePars: List[Rep[TypePar]], stage: StageTag) = {
    val t: Exp[DSLType] = Tpe(name, tpePars, stage)
    if (!Tpes.contains(t)) Tpes += t
    t
  }

  // creates a tpe, but doesn't add it to the IR state
  def forge_ephemeral_tpe(name: String, tpePars: List[Rep[TypePar]] = List(), stage: StageTag = future): Exp[DSLType] = {
    Tpe(name, tpePars, stage)
  }

  /* A DSLType instance of a higher-kinded type */
  case class TpeInst(hkTpe: Rep[DSLType], tpeArgs: List[Rep[DSLType]]) extends Def[DSLType]

  def forge_tpeinst(hkTpe: Rep[DSLType], tpeArgs: List[Rep[DSLType]]) = {
    if (tpeArgs.length != hkTpe.tpePars.length) err("cannot instantiate tpe " + hkTpe.name + " with args " + tpeArgs + " - not enough arguments")
    val t: Exp[DSLType] = TpeInst(hkTpe, tpeArgs)
    // if (!Tpes.contains(t)) Tpes += t
    t
  }

  /* A DSL-defined type class */
  case class TpeClass(name: String, signature: TypeClassSignature, tpePars: List[Rep[TypePar]]) extends Def[DSLTypeClass]

  def forge_tpeclass(name: String, signature: TypeClassSignature, tpePars: List[Rep[TypePar]]) = {
    TpeClass(name, signature, tpePars)
  }

  /* A DSL-defined type class instance */
  case class TpeClassInst(name: String, tpePars: List[Rep[TypePar]], tpe: Rep[DSLType]) extends Def[DSLTypeClassInst]

  def forge_tpeclassinst(name: String, tpePars: List[Rep[TypePar]], tpe: Rep[DSLType]) = {
    getHkTpe(tpe) match {
      case Def(TpeClass(_,_,_)) => TpeClassInst(name, tpePars, tpe)
      case _ => err("cannot instantiate tpe class instance from tpe " + name + " - not a tpe class")
    }
  }

  /* A function of Rep arguments */
  case class FTpe(args: List[Rep[DSLArg]], ret: Rep[DSLType], freq: Frequency) extends Def[DSLType]

  def forge_ftpe(args: List[Rep[DSLArg]], ret: Rep[DSLType], freq: Frequency) = FTpe(args,ret,freq)

  /* A singleton to represent an enumerated choice in the DSL. The enumeration is one possible value of 'tpe' */
  case class Identifier(name: String, tpe: Rep[DSLType]) extends Def[DSLIdentifier]
  def forge_identifier(name: String, tpe: Rep[DSLType]): Rep[DSLIdentifier] = {
    if (tpe.tpePars != Nil) err("identifiers cannot have type parameters")
    val id = Identifier(name,tpe)
    if (Identifiers.exists(_.name == id.name)) {
      err("identifier " + name + " already exists")
    }
    else {
      Identifiers += id
    }
    id
  }

  /* A statement declaring a type to lift in a particular scope (group) */
  def forge_lift(grp: Rep[DSLGroup], tpe: Rep[DSLType]) = {
    val buf = Lifts.getOrElseUpdate(grp, new ArrayBuffer[Exp[DSLType]]())
    if (!buf.contains(tpe)) buf += tpe
    ()
  }

  /* An argument to a DSL function */
  case class Arg(name: String, tpe: Rep[DSLType], default: Option[String]) extends Def[DSLArg]

  def forge_arg(name: String, tpe: Rep[DSLType], default: Option[String]) = Arg(name, tpe, default)
  def forge_anyToArg(a: (Any, Int)) : Rep[DSLArg] = a._1 match {
    case Def(Arg(_,_,_)) => a._1.asInstanceOf[Rep[DSLArg]]
    case _ => forge_arg(opArgPrefix+a._2, a._1.asInstanceOf[Rep[DSLType]], None)
  }
  def forge_anyToImplicitArg(a: (Any, Int)) : Rep[DSLArg] = a._1 match {
    case Def(Arg(_,_,_)) => a._1.asInstanceOf[Rep[DSLArg]]
    case _ => forge_arg(implicitOpArgPrefix+a._2, a._1.asInstanceOf[Rep[DSLType]], None)
  }

  /* A back-end data structure */
  case class Data(tpe: Rep[DSLType], fields: Seq[(String, Exp[DSLType])]) extends Def[DSLData]

  def forge_data(tpe: Rep[DSLType], fields: Seq[(String, Exp[DSLType])]) = {
    val d: Exp[DSLData] = Data(tpe, fields)
    if (DataStructs.contains(tpe)) err("multiple data structures declared for type " + tpe.name)
    else DataStructs(tpe) = d
    d
  }

  /* An operator - this represents a method or IR node */
  case class Op(grp: Rep[DSLGroup], name: String, style: MethodType, tpePars: List[Rep[TypePar]], args: List[Rep[DSLArg]], curriedArgs: List[List[Rep[DSLArg]]], implicitArgs: List[Rep[DSLArg]], retTpe: Rep[DSLType], effect: EffectType, aliasHint: AliasHint) extends Def[DSLOp]

  def forge_op(_grp: Rep[DSLGroup], name: String, style: MethodType, tpePars: List[Rep[TypePar]], args: List[Rep[DSLArg]], curriedArgs: List[List[Rep[DSLArg]]], implicitArgs: List[Rep[DSLArg]], retTpe: Rep[DSLType], effect: EffectType, aliasHint: AliasHint) = {
    args match {
      case a :: Def(Arg(_, Def(VarArgs(z)), _)) :: b if b != Nil => err("a var args op parameter must be the final one")
      case Def(Arg(_, Def(VarArgs(z)), _)) :: b if b != Nil => err("a var args op parameter must be the final one.")
      //case a :: Def(VarArgs(z)) :: b if b != Nil => err("a var args op parameter must be the final one")
      //case Def(VarArgs(z)) :: b if b != Nil => err("a var args op parameter must be the final one.")
      case _ => // ok
    }

    if (retTpe == MUnit && effect == pure && !args.exists(isFuncArg) && !curriedArgs.exists(_.exists(isFuncArg))) {
      warn("op " + name + " has return type " + MUnit.name + " but no effects, so it is a no-op")
    }

    // always add source context, unless it's an overridden method or blacklisted
    val amendedImplicitArgs = if (overrideList.contains(name) || noSourceContextList.contains(name)) implicitArgs else (arg("__pos",MSourceContext) :: implicitArgs).distinct

    val opsGrp = OpsGrp.getOrElseUpdate(_grp, new DSLOps {
      val grp = _grp
      override def targets: List[CodeGenerator] = Nil
    })
    val o = toAtom(Op(_grp, name, style, tpePars, args, curriedArgs, amendedImplicitArgs, retTpe, effect, aliasHint))
    opsGrp.ops :+=  o // append is required for lookups in declaration order
    o
  }

  /**
   * Add an op implementation rule
   */
  def forge_impl(op: Rep[DSLOp], rule: OpType) = {
    (Impls.get(op),rule) match {
      // only codegen rules may have multiple declarations
      case (Some(current@CodeGen(d1)),add@CodeGen(d2)) =>
        for ((g,d) <- add.decls) {
          if (current.decls.contains(g) && Config.verbosity > 0)
            warn("op " + op.name + " already has codegen rule defined at " + quotePos(current.decls(g).decl) + " for generator " + g.name + ". it is being replaced with rule defined at " + quotePos(d.decl))
          current.decls(g) = d
        }
      case (Some(x),y) =>
        if (Config.verbosity > 0) warn("op " + op.name + " already has implementation rule " + x + ". It is being overwritten with " + rule)
        Impls(op) = rule
      case _ =>
        Impls(op) = rule
    }

    // no matter what branch we took, we may need to reconcile codegen rules
    if (rule.isInstanceOf[CodeGen])
      reconcileCodegenRule(op,rule.asInstanceOf[CodeGen])

    // return the op so that users can bind it if they want
    op
  }

  // vet codegen rules and update the ops list if necessary
  def reconcileCodegenRule(op: Rep[DSLOp], rule: CodeGen) {
    if (op.args.exists(a => DataStructs.contains(getHkTpe(a.tpe))))
      err("(op " + op.name + ") code generated ops should not have struct types as inputs, since structs may be eliminated at compile time. consider passing in one or more fields of the struct as input(s) to the op instead.")

    // also may need to update opsGrp targets
    val opsGrp = OpsGrp.getOrElse(op.grp, err("couldn't find group " + op.grp.name + " for code generator declared on op " + op.name))
    val newTargets = rule.decls.map{case (g,r) => g}.toList
    if (!newTargets.diff(opsGrp.targets).isEmpty) {
      val updatedGrp = new DSLOps {
        val grp = op.grp
        override def targets: List[CodeGenerator] = (newTargets union opsGrp.targets).distinct
      }
      updatedGrp.ops = opsGrp.ops
      OpsGrp(op.grp) = updatedGrp
    }
  }

  /* Establishes that the given tpe implements the ParallelCollection interface */
  def forge_isparallelcollection(tpe: Rep[DSLType], dc: ParallelCollection) = {
    checkDcFunctions(tpe, dc)
    if (ForgeCollections.contains(tpe)) err("type " + tpe.name + " is already a parallel collection")
    ForgeCollections += (tpe -> dc)
    ()
  }

  /* Establishes that the given tpe implements the ParallelCollectionBuffer interface */
  def forge_isparallelcollection_buffer(tpe: Rep[DSLType], dc: ParallelCollectionBuffer) = {
    checkDcFunctions(tpe, dc)
    if (ForgeCollections.contains(tpe)) err("type " + tpe.name + " is already a parallel collection")
    ForgeCollections += (tpe -> dc)
    ()
  }

  def checkDcFunctions(tpe: Rep[DSLType], dc: ForgeCollectionType) {
    // verify the dc functions match our expectations

    if (dc.alloc.args.size != 2 || dc.alloc.tpePars.size > 2 /*getHkTpe(dc.alloc.args.apply(0).tpe) != getHkTpe(tpe) */|| dc.alloc.args.apply(1).tpe != MInt
      || ((getHkTpe(dc.alloc.retTpe) != getHkTpe(tpe)) && (getHkTpe(dc.alloc.retTpe) != MNothing)))
      // err("dcAlloc must take two arguments (" + tpe.name + ", " + MInt.name + ") and return an instance of " + tpe.name)
      err("dcAlloc must take at most two type parameters, two arguments ([Col], " + MInt.name + ") and return an instance of " + tpe.name)
    if (dc.alloc.tpePars.length > 2)
      warn("dcAlloc with more than 2 type parameters has undefined semantics. Consider trying to rewrite your alloc method to be in the form alloc[A,R](in: C[A], size: Int): C[R]")
    if (dc.size.args.size != 1 || getHkTpe(dc.size.args.apply(0).tpe) != getHkTpe(tpe) || (dc.size.retTpe != MInt))
      err("dcSize must take a single argument of type " + tpe.name + " and return an MInt")
    if (dc.apply.args.size != 2 || ((getHkTpe(dc.apply.args.apply(0).tpe), dc.apply.args.apply(1).tpe) != (getHkTpe(tpe), MInt)) || (dc.apply.retTpe != dc.tpeArg))
      err("dcApply must take two arguments of type(" + tpe.name + ", " + MInt.name + ") and return a " + dc.tpeArg.name)
    if (dc.update.args.size != 3 || ((getHkTpe(dc.update.args.apply(0).tpe), dc.update.args.apply(1).tpe, dc.update.args.apply(2).tpe) != (getHkTpe(tpe), MInt, dc.tpeArg))
      || ((dc.update.retTpe != MUnit) && (dc.update.retTpe != MNothing)))
      err("dcUpdate must take arguments of type (" + tpe.name + ", " + MInt.name + ", " + dc.tpeArg.name + ") and return " + MUnit.name)
  }

  /* A reference to an external ops group */
  case class Extern(opsGrp: DSLOps, withLift: Boolean) extends Def[Unit]

  def forge_extern(_grp: Rep[DSLGroup], withLift: Boolean, _targets: List[CodeGenerator]) = {
    val opsGrp = new DSLOps {
      val grp = _grp
      override def targets = _targets
    }
    val e = Extern(opsGrp, withLift)
    if (!Externs.contains(e)) Externs += e
    e
  }

  /* Specifies a unique name to assign to the op's method/case class for overriding and pattern matching */
  def forge_label(op: Rep[DSLOp], name: String) = {
    if (Labels.contains(op)) warn("overriding existing label " + Labels(op) + " with new label " + name)
    Labels(op) = name
    ()
  }
}


trait ScalaGenForgeOps extends ScalaGenBase {
  val IR: ForgeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

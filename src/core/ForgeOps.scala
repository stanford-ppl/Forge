package ppl.dsl.forge
package core

import java.io.{PrintWriter}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.collection.mutable.{ArrayBuffer,HashMap}

trait ForgeOps extends Base {
  this: Forge =>
      
  def infix_withBound(a: Rep[TypePar], b: TypeClass) = forge_withbound(a,b)
  implicit class TpeClassOps(a: TypeClass) {
    def apply(b: Rep[TypePar]*) = forge_typeclasson(a,b.toList)
  }
  
  def grp(name: String) = forge_grp(name)  
  def tpeAlias(name: String, tpe: Rep[DSLType]) = forge_tpealias(name, tpe)
  def tpePar(name: String, ctxBounds: List[TypeClass] = List()) = forge_tpepar(name, ctxBounds) // TODO: type bounds    
  def tpe(name: String, tpePars: List[Rep[TypePar]] = List(), stage: StageTag = future) = forge_tpe(name, tpePars, stage)
  def tpeInst(hkTpe: Rep[DSLType], tpeArgs: List[Rep[DSLType]] = List(), stage: StageTag = future) = forge_tpeinst(hkTpe, tpeArgs, stage)    
  def ftpe(args: List[Rep[DSLArg]], ret: Rep[DSLType], freq: Frequency) = forge_ftpe(args, ret, freq)
  def arg(name: String, tpe: Rep[DSLType], default: Option[String] = None) = forge_arg(name, tpe, default)
  def lift(grp: Rep[DSLGroup])(tpe: Rep[DSLType]) = forge_lift(grp, tpe)
  def data(tpe: Rep[DSLType], fields: (String, Rep[DSLType])*) = forge_data(tpe, fields)
  
  implicit def namedTpeToArg(arg: (String, Rep[DSLType])): Rep[DSLArg] = forge_arg(arg._1, arg._2, None)
  implicit def namedTpeWithDefaultToArg(arg: (String, Rep[DSLType], String)): Rep[DSLArg] = forge_arg(arg._1, arg._2, Some(arg._3))
  def anyToArg(a: (Any, Int)): Rep[DSLArg] = forge_anyToArg(a)
  def op(grp: Rep[DSLGroup])(name: String, style: MethodType, tpePars: List[Rep[TypePar]], args: List[Rep[Any]], retTpe: Rep[DSLType], implicitArgs: List[Rep[DSLType]] = List(), effect: EffectType = pure, aliasHint: AliasHint = nohint) = 
    forge_op(grp,name,style,tpePars,args.zipWithIndex.map(anyToArg).asInstanceOf[List[Rep[DSLArg]]],implicitArgs,retTpe,effect,aliasHint)
  
  def impl(op: Rep[DSLOp])(rule: OpType) = forge_impl(op,rule)  
  def extern(grp: Rep[DSLGroup], withLift: Boolean = false, targets: List[CodeGenerator] = generators) = forge_extern(grp, withLift, targets)
      
  def infix_is(tpe: Rep[DSLType], dc: ParallelCollection) = forge_isparallelcollection(tpe, dc)  
  case class ParallelCollection(val tpeArg: Rep[DSLType], val alloc: Rep[DSLOp], val size: Rep[DSLOp], val apply: Rep[DSLOp], val update: Rep[DSLOp]) extends ForgeCollectionType
  def infix_is(tpe: Rep[DSLType], dc: ParallelCollectionBuffer) = forge_isparallelcollection_buffer(tpe,dc)
  case class ParallelCollectionBuffer(
    val tpeArg: Rep[DSLType], val alloc: Rep[DSLOp], val size: Rep[DSLOp], val apply: Rep[DSLOp], val update: Rep[DSLOp],
    /*val parallelization: Rep[DSLOp],*/ val setSize: Rep[DSLOp], val appendable: Rep[DSLOp], val append: Rep[DSLOp], val copy: Rep[DSLOp]
  ) extends ForgeCollectionType

  implicit def forceOpOption(o: Option[Rep[DSLOp]]): Rep[DSLOp]
  def lookup(grpName: String, opName: String): Option[Rep[DSLOp]] = forge_lookup(grpName,opName,0)
  def lookupOverloaded(grpName: String, opName: String, index: Int) = forge_lookup(grpName,opName,index)
  
  def forge_grp(name: String): Rep[DSLGroup]  
  def forge_tpealias(name: String, tpe: Rep[DSLType]): Rep[TypeAlias]
  def forge_tpepar(name: String, ctxBounds: List[TypeClass]): Rep[TypePar]
  def forge_withbound(a: Rep[TypePar], b: TypeClass): Rep[TypePar] 
  def forge_typeclasson(a: TypeClass, b: List[Rep[TypePar]]): Rep[DSLType] 
  def forge_tpe(name: String, tpePars: List[Rep[TypePar]], stage: StageTag): Rep[DSLType]    
  def forge_tpeinst(hkTpe: Rep[DSLType], tpeArgs: List[Rep[DSLType]], stage: StageTag): Rep[DSLType]    
  def forge_arg(name: String, tpe: Rep[DSLType], default: Option[String]): Rep[DSLArg]
  def forge_anyToArg(a: (Any, Int)): Rep[DSLArg]
  def forge_ftpe(args: List[Rep[DSLArg]], ret: Rep[DSLType], freq: Frequency): Rep[DSLType]
  def forge_lift(grp: Rep[DSLGroup], tpe: Rep[DSLType]): Rep[Unit]
  def forge_data(tpe: Rep[DSLType], fields: Seq[(String, Rep[DSLType])]): Rep[DSLData]  
  def forge_op(tpe: Rep[DSLGroup], name: String, style: MethodType, tpePars: List[Rep[TypePar]], args: List[Rep[DSLArg]], implicitArgs: List[Rep[DSLType]], retTpe: Rep[DSLType], effect: EffectType, aliasHint: AliasHint): Rep[DSLOp]
  def forge_impl(op: Rep[DSLOp], rule: OpType): Rep[Unit]
  def forge_extern(grp: Rep[DSLGroup], withLift: Boolean, targets: List[CodeGenerator]): Rep[Unit]
  def forge_isparallelcollection(tpe: Rep[DSLType], dc: ParallelCollection): Rep[Unit]
  def forge_isparallelcollection_buffer(tpe: Rep[DSLType], dc: ParallelCollectionBuffer): Rep[Unit]
  def forge_lookup(grpName: String, opName: String, overloadedIndex: Int): Option[Rep[DSLOp]]  
}

trait ForgeSugar extends ForgeOps with ForgeUtilities {
  this: Forge =>
  
  /**
   * Sugar available everywhere inside Forge
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
    def apply(block: => Unit) = new Scope[TpeScope, TpeScopeRunner, Unit](block)
  }  
  
  trait TpeScope {
    // it appears that shadowing the Forge op with the simpler version makes the general version inaccessible unless we explicitly define it
    
    def data(fields: (String, Rep[DSLType])*) 
      = forge_data(_tpeScopeBox, fields)
          
    // 'is' is a sugared version of 'op' designed to be declared inside a tpe scope 
    // it automatically imports the enclosing type as a type parameter and an instance of that type as the first argument, exported under 'self' 
    def infix_is(s: String, style: MethodType, args: List[Rep[Any]], retTpe: Rep[DSLType], implicitArgs: List[Rep[DSLType]] = List(), addTpePars: List[Rep[TypePar]] = List(), effect: EffectType = pure, aliasHint: AliasHint = nohint) = {
      val t = _tpeScopeBox
      
      if (style == static) {
        warn("op " + s + ": static methods should normally be not be declared with 'is'")
      }
      
      val amendedTpePars = (t.tpePars ::: addTpePars).distinct
      val amendedArgs = namedTpeToArg("self",tpeInst(t,t.tpePars)) :: args.zipWithIndex.map(t => anyToArg(t._1,t._2+1)).asInstanceOf[List[Rep[DSLArg]]] // arg numbering starts at 1
      
      forge_op(_tpeScopeBox,s,style,amendedTpePars,amendedArgs,implicitArgs,retTpe,effect,aliasHint)        
    }      
  
    abstract class ParallelizeKey
    object parallelize extends ParallelizeKey
    def infix_as(p: ParallelizeKey, dc: ParallelCollection) = forge_isparallelcollection(_tpeScopeBox, dc)  
    def infix_as(p: ParallelizeKey, dc: ParallelCollectionBuffer) = forge_isparallelcollection_buffer(_tpeScopeBox, dc)  
    
    def lookup(opName: String) = forge_lookup(_tpeScopeBox.name,opName,0)
    def lookup(grpName: String, opName: String) = forge_lookup(grpName,opName,0)    
    def lookupOverloaded(opName: String, index: Int) = forge_lookup(_tpeScopeBox.name,opName,index)
    def lookupOverloaded(grpName: String, opName: String, index: Int) = forge_lookup(grpName,opName,index)
  }
  
  trait TpeScopeRunner extends TpeScope {
    def apply: Any
    apply
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
  val DataStructs = HashMap[Exp[DSLType],Exp[DSLData]]()
  val OpsGrp = HashMap[Exp[DSLGroup],DSLOps]()  
  val Impls = HashMap[Exp[DSLOp],OpType]() 
  val ForgeCollections = HashMap[Exp[DSLType], ForgeCollectionType]()
  val Externs = ArrayBuffer[Extern]()
  
  /**
   * Convenience method providing access to defined ops in other modules
   */  
   
  // this unsafe implicit is provided only to simplify the common case 
  def forceOpOption(o: Option[Rep[DSLOp]]): Rep[DSLOp] = {
    if (o.isEmpty) (err("Empty option " + o + " used as in place of an op argument"))
    o.get
  }
  
  def forge_lookup(grpName: String, opName: String, overloadedIndex: Int): Option[Rep[DSLOp]] = {
    val matches = OpsGrp.find(t => t._1.name == grpName).map(_._2.ops.filter(_.name == opName))
    if (matches.isDefined) {
      if (overloadedIndex > matches.get.length-1) err("lookup failed: no op exists in grp " + grpName + " with name " + opName + " and index " + overloadedIndex)
      Some(matches.get(overloadedIndex))
    }
    else {
      None
    }
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
  case class TpePar(name: String, ctxBounds: List[TypeClass]) extends Def[TypePar]
   
  def forge_tpepar(name: String, ctxBounds: List[TypeClass]) = TpePar(name, ctxBounds)
  
  /* Adds a bound to a type parameter by constructing a new type parameter */
  def forge_withbound(a: Rep[TypePar], b: TypeClass) = tpePar(a.name, b :: a.ctxBounds)
  
  def forge_typeclasson(a: TypeClass, b: List[Rep[TypePar]]) = ephemeralTpe(a.name, b)

  /* A DSLType */    
  case class Tpe(name: String, tpePars: List[Rep[TypePar]], stage: StageTag) extends Def[DSLType]
  
  def forge_tpe(name: String, tpePars: List[Rep[TypePar]], stage: StageTag) = {
    val t: Exp[DSLType] = Tpe(name, tpePars, stage)
    if (!Tpes.contains(t)) Tpes += t
    t
  }
  // creates a tpe, but doesn't add it to the IR state (only available inside Forge for now)
  def ephemeralTpe(name: String, tpePars: List[Rep[TypePar]] = List(), stage: StageTag = future): Exp[DSLType] = {
    Tpe(name, tpePars, stage)
  }
  
  /* A DSLType instance of a higher-kinded type */
  case class TpeInst(hkTpe: Rep[DSLType], tpeArgs: List[Rep[DSLType]], stage: StageTag) extends Def[DSLType]
    
  def forge_tpeinst(hkTpe: Rep[DSLType], tpeArgs: List[Rep[DSLType]], stage: StageTag) = {
    if (tpeArgs.length != hkTpe.tpePars.length) err("cannot instantiate tpe " + hkTpe.name + " with args " + tpeArgs + " - not enough arguments")
    val t: Exp[DSLType] = TpeInst(hkTpe, tpeArgs, stage)
    if (!Tpes.contains(t)) Tpes += t
    t
  }
    
  /* A function of Rep arguments */
  case class FTpe(args: List[Rep[DSLArg]], ret: Rep[DSLType], freq: Frequency) extends Def[DSLType]
  
  def forge_ftpe(args: List[Rep[DSLArg]], ret: Rep[DSLType], freq: Frequency) = FTpe(args,ret,freq)
  
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

  /* A back-end data structure */
  case class Data(tpe: Rep[DSLType], fields: Seq[(String, Exp[DSLType])]) extends Def[DSLData]
  
  def forge_data(tpe: Rep[DSLType], fields: Seq[(String, Exp[DSLType])]) = {
    val d: Exp[DSLData] = Data(tpe, fields)
    if (DataStructs.contains(tpe)) err("multiple data structures declared for type " + tpe.name)        
    else DataStructs(tpe) = d
    d
  }
  
  /* An operator - this represents a method or IR node */
  case class Op(grp: Rep[DSLGroup], name: String, style: MethodType, tpePars: List[Rep[TypePar]], args: List[Rep[DSLArg]], implicitArgs: List[Rep[DSLType]], retTpe: Rep[DSLType], effect: EffectType, aliasHint: AliasHint) extends Def[DSLOp]
  
  def forge_op(_grp: Rep[DSLGroup], name: String, style: MethodType, tpePars: List[Rep[TypePar]], args: List[Rep[DSLArg]], implicitArgs: List[Rep[DSLType]], retTpe: Rep[DSLType], effect: EffectType, aliasHint: AliasHint) = {
    args match {
      case a :: Def(Arg(_, Def(VarArgs(z)), _)) :: b if b != Nil => err("a var args op parameter must be the final one")
      case Def(Arg(_, Def(VarArgs(z)), _)) :: b if b != Nil => err("a var args op parameter must be the final one.")
      //case a :: Def(VarArgs(z)) :: b if b != Nil => err("a var args op parameter must be the final one")
      //case Def(VarArgs(z)) :: b if b != Nil => err("a var args op parameter must be the final one.")
      case _ => // ok
    }
    
    if (retTpe == MUnit && effect == pure) {
      warn("op " + name + " has return type " + MUnit.name + " but no effects, so it is a no-op")
    }
        
    // always add source context
    val amendedImplicitArgs = (MSourceContext :: implicitArgs).distinct 
    
    val o = Op(_grp, name, style, tpePars, args, amendedImplicitArgs, retTpe, effect, aliasHint)
    val opsGrp = OpsGrp.getOrElseUpdate(_grp, new DSLOps {
      val grp = _grp
      override def targets: List[CodeGenerator] = Nil
    })
    opsGrp.ops ::= o
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
          if (current.decls.contains(g))
            warn("op " + op.name + " already has codegen rule defined at " + quotePos(current.decls(g).decl) + " for generator " + g.name + ". it is being replaced with rule defined at " + quotePos(d.decl))
          current.decls(g) = d        
        }
      case (Some(x),y) => 
        warn("op " + op.name + " already has implementation rule " + x + ". It is being overwritten with " + rule)                  
        Impls(op) = rule      
      case _ =>
        Impls(op) = rule            
    }
    
    // no matter what branch we took, we may need to reconcile codegen rules
    if (rule.isInstanceOf[CodeGen])
      reconcileCodegenRule(op,rule.asInstanceOf[CodeGen])    
  }
        
  // vet codegen rules and update the ops list if necessary
  def reconcileCodegenRule(op: Rep[DSLOp], rule: CodeGen) {
    if (op.args.exists(a => DataStructs.contains(a.tpe)))
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
    // verify the dc functions match our expectations
    
    // -- below causes scalac typer crash :(
    // if ((dc.alloc.args.size != 1 || dc.alloc.args.apply(0).tpe != MInt || dc.alloc.retTpe != tpe))
    //   // TODO: how should this work? alloc can really map to anything.. e.g can have other fields that get mapped from the inputs in arbitrary ways
    //   // needs to be specified in the zip in some context attached to the alloc method?      
    //   err("dcAlloc must take a single argument of type " + MInt.name + " and return an instance of " + tpe.name)
    // if (dc.size.args.size != 1 || getHkTpe(dc.size.args.apply(0).tpe) != tpe || (dc.size.retTpe != MInt))
    //   err("dcSize must take a single argument of type " + tpe.name + " and return an MInt")
    // if (dc.apply.args.size != 2 || (getHkTpe(dc.apply.args.apply(0).tpe), dc.apply.args(1).tpe) != (tpe, MInt) || (dc.apply.retTpe != dc.tpeArg))
    //   err("dcApply must take two arguments of type(" + tpe.name + ", " + MInt.name + ") and return a " + dc.tpeArg.name)
    // if (dc.update.args.size != 3 || (getHkTpe(dc.update.args.apply(0).tpe), dc.update.args.apply(1).tpe, dc.update.args.apply(2).tpe) != (tpe, MInt, dc.tpeArg) || (dc.update.retTpe != MUnit))
    //   err("dcUpdate must take arguments of type (" + tpe.name + ", " + MInt.name + ", " + dc.tpeArg.name + ") and return " + MUnit.name)
    
    ForgeCollections += (tpe -> dc)
    ()
  }
  
  /* Establishes that the given tpe implements the ParallelCollectionBuffer interface */
  def forge_isparallelcollection_buffer(tpe: Rep[DSLType], dc: ParallelCollectionBuffer) = {
    ForgeCollections += (tpe -> dc)
    ()
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
}


trait ScalaGenForgeOps extends ScalaGenBase {
  val IR: ForgeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

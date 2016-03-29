package ppl.dsl.forge
package core

import java.io.{File,PrintWriter,FileWriter}
import scala.tools.nsc.io.{Directory,Path}
import scala.reflect.SourceContext
import scala.collection.mutable.{ArrayBuffer,HashSet,HashMap}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import library._
import format._

trait ForgeApplication extends Forge with ForgeLift with ForgeLib {
  def dslName: String
  def specification(): Rep[Unit]
  def addREPLOverride = false // DSLs can override this to true if they need a REPL override trait
}

/**
 * These are libraries written in Forge that are available to be imported by DSL authors.
 */
trait ForgeLib extends ScalaOps with AutoOps with BitSetOps with KeyValueStoreOps {
  this: ForgeApplication =>
}

/**
 * These are the portions of Scala imported into Forge's scope.
 */
trait ForgeLift extends LiftString with LiftBoolean with LiftNumeric with LiftPrimitives {
  this: Forge =>
}

trait ForgeScalaOpsPkg extends Base
  with ImplicitOps with OrderingOps with StringOps with ArrayOps
  with BooleanOps with PrimitiveOps with TupleOps with CastingOps

trait ForgeScalaOpsPkgExp extends ForgeScalaOpsPkg
  with ImplicitOpsExp with OrderingOpsExp with StringOpsExp  with ArrayOpsExp
  with BooleanOpsExp with PrimitiveOpsExp with TupleOpsExp with CastingOpsExp

trait ForgeScalaCodeGenPkg extends ScalaGenEffect
  with ScalaGenImplicitOps with ScalaGenOrderingOps with ScalaGenStringOps  with ScalaGenArrayOps
  with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenTupleOps with ScalaGenCastingOps
  { val IR: ForgeScalaOpsPkgExp  }


/**
 * This the trait that every Forge application must extend.
 */
trait Forge extends ForgeScalaOpsPkg with Definitions with ForgeSugar with FieldOps with QuoteOps {
  this: ForgeApplication =>

  // exposed to end-users, but obfuscated, for what it's worth
  def forge_err(s: String)(implicit ctx: SourceContext): Unit
  def forge_warn(s: String)(implicit ctx: SourceContext): Unit

  // exposed since DSL developers may also need to introspect
  def canonicalName(tpe: Rep[DSLType]): String
  def isTpePar(tpe: Rep[DSLType]): Boolean
  def asTpePar(tpe: Rep[DSLType]): Rep[TypePar]
  def isTpeInst(tpe: Rep[DSLType]): Boolean
  def isTpeClass(grp: Rep[DSLGroup]): Boolean
  def asTpeClass(grp: Rep[DSLGroup]): Rep[DSLTypeClass]
  def isTpeClassInst(grp: Rep[DSLGroup]): Boolean
  def asTpeClassInst(grp: Rep[DSLGroup]): Rep[DSLTypeClassInst]
}

/**
 * These are the corresponding IR nodes for Forge.
 */
trait ForgeExp extends Forge with ForgeUtilities with ForgeScalaOpsPkgExp with DefinitionsExp with ForgeOpsExp with FieldOpsExp with QuoteOpsExp {
  this: ForgeApplication =>

  // -- for fast compile mode

  def flattenIR() {
    val flat = grp("$Flat")
    val newOps = new ArrayBuffer[Exp[DSLOp]]()
    val newTargets = new HashSet[CodeGenerator]()
    val save = new HashMap[Exp[DSLGroup], DSLOps]()
    for ((grp,opsGrp) <- OpsGrp) {
      if (isTpeClass(grp) || isTpeClassInst(grp)) {
        save(grp) = opsGrp
      }
      else {
        newOps ++= opsGrp.ops
        newTargets ++= opsGrp.targets
      }
    }
    val newOpsGrp = new DSLOps {
      val grp = flat
      override def targets: List[CodeGenerator] = newTargets.toList
    }
    newOpsGrp.ops = newOps.toList
    OpsGrp.clear()
    OpsGrp(flat) = newOpsGrp
    for ((grp,opsGrp) <- save) {
      OpsGrp(grp) = opsGrp
    }
  }

  // -- IR helpers

  def isForgePrimitiveType(t: Rep[DSLType]) = t match {
    case `MShort` | `MInt` | `MLong` | `MFloat` | `MDouble` | `MBoolean` | `MChar` | `MByte` | `MString` | `MUnit` | `MAny` | `MNothing` | `MLambda` | `MSourceContext` | `byName` => true
    case `CShort` | `CInt` | `CLong` | `CFloat` | `CDouble` | `CBoolean` | `CChar` | `CByte` | `CString` | `CUnit` | `CAny` | `CNothing` => true
    // case Def(Tpe(_,_,`now`)) => true
    case Def(Tpe(name,_,_)) if name.startsWith("Tuple") => true
    case Def(Tpe(name,_,_)) if primitiveTpePrefix exists { t => name.startsWith(t) } => true
    case Def(Tpe("ForgeArray",_,_)) | Def(Tpe("ForgeArrayBuffer",_,_)) | Def(Tpe("ForgeHashMap",_,_)) => true
    case Def(Tpe("ForgeFileInputStream",_,_)) | Def(Tpe("ForgeFileOutputStream",_,_)) => true
    case Def(Tpe("Var",_,_)) => true
    case Def(Tpe("Overloaded",_,_)) => true
    case _ => false
  }

  def opsGrpTpes(opsGrp: DSLOps) = {
    // assumption: all ops associated with a particular data structure belong to exactly 1 group
    opsGrp.ops.collect { case o if (grpIsTpe(o.grp)) => grpAsTpe(o.grp) }.distinct
  }

  def opsGrpOf(o: Rep[DSLOp]) = {
    OpsGrp.get(o.grp).orElse(OpsGrp.find(g => g._2.ops.contains(o)).map(_._2))
  }

  def grpIsTpe(grp: Rep[DSLGroup]) = grp match {
    case Def(Tpe(n,targs,s)) => true
    case Def(TpeInst(t,args)) => true
    case Def(TpePar(n,ctx,s)) => true
    case Def(HkTpePar(n,tpars,ctx,s)) => true
    case Def(TpeClass(n,sig,targs)) => true
    case Def(TpeClassInst(n,targs,t)) => true
    case _ => false
  }
  def grpAsTpe(grp: Rep[DSLGroup]): Rep[DSLType] = grp match {
    case t@Def(Tpe(n,targs,s)) => t.asInstanceOf[Rep[DSLType]]
    case t@Def(TpeInst(hk,args)) => t.asInstanceOf[Rep[DSLType]]
    case t@Def(TpePar(n,ctx,s)) => t.asInstanceOf[Rep[DSLType]]
    case t@Def(HkTpePar(n,tpars,ctx,s)) => t.asInstanceOf[Rep[DSLType]]
    case t@Def(TpeClass(n,sig,targs)) => t.asInstanceOf[Rep[DSLType]]
    case t@Def(TpeClassInst(n,targs,t2)) => t.asInstanceOf[Rep[DSLType]]
    // case _ => err(grp.name + " is not a DSLType")
  }

  def canonicalName(x: Rep[DSLType]): String = {
    val prefix = if (x.stage == future) "Rep" else ""
    prefix + x.name + x.tpeArgs.map(canonicalName).mkString("")
  }

  def isTpePar(tpe: Rep[DSLType]) = tpe match {
    case Def(TpePar(_,_,_)) => true
    case Def(HkTpePar(_,_,_,_)) => true
    case _ => false
  }
  def asTpePar(tpe: Rep[DSLType]) = tpe match {
    case t@Def(TpePar(_,_,_)) => t.asInstanceOf[Rep[TypePar]]
    case t@Def(HkTpePar(_,_,_,_)) => t.asInstanceOf[Rep[TypePar]]
  }

  def isTpeInst(tpe: Rep[DSLType]) = tpe match {
    case Def(TpeInst(_,_)) => true
    case _ => false
  }

  def isTpeClass(grp: Rep[DSLGroup]) = grp match {
    case Def(TpeClass(_,_,_)) => true
    case Def(TpeClassEvidence(_,_,_)) => true
    case _ => false
  }
  def asTpeClass(grp: Rep[DSLGroup]) = grp match {
    case t@Def(TpeClass(_,_,_)) => t.asInstanceOf[Rep[DSLTypeClass]]
    case t@Def(TpeClassEvidence(_,_,_)) => t.asInstanceOf[Rep[DSLTypeClass]]
  }

  def isTpeClassInst(grp: Rep[DSLGroup]) = grp match {
    case Def(TpeClassInst(_,_,_)) => true
    case _ => false
  }
  def asTpeClassInst(grp: Rep[DSLGroup]) = grp match {
    case t@Def(TpeClassInst(_,_,_)) => t.asInstanceOf[Rep[DSLTypeClassInst]]
  }

  def getHkTpe(tpe: Rep[DSLType]) = tpe match {
    case Def(TpeInst(hkTpe,_)) => hkTpe
    case _ => tpe
  }

  def isFuncArg(a: Rep[DSLArg]) = a match {
    case Def(Arg(_, Def(FTpe(args,ret,freq)),_)) => true
    case _ => false
  }

  def hasFuncArgs(o: Rep[DSLOp]) = {
    o.args.exists(isFuncArg) || o.implicitArgs.exists(isFuncArg)
  }

  def hasDefaultValue(a: Rep[DSLArg]) = a match {
    case Def(Arg(_, _, Some(x))) => true
    case _ => false
  }

  def isThunk(f: Rep[DSLType]) = f match {
    case Def(FTpe(List(Def(Arg(_,`byName`,_))),ret,freq)) => true
    case _ => false
  }

  def isRedirect(o: Rep[DSLOp]) = Impls.contains(o) && Impls(o).isInstanceOf[Redirect]
}

trait ForgeUtilities {
  this: ForgeExp =>

  def err(s: String)(implicit ctx: SourceContext) = {
    println("[forge error]: " + s)
    // println("  at " + (ctx.fileName.split("/").last + ":" + ctx.line))
    println("  at " + quotePos(fresh[Nothing].withPos(List(ctx))))
    sys.exit(1)
  }
  def warn(s: String) = println("[forge warning]: " + s)
  def info(s: String) = println("[forge]: " + s)

  def forge_err(s: String)(implicit ctx: SourceContext) = err(s)
  def forge_warn(s: String)(implicit ctx: SourceContext) = warn(s)
}


/**
 * Forge code generators
 */
trait ForgeCodeGenBase extends GenericCodegen with ScalaGenBase {
  val IR: ForgeApplicationRunner with ForgeExp
  import IR._

  def targetName: String
  lazy val packageName = dsl.toLowerCase() + "." + targetName
  lazy val buildDir = build + File.separator + dsl + File.separator + targetName
  lazy val dslDir = buildDir + File.separator + "src" + File.separator + dsl.toLowerCase() + File.separator + targetName + File.separator

  // -- code gen helpers

  def varify(a: Exp[Any]): String = a match {
    case Def(Arg(name, tpe, default)) => varify(tpe)
    case Def(FTpe(args,ret,freq)) => err("variables in function tpe")
    case _ => "Var[" + quote(a) + "]"
  }

  def repify(a: Exp[Any]): String = a match {
    case Def(Arg(name, tpe, default)) => repify(tpe)
    case Def(FTpe(args,ret,freq)) => args match {
      case List(Def(Arg(_,`byName`,_))) => " => " + repify(ret)
      case _ => "(" + args.map(repify).mkString(",") + ") => " + repify(ret)
    }
    case Def(Tpe(name, arg, `compile`)) => quote(a)
    case Def(Tpe("Var", arg, stage)) => repify(arg(0))
    case Def(TpePar(name, ctx, `compile`)) => quote(a)
    case Def(HkTpePar(name, tpePars, ctx, `compile`)) => quote(a)
    case Def(TpeClass(_,_,_)) | Def(TpeClassEvidence(_,_,_)) | Def(TpeClassInst(_,_,_)) => quote(a)
    case Def(TpeInst(Def(Tpe(name, args, `compile`)), args2)) => name + (if (!args2.isEmpty) "[" + args2.map(repifySome).mkString(",") + "]" else "") // implicits don't auto-convert things wrapped in an outer tpe, so we still use repifySome
    case Def(TpeInst(Def(Tpe("Var",a1,s1)), a2)) => repify(a2(0))
    case Def(VarArgs(t)) => "Seq[" + repify(t) + "]"
    case _ => "Rep[" + quote(a) + "]"
  }

  def repifySome(a: Exp[Any]): String = a match {
    case Def(Arg(name, tpe, default)) => repifySome(tpe)
    case Def(FTpe(args,ret,freq)) => args match {
      case List(Def(Arg(_,`byName`,_))) => " => " + repifySome(ret)
      case _ => "(" + args.map(repifySome).mkString(",") + ") => " + repifySome(ret)
    }
    case Def(Tpe(name, arg, `now`)) => quote(a)
    case Def(Tpe("Var", arg, stage)) => varify(arg(0))
    case Def(TpePar(name, ctx, `now`)) => quote(a)
    case Def(HkTpePar(name, tpePars, ctx, `now`)) => quote(a)
    case Def(TpeInst(Def(Tpe(name, args, `now` | `compile`)), args2)) => name + (if (!args2.isEmpty) "[" + args2.map(repifySome).mkString(",") + "]" else "") // is this the right thing to do?
    case Def(TpeInst(Def(Tpe("Var",a1,s1)), a2)) => varify(a2(0))
    case Def(VarArgs(t)) => repifySome(t) + "*"
    case _ => repify(a)
  }

  def argify(a: Exp[DSLArg], typify: Exp[DSLType] => String = repify): String = a match {
    case Def(Arg(name, tpe@Def(FTpe(args,ret,freq)), Some(d))) => name + ": " + typify(tpe) + " = " + escape(d)
    case Def(Arg(name, tpe, Some(d))) => name + ": " + typify(tpe) + " = " + escape(d)
    case Def(Arg(name, tpe, None)) => name + ": " + typify(tpe)
  }

  def makeTpeParsWithBounds(args: List[Rep[TypePar]]): String = {
    if (args.length < 1) return ""
    val args2 = args.map(a => a match {
      // Do not include HkTpePar ctxBounds, which must be supplied as implicits
      case Def(TpePar(n,c,s)) if a.ctxBounds != Nil => quote(a) + ":" + a.ctxBounds.map(_.name).mkString(":")
      case _ => quote(a)
    })
    "[" + args2.mkString(",") + "]"
  }

  def makeTpePars(args: List[Rep[DSLType]]): String = {
    if (args.length < 1) return ""
    "[" + args.map(quote).mkString(",") + "]"
  }

  // This version is needed for arguments to higher-kinded type parameters,
  // as they should be quoted by name.
  def makeTpeParsAsArgs(pars: List[Rep[TypePar]], args: List[Rep[DSLType]]): String = {
    if (args.length < 1) return ""
    val args2 = pars.zip(args).map { case (p,a) =>
      p match {
        case Def(HkTpePar(n,args,ctx,stage)) => getHkTpe(a).name
        case _ => quote(a)
      }
    }
    "[" + args2.mkString(",") + "]"
  }

  // TODO: we should do this for regular op impls as well (issue 22), but it seems to interact differently with string interpolation (need to investigate)
  // the issue is that string interp returns us a string with quoted newlines which we *do not want* to escape further -- although escapes inside quotes
  // in the original string should be further escaped.
  def escape(s: String) = {
    var o = s
    // we lose user backslashes in the code gen step
    o.replace("\\", "\\\\")
  }

  var quoteLiterally = false
  def quoteLiteral(x: Exp[Any]): String = {
    val save = quoteLiterally
    quoteLiterally = true
    val z = quote(x)
    quoteLiterally = save
    z
  }

  override def quote(x: Exp[Any]): String = x match {
    case Def(Tpe(s,args,stage)) => s + makeTpePars(args)
    case Def(TpeInst(t,args)) => t.name + makeTpePars(args)
    case Def(TpePar(s,ctx,stage)) => s
    case Def(HkTpePar(s,args,ctx,stage)) => s + makeTpePars(args)
    case Def(TpeClass(s,sig,args)) => s + makeTpePars(args)
    case Def(TpeClassEvidence(s,sig,args)) =>
      val pars = lookupTpeClass(s).map(_.tpePars).getOrElse(args.map(asTpePar))
      s + makeTpeParsAsArgs(pars, args)
    case Def(TpeClassInst(s,args,t)) => quote(t)
    case Def(FTpe(args,ret,freq)) => "(" + args.map(a => quote(a.tpe)).mkString(",") + ") => " + quote(ret)
    case Def(StringPlus(a,b)) => quote(quoteLiteral(a)+quoteLiteral(b))
    case Def(Arg(name,Def(Tpe(s,_,_)),default)) if s.startsWith("Overload") => s.toLowerCase // convention established in GenOverloadHack.scala (TODO: use constant name)
    case Def(Arg(name,tpe,default)) => name
    case s@Sym(n) => err("could not resolve symbol " + findDefinition(s).toString + ". All Forge symbols must currently be statically resolvable.")
    case _ => super.quote(x)
  }
}

/**
 * This is the interface that all backends must implement to generate an implementation of the DSL
 * from the specification.
 */
trait ForgeCodeGenBackend extends ForgeCodeGenBase with ForgeScalaCodeGenPkg with ScalaGenForgeOps {
  val IR: ForgeApplicationRunner with ForgeExp

  def emitDSLImplementation(): Unit
}

package ppl.dsl.forge
package templates
package compiler

import java.io.{File,PrintWriter,FileWriter}
import scala.tools.nsc.io.{Directory,Path}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import core._
import shared._
import Utilities._

trait ForgeCodeGenDelite extends ForgeCodeGenBackend with DeliteGenPackages with DeliteGenDataStructures with DeliteGenOps with DeliteGenImports with DeliteGenTraversals with BaseGenMetadata {
  val IR: ForgeApplicationRunner with ForgeExp
  import IR._

  lazy val targetName = "compiler"

  def makeEffectAnnotation(effect: EffectType, o: Rep[DSLOp]) = effect match {
    case `pure` => "reflectPure"
    case `mutable` => "reflectMutable" + makeTpePars(o.retTpe)
    case `simple` => "reflectEffect" + makeTpePars(o.retTpe)
    case write(args @ _*) => "reflectWrite" + makeTpePars(o.retTpe) + "(" + args.map(i => o.args.apply(i).name).mkString(",") + ")"
    case `global` => err("TODO")
  }

  def makeFrequencyAnnotation(freq: Frequency) = freq match {
    case `normal` => "freqNormal"
    case `hot` => "freqHot"
    case `cold` => "freqCold"
  }

  def blockify(a: Exp[Any]): String = a match {
    case Def(Arg(name, tpe, default)) => blockify(tpe)
    case Def(FTpe(args,ret,freq)) => "Block[" + quote(ret) + "]"
    case _ => repify(a)
  }

  def emitDSLImplementation() = {
    Directory(Path(dslDir)).createDirectory()
    emitDSLDefinition()
    // emitDataStructures()
    emitOps()
    emitMetadata()
    emitTraversals()
  }

  def emitDSLDefinition() {
    val dslStream = new PrintWriter(new FileWriter(dslDir+dsl+".scala"))
    val genOps = OpsGrp.filterNot(t => isTpeClass(t._1) || isTpeClassInst(t._1)).values.toList
    dslStream.println("package " + packageName)
    dslStream.println()
    emitAllImports(dslStream)
    dslStream.println()
    emitApplicationRunner(dslStream)
    dslStream.println()
    emitDSLPackageDefinitions(genOps, dslStream)
    dslStream.println()
    emitDSLCodeGeneratorPackageDefinitions(genOps, dslStream)
    dslStream.close()
  }

  // def emitDataStructures() {
  //   val dataDir = dslDir + File.separator + "datastruct"
  //   Directory(Path(dataDir)).createDirectory()
  //   emitStructs(dataDir)
  // }

  def emitOpsExp(traitName: String, base: String, opsGrps: List[DSLOps], stream: PrintWriter) {
    val ops = opsGrps.flatMap{opsGrp => unique(opsGrp.ops) }
    val tpes = opsGrps.flatMap{opsGrp => opsGrpTpes(opsGrp) }

    stream.println("package " + packageName + ".ops")
    stream.println()
    emitScalaReflectImports(stream)
    emitLMSImports(stream)
    emitDeliteOpsImports(stream)
    stream.println("import ppl.delite.framework.Config") // e.g. used for rewrites
    emitDSLImports(stream)
    stream.println()

    emitBlockComment("IR Definitions", stream)
    stream.println("trait " + traitName + "OpsExp extends " + base + " {")
    stream.println("  this: " + dsl + "Exp => ")
    stream.println()
    emitIRNodes(ops, stream)
    stream.println()
    emitNodeConstructors(ops, stream)
    stream.println()
    emitSyms(ops, stream)
    stream.println()
    emitAliasInfo(ops, stream)
    stream.println()
    emitMirrors(ops, stream)
    stream.println()
    emitDeliteCollection(tpes, stream)
    stream.println()
    emitStructMethods(tpes, stream)
    stream.println()
    stream.println("}")
    stream.println()
  }

  def emitOpsImpl(traitName: String, opsGrps: List[DSLOps], stream: PrintWriter) {
    val ops = opsGrps.flatMap{opsGrp => unique(opsGrp.ops) }

    stream.println("package " + packageName + ".ops")
    stream.println()
    emitScalaReflectImports(stream)
    emitDSLImports(stream)
    stream.println()

    emitBlockComment("Op Implementations", stream)
    stream.println("trait " + traitName + "OpsImpl {")
    stream.println("  this: " + dsl + "CompilerOps with " + dsl + "Lift => ")
    stream.println()
    emitImpls(ops, stream)
    stream.println("}")
  }


  def emitOps() {
    val opsDir = dslDir + File.separator + "ops"
    Directory(Path(opsDir)).createDirectory()

    // 1 or 2 files per group: First includes OpsExp, Rewrites, and codegen
    // Second is an additional Impl file if the group contains SingleTask and Composite ops
    // TODO: Would it make sense to just put both of these in one file?
    for ((grp,opsGrp) <- OpsGrp if !isTpeClass(grp) && !isTpeClassInst(grp) && !isMetahelp(grp)) {
      val stream = new PrintWriter(new FileWriter(opsDir+File.separator+grp.name+"OpsExp.scala"))
      emitOpsExp(grp.name, baseOpsCls(opsGrp), List(opsGrp), stream)
      emitOpRewrites(opsGrp, stream)
      emitOpCodegen(opsGrp, stream)
      stream.close()

      if (opsGrp.ops.exists(requiresImpl)) {
        val implStream = new PrintWriter(new FileWriter(opsDir+File.separator+grp.name+"OpsImpl.scala"))
        emitOpsImpl(grp.name, List(opsGrp), implStream)
        implStream.close()
      }
    }

    val metahelpers = OpsGrp.toList.filter{case (grp,opsGrp) => isMetahelp(grp) }.map(_._2)
    if (!metahelpers.isEmpty) {
      val stream = new PrintWriter(new FileWriter(opsDir+File.separator+dsl+"MetadataOpsExp.scala"))
      emitOpsExp(dsl+"Metadata", "EffectExp with "+dsl+"MetadataInternalOps", metahelpers, stream)
      stream.close()

      val implStream = new PrintWriter(new FileWriter(opsDir+File.separator+dsl+"MetadataOpsImpl.scala"))
      emitOpsImpl(dsl+"Metadata", metahelpers, implStream)
      implStream.close()
    }
  }


  def emitMetadata() {
    val MetaTpes = Tpes.filter(t => !isForgePrimitiveType(t) && DataStructs.contains(t) && isMetaType(t))
    if (MetaTpes.nonEmpty) {
      val stream = new PrintWriter(new FileWriter(dslDir+File.separator+dsl+"MetadataClasses.scala"))
      stream.println("package " + packageName)
      emitDSLImports(stream)
      emitLMSImports(stream)
      stream.println("import scala.virtualization.lms.common.MetadataOps")
      stream.println()
      emitMetadataClasses(dsl + "CompilerOps", stream, repify)
      stream.close()
    }
  }


  def emitTraversals() {
    val traversalDir = dslDir + File.separator + "transform"
    Directory(Path(traversalDir)).createDirectory()
    for (t <- Traversals if !t.isExtern) {
      val stream = new PrintWriter(new FileWriter(traversalDir+File.separator+makeTraversalName(t)+".scala"))
      stream.println("package " + packageName + ".transform")
      stream.println()
      emitScalaReflectImports(stream)
      emitDeliteTraversalImports(stream)
      //emitLMSImports(stream)
      emitDSLImports(stream)
      stream.println()
      emitTraversalDefs(t, stream)
      stream.close()
    }

    // DSLTransform
    val stream = new PrintWriter(new FileWriter(traversalDir+File.separator+dsl+"Transform.scala"))
    stream.println("package " + packageName + ".transform")
    stream.println()
    emitDSLImports(stream)
    emitLMSImports(stream)
    emitDelitePackageImports(stream)
    stream.println()
    stream.println("trait " + dsl + "Transforming extends " + dsl + "Exp with DeliteStructsExp with DeliteTransforming {")
    stream.println("  this: " + dsl + "Compiler with " + dsl + "Application with DeliteApplication =>")
    /*for ((grp,opsGrp) <- OpsGrp) {
      for (op <- unique(opsGrp)) {
        Impls(op) match {
          case _:Figment | _:AllocatesFigment if op.backend != libraryBackend =>
            if (!Transformers.exists{case (t,pattern) => pattern.rules.contains(op)})
              warn("No lowering rule defined for op " + op.name + ". Instantiated figment ops must be lowered prior to code generation.")

            stream.println(makeLowerMethodSignature(op, stream) + " = {")
            stream.println("  throw new Exception(\"No lowering rule for op " + op.name + "\")")
            stream.println("}")
          case _ =>
        }
    }}*/
    stream.println("}")


    stream.print("trait " + dsl + "Transform extends DeliteTransform")
    Traversals.filter(hasIR).foreach{t => stream.print(" with " + makeTraversalIRName(t))}
    stream.println(" {")
    stream.println("  this: " + dsl + "Compiler with " + dsl + "Application with DeliteApplication =>")
    stream.println()
    emitTypeMetadata(stream)
    stream.println("}")
    stream.close()

    val azstream = new PrintWriter(new FileWriter(traversalDir+File.separator+dsl+"AnalyzerBase.scala"))
    azstream.println("package " + packageName + ".transform")
    azstream.println()
    emitScalaReflectImports(azstream)
    emitDeliteTraversalImports(azstream)
    emitDSLImports(azstream)
    //emitLMSImports(azstream)
    azstream.println()
    emitAnalyzerBase(azstream)
    azstream.println()
    azstream.close()
  }
}

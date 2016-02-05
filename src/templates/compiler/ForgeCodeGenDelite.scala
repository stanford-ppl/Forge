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

trait ForgeCodeGenDelite extends ForgeCodeGenBackend with DeliteGenPackages with DeliteGenDataStructures with DeliteGenOps with DeliteGenImports with DeliteGenTraversals {
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
    println("[Forge] Emitting compiler backend")
    Directory(Path(dslDir)).createDirectory()
    emitDSLDefinition()
    // emitDataStructures()
    emitOps()
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

  def emitOps() {
    val opsDir = dslDir + File.separator + "ops"
    Directory(Path(opsDir)).createDirectory()

    // Up to 3 files per group: First includes OpsExp, Rewrites, and codegen
    // Second is an additional Impl file if the group contains SingleTask and Composite ops
    // Third is compiler-only ops if the group contains any
    // TODO: Would it make sense to just put all three of these in one file?
    for ((grp,opsGrp) <- OpsGrp if !isTpeClass(grp) && !isTpeClassInst(grp) && opsGrp.ops.exists(hasCompilerVersion)) {
      val stream = new PrintWriter(new FileWriter(opsDir+File.separator+grp.name+"OpsExp"+".scala"))
      stream.println("package " + packageName + ".ops")
      stream.println()
      emitScalaReflectImports(stream)
      emitLMSImports(stream)
      emitDeliteOpsImports(stream)
      emitDSLImports(stream)
      stream.println()
      emitOpExp(opsGrp, stream)
      stream.println()
      emitOpRewrites(opsGrp, stream)
      emitOpCodegen(opsGrp, stream)
      stream.close()

      if (opsGrp.ops.exists(requiresImpl)) {
        val implStream = new PrintWriter(new FileWriter(opsDir+File.separator+grp.name+"OpsImpl"+".scala"))
        implStream.println("package " + packageName + ".ops")
        implStream.println()
        emitScalaReflectImports(implStream)
        emitDSLImports(implStream)
        implStream.println()
        emitImpls(opsGrp, implStream)
        implStream.close()
      }
      if (opsGrp.ops.exists(_.backend == compilerBackend)) {
        val compStream = new PrintWriter(new FileWriter(opsDir+File.separator+grp.name+"CompilerOps.scala"))
        compStream.println("package " + packageName + ".ops")
        compStream.println()
        emitScalaReflectImports(compStream)
        emitDSLImports(compStream)
        compStream.println()
        emitCompilerOpSyntax(opsGrp, compStream)
        compStream.close()
      }
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
      emitLMSImports(stream)
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

    emitMetadataClasses(stream)

    stream.print("trait " + dsl + "Transform extends DeliteTransform")
    Traversals.filter(hasIR).foreach{t => stream.print(" with " + makeTraversalIRName(t))}
    stream.println(" {")
    stream.println("  this: " + dsl + "Compiler with " + dsl + "Application with DeliteApplication =>")
    stream.println()
    emitTypeMetadata(stream)
    stream.println("}")
    stream.close()
  }
}


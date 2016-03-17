package ppl.dsl.forge
package templates
package compiler

import java.io.PrintWriter
import scala.virtualization.lms.common._
import core._
import shared.BaseGenPackages
import Utilities._

// Included traversal gen here for emitting static traversal schedule
trait DeliteGenPackages extends BaseGenPackages with BaseGenTraversals {
  this: ForgeCodeGenDelite =>

  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  def emitApplicationRunner(stream: PrintWriter) {
    stream.println("trait " + dsl + "ApplicationCompilerTrait extends " + dsl + "Application with DeliteApplication with " + dsl+"Compiler")
    stream.println("abstract class " + dsl + "ApplicationCompiler extends " + dsl + "ApplicationCompilerTrait") //with DeliteApplication with " + dsl+"Compiler")
  }

  def targetName(g: CodeGenerator) = g match {
    case `cpp` => "Cpp" // only one that doesn't follow the convention of TargetX and XGen...
    case _ => g.name
  }

  def emitDSLPackageDefinitions(opsGrps: List[DSLOps], stream: PrintWriter) {
    emitBlockComment("DSL compiler definition", stream)

    // compiler
    stream.println("trait " + dsl + "CompilerOps extends " + dsl)
    for (opsGrp <- opsGrps) {
      // stream.print(" with " + opsGrp.name)
      if (opsGrp.ops.exists(requiresImpl))
        stream.print(" with " + opsGrp.name + "Impl")
      if (opsGrp.ops.exists(_.backend == internalBackend))
        stream.print(" with " + opsGrp.grp.name + "InternalOps")
      if (opsGrp.ops.exists(_.backend == compilerBackend))
        stream.print(" with " + opsGrp.grp.name + "CompilerOps")
    }
    for (e <- Externs) {
      stream.print(" with " + e.opsGrp.grp.name + "CompilerOps") // Legacy naming for InternalOps
    }
    val hasMetadata = Tpes.exists(t => !isForgePrimitiveType(t) && DataStructs.contains(t) && isMetaType(t))
    if (hasMetadata) stream.print(" with " + dsl + "Metadata")

    stream.println(" {")
    stream.println("  this: " + dsl + "Exp with " + dsl + "Application => ")
    stream.println("}")
    stream.println()

    // metadata ops exp -- used in code generators
    // HACK: Only expose extern stuff and metadata in code generator to support metadata ops on DSL types
    stream.print("trait " + dsl + "MetadataOpsExp extends " + dsl + "Identifiers")
    if (hasMetadata) stream.print(" with " + dsl + "Metadata")
    stream.println()
    for (opsGrp <- opsGrps if isMetaType(opsGrp.grp)) {
      stream.print(" with " + opsGrp.name + "Exp")
    }
    for (e <- Externs) {
      stream.print(" with " + e.opsGrp.name + "Exp")
    }
    stream.println(" {")
    stream.println("  this: " + dsl + "Exp =>")
    stream.println("}")
    stream.println()

    // scopes
    // NOTE: this currently only works in Delite mode. Is there a way to use scopes and still
    // delegate to a different implementation for interpreter mode?
    stream.println("trait " + dsl + "Interactive extends " + dsl + "Application with DeliteInteractive")
    stream.println("trait " + dsl + "InteractiveRunner[R] extends DeliteApplication with " + dsl + "ApplicationCompilerTrait with DeliteInteractiveRunner[R]")
    stream.println()
    stream.println("// executes scope immediately")
    stream.println("object " + dsl + " {")
    stream.println("  def apply[R](b: => R) = new Scope["+dsl+"Interactive, "+dsl+"InteractiveRunner[R], R](b)")
    stream.println("}")
    stream.println()
    stream.println("trait " + dsl + "Lower extends " + dsl + "Application with DeliteRestageOps")
    stream.println("trait " + dsl + "LowerRunner[R] extends " + dsl + "ApplicationCompilerTrait with DeliteRestageRunner[R]")
    stream.println()
    stream.println("// stages scope and generates re-stageable code")
    stream.println("object " + dsl + "_ {")
    stream.println("  def apply[R](b: => R) = new Scope["+dsl+"Lower, "+dsl+"LowerRunner[R], R](b)")
    stream.println("}")
    stream.println()

    // Exp
    stream.println("trait " + dsl + "Exp extends " + dsl + "CompilerOps with " + dsl + "TypeClasses with " + dsl + "MetadataOpsExp with ExpressionsOpt with DeliteOpsExp with DeliteRestageOpsExp with DeliteTestOpsExp")
    for (opsGrp <- opsGrps if !isMetaType(opsGrp.grp)) {
      // Group has an op with a set of rewrite rules that doesn't contain a Forwarding rule
      val hasRewrites = unique(opsGrp.ops).exists(o => Rewrites.get(o).map(rules => rules.nonEmpty && !rules.exists(_.isInstanceOf[ForwardingRule])).getOrElse(false))
      val opExpName = if (hasRewrites) opsGrp.grp.name + "RewriteOpsExp" else opsGrp.name + "Exp"
      stream.print(" with " + opExpName)
    }
    /*for (e <- Externs) {
      stream.print(" with " + e.opsGrp.name + "Exp")
    }*/
    stream.println()
    stream.println(" with DeliteAllOverridesExp {")
    stream.println("  this: " + dsl + "Compiler with " + dsl + "Application with DeliteApplication => ")
    stream.println()

    // These are the primitive Delite methods that Delite requires be forwarded, since they may be used in analyses or optimizations.
    // Note that this means the Forge-generated IR nodes for these methods will never get constructed, and rewrite rules should not
    // be written for them, as they will have no effect.
    stream.println()
    emitBlockComment("Disambiguations for Delite internal operations", stream, indent=2)
    for ((o,rules) <- Rewrites if rules.exists(_.isInstanceOf[ForwardingRule])) {
      val forwarder = rules.find(_.isInstanceOf[ForwardingRule]).get.asInstanceOf[ForwardingRule]
      val lines = inline(o, forwarder.rule, quoteLiteral).split(nl)
      // TODO: Should have better way of determining which version to override. Is this even needed?
      val signature = o.name match {
        case "__ifThenElse" | "__whileDo" => makeSyntaxSignature(o)
        case _ => makeOpMethodSignature(o)
      }
      stream.print("  override " + signature + " = ")
      if (lines.length > 1) {
        stream.println("{")
        lines.foreach{line => emitWithIndent(line, stream, 4) }
        stream.println("  }")
      }
      else stream.println(lines.head)
    }

    val StructTpes = Tpes.filter(t => !isForgePrimitiveType(t) && DataStructs.contains(t) && !FigmentTpes.contains(t) && !isMetaType(t))
    val FigmentStructTpes = Tpes.filter(t => !isForgePrimitiveType(t) && DataStructs.contains(t) && FigmentTpes.contains(t) && !isMetaType(t))
    val MetaTpes = Tpes.filter(t => !isForgePrimitiveType(t) && DataStructs.contains(t) && isMetaType(t))

    emitBlockComment("DSL types", stream, indent=2)
    for (tpe <- StructTpes) {
      stream.print("  abstract class " + quote(tpe))
      stream.print(ForgeCollections.get(tpe).map(c => " extends DeliteCollection[" + quote(c.tpeArg) + "]").getOrElse(""))
      //if (ForgeCollections.contains(tpe) && TpeParents.contains(tpe)) stream.print(" with " + quote(TpeParents(tpe)))
      //else if (TpeParents.contains(tpe)) stream.print(" extends " + quote(TpeParents(tpe)))
      stream.println()
    }
    for (tpe <- FigmentStructTpes) {
      stream.print("  abstract class " + quote(tpe) + " extends FigmentStruct")
      stream.print(ForgeCollections.get(tpe).map(c => " with DeliteCollection[" + quote(c.tpeArg) + "]").getOrElse(""))
      //stream.print(TpeParents.get(tpe).map(p => " with " + quote(p)).getOrElse(""))
      stream.println()
    }
    stream.println()
    emitBlockComment("implicit manifests", stream, indent=2)
    for (tpe <- StructTpes ++ FigmentStructTpes ++ MetaTpes) {
      stream.println("  def m_" + tpe.name + makeTpeParsWithBounds(tpe.tpePars) + " = manifest[" + quote(tpe) + "]")
    }

    if (TpeParents.nonEmpty) {
      stream.println()
      emitBlockComment("Figment type inheritance", stream, indent=2)
      for ((tpe,parent) <- TpeParents) {
        stream.println("  def m_" + tpe.name + "_to_" + parent.name + makeTpeParsWithBounds(tpe.tpePars) + "(__arg0: Rep[" + tpe.name + makeTpePars(tpe.tpePars) + "]): Rep[" + parent.name + makeTpePars(parent.tpePars) + "] = __arg0.asInstanceOf[Rep[" + parent.name + makeTpePars(parent.tpePars) + "]]")
      }
    }
    stream.println("}")
    stream.println()

    // DSLCompiler
    stream.println("trait " + dsl + "Compiler extends " + dsl + "Exp with " + dsl + "Transform {") //with MultiloopSoATransformExp
    stream.println("  self: " + dsl + "Application with DeliteApplication => ")
    stream.println()

    emitBlockComment("Static config settings for DSL", stream, indent=2)
    if (!IR.enableSoA) stream.println("  Config.soaEnabled = false")
    if (!IR.enableFusion) stream.println("  Config.opfusionEnabled = false")
    if (!IR.enableStructUnwrapping) stream.println("  unwrapStructs = false")
    stream.println()
    if (!IR.enableSoA && TraversalSchedule.contains(MultiloopSoA)) {
      warn("You've disabled SoA in the compiler but scheduled SoA as a transformer!")
    }

    emitBlockComment("traversals", stream, indent=2)
    Traversals.zipWithIndex.foreach{ case (t,idx) =>
      stream.println("  private val __trv" + idx + " = new " + makeTraversalName(t) + " { override val IR: self.type = self }")
    }
    stream.println()
    emitBlockComment("traversal schedule", stream, indent=2)
    if (IR.clearTraversals) stream.println("  clearTraversals()")
    TraversalSchedule.foreach{t =>
      val idx = Traversals.indexOf(t)
      if (idx == -1) err("traversal " + t.name + " was scheduled but not declared (how did you do that?)")
      stream.println("  appendTraversal(__trv" + idx + ")")
    }

    stream.println()
    stream.println("  def getCodeGenPkg(t: Target{val IR: " + dsl + "Compiler.this.type}): GenericFatCodegen{val IR: " + dsl + "Compiler.this.type} = {")
    stream.println("    t match {")
    for (g <- generators) {
      stream.println("      case _:Target" + targetName(g) + " => new " + dsl + "Codegen" + g.name + "{val IR: " + dsl + "Compiler.this.type = " + dsl + "Compiler.this}")
    }
    stream.println("      case _ => throw new RuntimeException(\"" + dsl + " does not support this target\")")
    stream.println("    }")
    stream.println("  }")
    stream.println("}")
  }

  private def emitBaseCodegen(stream: PrintWriter) {
    val packageDir = packageName.split("\\.").map(s => "\""+s+"\"").mkString("+s+")
    stream.println("trait " + dsl + "CodegenBase extends GenericFatCodegen {")
    stream.println("  val IR: " + dsl + "Compiler with DeliteApplication")
    stream.println("  override def initialDefs = IR.deliteGenerator.availableDefs")
    stream.println()
    stream.println("  def dsmap(line: String) = line")
    stream.println("  override def emitDataStructures(path: String) {")
    stream.println("    val s = File.separator")
    stream.println("    val dsRoot = sys.env.get(\""+dsl.toUpperCase+"_HOME\").getOrElse(System.getProperty(\"user.dir\"))+s+\"compiler\"+s+\"src\"+s+"+packageDir+"+s+\"datastruct\"+s+this.toString")
    stream.println()
    stream.println("    val dsDir = Directory(Path(dsRoot))")
    stream.println("    val outDir = Directory(Path(path))")
    stream.println("    outDir.createDirectory()")
    stream.println()
    stream.println("    for(f <- dsDir.files) {")
    stream.println("      val outFile = path + s + f.name")
    stream.println("      val out = new BufferedWriter(new FileWriter(outFile))")
    stream.println("      for (line <- scala.io.Source.fromFile(f.jfile).getLines) {")
    stream.println("        out.write(dsmap(line) + \"\\n\")")
    stream.println("      }")
    stream.println("      out.close()")
    stream.println("    }")
    stream.println("  }")
    stream.println()
    stream.println("  override def remap[A](m: Manifest[A]): String = {")
    stream.println("    var res = super.remap(m)")
    stream.println("    if (res.contains(\"$\")) {")
    stream.println("      res = res.slice(res.indexOf(\"#\")+1,res.length)")
    stream.println("      res = res.slice(0, res.lastIndexOf(\".\")+1) + res.slice(res.lastIndexOf(\"$\")+1, res.length)")
    stream.println("    }")
    stream.println("    res")
    stream.println("  }")
    stream.println()
    stream.println("}")
  }

  def emitDSLCodeGeneratorPackageDefinitions(dslOps: List[DSLOps], stream: PrintWriter) {
    emitBlockComment("code generators", stream)
    emitBaseCodegen(stream)
    stream.println()
    for (g <- generators) {
      stream.println("trait " + dsl + "Codegen" + g.name + " extends " + dsl + "CodegenBase")
      for (op <- dslOps) {
        if (op.targets.contains(g))
          stream.print(" with " + g.name + "Gen" + op.name)
      }
      for (e <- Externs) {
        if (e.opsGrp.targets.contains(g))
          stream.print(" with " + g.name + "Gen" + e.opsGrp.name)
      }
      if (g == $cala) stream.println(" with ScalaGenDeliteTest ")
      if (g == cuda) stream.println(" with DeliteCppHostTransfer with DeliteCudaDeviceTransfer ")
      if (g == cpp) stream.println(" with DeliteCppHostTransfer ")
      if (g == restage && generators.contains($cala)) stream.println(" with " + dsl + "Codegen"+$cala.name + " with DeliteCodeGenRestage { ")
      else stream.println(" with " + g.name + "GenDeliteOps with Delite" + g.name + "GenAllOverrides {" )
      stream.println("  val IR: " + dsl + "Compiler with DeliteApplication")
      // TODO: generalize, other targets can have remaps too, need to be encoded somehow
      // store dsmap and remap inside the generator itself?
      if (g == $cala) {
        stream.println()
        emitComment("These methods translate types in the compiler to typed in the generated code", stream, indent=1)
        stream.println("  override def dsmap(line: String): String = {")
        stream.println("    var res = line.replaceAll(\"" + packageName + ".datastruct.scala\", this.packageName)")
        stream.println("    res")
        stream.println("  }")
        stream.println()
        stream.println("  override def remap[A](m: Manifest[A]): String = {")
        stream.println("    var res = super.remap(m)")
        stream.println("    res = res.replaceAllLiterally(\"package$\", \"\")")
        stream.println("    dsmap(res)")
        stream.println("  }")
        stream.println()
      }
      stream.println("}")
    }
  }

}

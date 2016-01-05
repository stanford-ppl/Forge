package ppl.dsl.forge
package templates
package compiler

import java.io.PrintWriter
import scala.virtualization.lms.common._
import core._
import shared.BaseGenPackages
import Utilities._

trait DeliteGenPackages extends BaseGenPackages {
  this: ForgeCodeGenDelite =>

  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  def emitApplicationRunner(stream: PrintWriter) {
    stream.println("trait " + dsl + "ApplicationCompilerTrait extends " + dsl + "Application with DeliteApplication with " + dsl+"Exp")
    stream.println("abstract class " + dsl + "ApplicationCompiler extends " + dsl + "Application with DeliteApplication with " + dsl+"Exp")
  }

  def targetName(g: CodeGenerator) = g match {
    case `cpp` => "Cpp" // only one that doesn't follow the convention of TargetX and XGen...
    case _ => g.name
  }

  def emitDSLPackageDefinitions(opsGrps: List[DSLOps], stream: PrintWriter) {
    emitBlockComment("dsl compiler definition", stream)

    // compiler
    stream.println("trait " + dsl + "Compiler extends " + dsl)
    for (opsGrp <- opsGrps) {
      // stream.print(" with " + opsGrp.name)
      if (opsGrp.ops.exists(o => Impls(o).isInstanceOf[SingleTask] || Impls(o).isInstanceOf[Composite]))
        stream.print(" with " + opsGrp.name + "Impl")
      if (opsGrp.ops.exists(_.style == compilerMethod))
        stream.print(" with " + opsGrp.grp.name + "CompilerOps")
    }
    for (e <- Externs) {
      stream.print(" with " + e.opsGrp.grp.name + "CompilerOps")
    }
    stream.println(" {")
    stream.println("  this: " + dsl + "Application with " + dsl + "Exp => ")
    stream.println()
    stream.println("}")
    stream.println()

    // scopes
    // NOTE: this currently only works in Delite mode. Is there a way to use scopes and still
    // delegate to a different implementation for interpreter mode?
    stream.println("trait " + dsl + "Interactive extends " + dsl + "Application with DeliteInteractive")
    stream.println("trait " + dsl + "InteractiveRunner[R] extends " + dsl + "ApplicationCompilerTrait with DeliteInteractiveRunner[R]")
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

    // exp
    stream.println("trait " + dsl + "Exp extends " + dsl + "Compiler with ExpressionsOpt with DeliteOpsExp with DeliteRestageOpsExp with DeliteTestOpsExp")
    for (opsGrp <- opsGrps) {
      stream.print(" with " + opsGrp.name + "Exp")
    }
    for (e <- Externs) {
      stream.print(" with " + e.opsGrp.name + "Exp")
    }
    stream.println(" with DeliteAllOverridesExp with MultiloopSoATransformExp {")
    stream.println(" this: DeliteApplication with " + dsl + "Application => ")
    stream.println()
    emitBlockComment("disambiguations for Delite internal operations", stream, indent=2)

    // These are the primitive Delite methods that Delite requires be forwarded, since they may be used in analyses or optimizations.
    // Note that this means the Forge-generated IR nodes for these methods will never get constructed, and rewrite rules should not
    // be written for them, as they will have no effect. Any way to check this?

    if (OpsGrp.keySet.exists(_.name == "Primitive")) {
      stream.println("  override def primitive_forge_int_plus(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext) = delite_int_plus(__arg0, __arg1)")
      stream.println("  override def primitive_forge_int_minus(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext) = delite_int_minus(__arg0, __arg1)")
      stream.println("  override def primitive_forge_int_times(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext) = delite_int_times(__arg0, __arg1)")
      stream.println("  override def primitive_unary_bang(__arg0: Rep[Boolean])(implicit __pos: SourceContext): Rep[Boolean] = delite_boolean_negate(__arg0)")
    }

    if (OpsGrp.keySet.exists(_.name == "Misc")) {
      stream.println("  override def misc_unsafeimmutable[A:Manifest](lhs: Rep[A])(implicit pos: SourceContext): Rep[A] = delite_unsafe_immutable(lhs)")
      stream.println("  override def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit])(implicit pos: SourceContext) = delite_while(cond, body)")
      // delite and lms if-then-else don't use by-name-parameter for cond
      stream.println("  override def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit ctx: SourceContext) = delite_ifThenElse(cond, thenp, elsep, false, true)")
      stream.println("  override def __ifThenElse[T:Manifest](cond: => Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit ctx: SourceContext) = delite_ifThenElse(cond, thenp, elsep, false, true)")
    }

    if (OpsGrp.keySet.exists(_.name == "Ordering")) {
      stream.println("  override def forge_equals[A:Manifest,B:Manifest](__arg0: Rep[A],__arg1: Rep[B])(implicit __pos: SourceContext) = delite_equals(__arg0,__arg1)")
      stream.println("  override def forge_notequals[A:Manifest,B:Manifest](__arg0: Rep[A],__arg1: Rep[B])(implicit __pos: SourceContext) = delite_notequals(__arg0,__arg1)")
    }

    stream.println()
    emitBlockComment("dsl types", stream, indent=2)
    for (tpe <- Tpes if (!isForgePrimitiveType(tpe) && DataStructs.contains(tpe) && !FigmentTpes.contains(tpe))) {
      stream.print("  abstract class " + quote(tpe))
      if (ForgeCollections.contains(tpe)) stream.println(" extends DeliteCollection[" + quote(ForgeCollections(tpe).tpeArg) + "]") else stream.println()
    }
    for (tpe <- Tpes if (!isForgePrimitiveType(tpe)) && DataStructs.contains(tpe) && FigmentTpes.contains(tpe)) {
      stream.println("  trait " + quote(tpe) + TpeParents.get(tpe).map(p => " extends " + quote(p)).getOrElse(""))
    }
    stream.println()
    emitBlockComment("implicit manifests", stream, indent=2)
    for (tpe <- Tpes if (!isForgePrimitiveType(tpe)) && DataStructs.contains(tpe) && !FigmentTpes.contains(tpe)) {
      stream.println("  def m_" + tpe.name + makeTpeParsWithBounds(tpe.tpePars) + " = manifest[" + quote(tpe) + "]")
    }
    for (tpe <- Tpes if (!isForgePrimitiveType(tpe)) && DataStructs.contains(tpe) && FigmentTpes.contains(tpe)) {
      stream.println("  def m_" + tpe.name + makeTpeParsWithBounds(tpe.tpePars) + " = manifest[" + quote(tpe) + "]")
    }

    if (TpeParents.size > 0) {
      stream.println()
      emitBlockComment("abstract type inheritance", stream, indent=2)
      for ((tpe,parent) <- TpeParents) {
        stream.println("  def m_" + tpe.name + "_to_" + parent.name + makeTpeParsWithBounds(tpe.tpePars) + "(__arg0: Rep[" + tpe.name + makeTpePars(tpe.tpePars) + "]): Rep[" + parent.name + makeTpePars(parent.tpePars) + "] = __arg0.asInstanceOf[Rep[" + parent.name + makeTpePars(parent.tpePars) + "]]")
      }
    }

    stream.println()
    stream.println("  def getCodeGenPkg(t: Target{val IR: " + dsl + "Exp.this.type}): GenericFatCodegen{val IR: " + dsl + "Exp.this.type} = {")
    stream.println("    t match {")
    for (g <- generators) {
      stream.println("      case _:Target" + targetName(g) + " => new " + dsl + "Codegen" + g.name + "{val IR: " + dsl + "Exp.this.type = " + dsl + "Exp.this}")
    }
    stream.println("      case _ => throw new RuntimeException(\"" + dsl + " does not support this target\")")
    stream.println("    }")
    stream.println("  }")
    stream.println("}")
  }

  private def emitBaseCodegen(stream: PrintWriter) {
    val packageDir = packageName.split("\\.").map(s => "\""+s+"\"").mkString("+s+")
    stream.println("trait " + dsl + "CodegenBase extends GenericFatCodegen {")
    stream.println("  val IR: DeliteApplication with " + dsl + "Exp")
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
      stream.println("  val IR: DeliteApplication with " + dsl + "Exp")
      // TODO: generalize, other targets can have remaps too, need to be encoded somehow
      // store dsmap and remap inside the generator itself?
      if (g == $cala) {
        stream.println()
        emitComment("these methods translate types in the compiler to typed in the generated code", stream, indent=1)
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

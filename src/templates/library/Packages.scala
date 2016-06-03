package ppl.dsl.forge
package templates
package library

import java.io.PrintWriter
import scala.virtualization.lms.common._
import core._
import shared.{BaseGenPackages,BaseGenOps}
import Utilities._

trait LibGenPackages extends BaseGenPackages with BaseGenOps {
  this: ForgeCodeGenInterpreter =>

  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  def emitApplicationRunner(opsGrps: List[DSLOps], stream: PrintWriter) {
    stream.println("trait " + dsl + "REPL extends " + dsl + "ApplicationInterpreter")
    if (addREPLOverride) stream.println("  with " + dsl + "REPLOverrides")

    stream.println()
    stream.println("abstract class " + dsl + "ApplicationInterpreter extends " + dsl + "Library {")
    stream.println("  var args: Rep[Array[String]] = _")
    stream.println("  var stagingArgs: Array[String] = _")
    stream.println("  final def main(argsIn: Array[String]) {")
    stream.println("    this.args = argsIn")
    stream.println("    this.stagingArgs = argsIn")
    stream.println("    main()")
    stream.println("  }")
    stream.println()

    emitBlockComment("Dismabiguations for interpreter mode", stream, indent=2)
    for (opsGrp <- opsGrps if !isTpeClass(opsGrp.grp) && !isTpeClassInst(opsGrp.grp)) {
      for (o <- unique(opsGrp.ops) if (nameClashesUniversal(o).length > 1) && !o.args.exists(a => getHkTpe(a.tpe).name == "Var")) {
        var prefix = ""
        if (o.style == infixMethod && !noInfix(o))
          prefix = "  override def infix_"
        else if (o.style == directMethod)
          prefix = "  override def "

        if (prefix != "") {
          stream.println(makeSyntaxMethod(o, prefix))
        }
      }
    }

    // other required overrides for embedded controls
    stream.println("  override def __newVar[T](x: T) = super.__newVar(x)")
    // TODO: this is ambiguous and unresolvable for some reason, even when supplying explicit arguments. possibly due to the thunks?
    // stream.println("  override def __whileDo(c: => Boolean, b: => Unit): Unit = super.__whileDo(c,b)")
    if (OpsGrp.keySet.exists(_.name == "Ordering")) {
      for (t1 <- List("Int","Float","Double")) {
        for (t2 <- List("Int","Float","Double")) {
          stream.println("  def infix_!=(x: "+t1+", y: "+t2+") = forge_notequals(x,y)")
        }
      }
      stream.println("  def infix_!=(x: Boolean, y: Boolean) = forge_notequals(x,y)")
    }

    stream.println("}")
  }

  def emitDSLPackageDefinitions(opsGrps: List[DSLOps], stream: PrintWriter) {
    emitBlockComment("DSL library definition", stream)

    // --- DSLBase
    // base trait sets Rep[T] to T and mixes in the necessary portions of the front-end, without
    // bringing the abstract impls in scope, since they can cause a recursive loop in the library
    stream.println("trait " + dsl + "Base extends " + dsl + "Identifiers {")
    stream.println("  type Rep[+T] = T")
    stream.println("  protected def unit[T:Manifest](x: T) = x")
    stream.println("  protected def param[T:Manifest](x: T) = x")
    stream.println("}")
    stream.println()

    // --- DSLLibraryOps
    // Library mixes in application ops with internal ops
    stream.println("trait " + dsl + "LibraryOps extends " + dsl + "Application")
    for (opsGrp <- opsGrps if !isMetaType(opsGrp.grp) && opsGrp.ops.exists(_.visibility == privateMethod)) {
      stream.print(" with " + opsGrp.grp.name + "InternalOps")
    }
    for (e <- Externs) { stream.print(" with " + e.opsGrp.grp.name + "CompilerOps") }
    if (hasMetadata)   { stream.print(" with " + dsl + "MetadataClasses") }
    if (hasMetatype)   { stream.print(" with " + dsl + "MetadataInternalOps") }
    stream.println(" {")
    stream.println("  this: " + dsl + "Library =>")
    stream.println("}")
    stream.println()


    // --- DSLLibrary
    // library impl brings all of the library types in scope
    stream.println("trait " + dsl + "Library extends " + dsl + "Base with " + dsl + "LibraryOps with " + dsl + "Classes {")
    //stream.println("  this: " + dsl + "Application => ")
    stream.println()
    stream.println("  // override required due to mix-in")
    stream.println("  override type Rep[+T] = T")
    stream.println()
    emitBlockComment("DSL types", stream, indent=2)
    for (tpe <- Tpes if (!isForgePrimitiveType(tpe) && DataStructs.contains(tpe))) {
      stream.println("  def m_" + tpe.name + makeTpeParsWithBounds(tpe.tpePars) + " = manifest[" + quote(tpe) + "]")
    }
    stream.println()

    // TODO: We should instead generate the actual class inheritances rather than making these shallow copies
    if (TpeParents.size > 0) {
      emitBlockComment("Figment type inheritance", stream, indent=2)
      for ((tpe,parent) <- TpeParents) {
        stream.println("  def m_" + tpe.name + "_to_" + parent.name + makeTpeParsWithBounds(tpe.tpePars) + "(__arg0: Rep[" + tpe.name + makeTpePars(tpe.tpePars) + "]): Rep[" + parent.name + makeTpePars(parent.tpePars) + "] = {")
        if (DataStructs.contains(parent)) {
          // Sanity check - child must have at least the fields that its parent does
          // TODO: Should also check types?
          val parentFields = DataStructs(parent).fields
          val childFields = DataStructs(tpe).fields
          if (!parentFields.forall(t => childFields.exists(f => f._1 == t._1)))
            err(tpe.name + " must contain all fields in its parent to inherit from " + parent.name)

          stream.println("    new " + quote(parent) + "(" + DataStructs(parent).fields.map(f => "__arg0." + f._1).mkString(",") + ")")
        }
        else {
          // TODO: What to do here? No data backing means no class, so what's the library version?
          stream.println("    __arg0.asInstanceOf[Rep[" + parent.name + makeTpePars(parent.tpePars) + "]]")
        }
        stream.println("  }")
      }
    }

    stream.println("}")
  }
}

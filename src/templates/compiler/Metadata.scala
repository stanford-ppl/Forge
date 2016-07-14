package ppl.dsl.forge
package templates
package compiler

import java.io.PrintWriter
import scala.virtualization.lms.common._
import core._

import shared.BaseGenMetadata
import Utilities._

trait DeliteGenMetadata extends BaseGenMetadata with DeliteGenOps {
  this: ForgeCodeGenDelite =>

  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  override def emitMetadataClasses(metaTpes: List[Rep[DSLMetadata]], stream: PrintWriter, typify: Rep[DSLType] => String = repify) {
    super.emitMetadataClasses(metaTpes, stream, typify)

    stream.println("")
    emitBlockComment("Metadata mirroring", stream, 2)
    stream.println("  override def mirror[T<:Metadata](m: T, f: Transformer): T = m match {")

    for (m <- metaTpes) {
      val struct = DataStructs(m)
      val mirrorer = m.name + struct.fields.zipWithIndex.map{case (field,idx) => makeTransformedArg(idx, field._2, "f") }.mkString("(", ",", ")")
      stream.println("    case " + m.name + List.tabulate(struct.fields.length){i => opArgPrefix+i}.mkString("(", ",", ")") + " => " + mirrorer + ".asInstanceOf[T]")
    }

    stream.println("    case _ => super.mirror(m, f)")
    stream.println("  }")
  }
}

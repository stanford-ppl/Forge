package ppl.dsl.forge
package templates
package compiler

import java.io.PrintWriter
import core._
import shared.BaseGenImports

trait DeliteGenImports extends BaseGenImports {
  this: ForgeCodeGenDelite =>

  val IR: ForgeApplicationRunner with ForgeExp
  import IR._

  def emitDeliteCollectionImports(stream: PrintWriter) {
    stream.println("import ppl.delite.framework.ops.DeliteCollection")
    stream.println("import ppl.delite.framework.datastructures._")
  }

  def emitDeliteOpsImports(stream: PrintWriter) {
    emitDeliteCollectionImports(stream)
    stream.println("import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}")
    stream.println("import ppl.delite.framework.Util._")
  }

  def emitDelitePackageImports(stream: PrintWriter) {
    stream.println("import ppl.delite.framework.{Config, DeliteApplication}")
    stream.println("import ppl.delite.framework.codegen.Target")
    stream.println("import ppl.delite.framework.codegen.scala.TargetScala")
    stream.println("import ppl.delite.framework.codegen.cuda.TargetCuda")
    stream.println("import ppl.delite.framework.codegen.cpp.TargetCpp")
    stream.println("import ppl.delite.framework.codegen.opencl.TargetOpenCL")
    stream.println("import ppl.delite.framework.ops._")
    stream.println("import ppl.delite.framework.datastructures._")
    stream.println("import ppl.delite.framework.codegen.delite.overrides._")
    stream.println("import ppl.delite.framework.transform._")
  }

  def emitDeliteTestImports(stream: PrintWriter) {
    stream.println("import ppl.tests.scalatest._")
  }

  def emitDeliteImports(stream: PrintWriter) {
    emitDelitePackageImports(stream)
    emitDeliteOpsImports(stream)
    emitDeliteTestImports(stream)
  }

  override def emitDSLImports(stream: PrintWriter) {
    super.emitDSLImports(stream)
    stream.println("import " + packageName + "._")
    stream.println("import " + packageName + ".ops._")
    // stream.println("import " + dsl.toLowerCase() + ".compiler.extern._")
  }

  override def emitAllImports(stream: PrintWriter) {
    super.emitAllImports(stream)
    emitLMSImports(stream)
    emitDeliteImports(stream)
  }
}

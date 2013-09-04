package ppl.dsl.forge
package templates
package library

import java.io.PrintWriter
import core._
import shared.BaseGenImports

trait LibGenImports extends BaseGenImports {
  this: ForgeCodeGenInterpreter =>

  val IR: ForgeApplicationRunner with ForgeExp
  import IR._

  def emitScalaMathImports(stream: PrintWriter) {
    stream.println("import scala.math.Ordering.Implicits._")
    stream.println("import scala.math.Numeric.Implicits._")
  }

  override def emitScalaImports(stream: PrintWriter) {
    super.emitScalaImports(stream)
    emitScalaMathImports(stream)
  }

  override def emitDSLImports(stream: PrintWriter) {
    super.emitDSLImports(stream)
    stream.println("import " + packageName + "._")
    stream.println("import " + packageName + ".classes._")
    // stream.println("import " + dsl.toLowerCase() + ".library.extern._")
  }
}

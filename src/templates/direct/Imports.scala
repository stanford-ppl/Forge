package ppl.dsl.forge
package templates
package direct

import java.io.PrintWriter
import core._

trait BaseGenImports extends ForgeCodeGenBase {
  val IR: ForgeApplicationRunner with ForgeExp
  import IR._

  def emitScalaIOImports(stream: PrintWriter) {
    stream.println("import java.io.{BufferedWriter, FileWriter, PrintWriter}")
  }

  def emitScalaReflectImports(stream: PrintWriter) {    
    stream.println("import scala.reflect.{Manifest,SourceContext}")    
  }

  def emitScalaImports(stream: PrintWriter) {
    emitScalaIOImports(stream)
    emitScalaReflectImports(stream)
  }

  def emitLMSImports(stream: PrintWriter) {
/*
    stream.println("import scala.virtualization.lms.common.{Base,BaseExp,EffectExp,BaseFatExp}")
    stream.println("import scala.virtualization.lms.common.{ScalaGenBase,ScalaGenEffect,ScalaGenFat,CudaGenFat,CGenFat}")
    stream.println("import scala.virtualization.lms.util._")
    stream.println("import scala.virtualization.lms.internal._")
*/
  }

  def emitDSLImports(stream: PrintWriter) {
    // stream.println("import " + dsl.toLowerCase() + "._")
    stream.println("import " + dsl.toLowerCase() + ".direct._")
    stream.println("import " + dsl.toLowerCase() + ".direct.ops._")
    if (OpsGrp.exists(t => isTpeClass(t._1))) {
      stream.println("import " + dsl.toLowerCase() + ".direct.typeclass._")
    }
    // stream.println("import " + dsl.toLowerCase() + ".direct.extern._")
  }

  def emitAllImports(stream: PrintWriter) {
    emitScalaImports(stream)
    emitLMSImports(stream)
    emitDSLImports(stream)
  }
}

package ppl.dsl.meta
package templates

import java.io.PrintWriter

object Imports {
  def emitScalaIOImports(stream: PrintWriter) {
    stream.println("import java.io.{BufferedWriter, FileWriter, PrintWriter}")
  }
  
  def emitScalaReflectImports(stream: PrintWriter) {
    stream.println("import scala.tools.nsc.io._")
    stream.println("import scala.reflect.{Manifest,SourceContext}")    
  }
  
  def emitScalaImports(stream: PrintWriter) {
    emitScalaIOImports(stream)
    emitScalaReflectImports(stream)
  }
  
  def emitLMSImports(stream: PrintWriter) {
    stream.println("import scala.virtualization.lms.common._")    
    stream.println("import scala.virtualization.lms.internal._")
  }
    
  def emitDeliteCollectionImport(stream: PrintWriter) {
    stream.println("import ppl.delite.framework.datastruct.scala.DeliteCollection")
  }
  
  def emitDeliteOpsImports(stream: PrintWriter) {
    emitDeliteCollectionImport(stream)
    stream.println("import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}")
    stream.println("import ppl.delite.framework.Util._")
  }
  
  def emitDelitePackageImports(stream: PrintWriter) {
    stream.println("import ppl.delite.framework.{Config, DeliteApplication}")
    stream.println("import ppl.delite.framework.codegen.Target")
    stream.println("import ppl.delite.framework.codegen.scala.TargetScala")
    stream.println("import ppl.delite.framework.codegen.cuda.TargetCuda")
    stream.println("import ppl.delite.framework.codegen.c.TargetC")
    stream.println("import ppl.delite.framework.codegen.opencl.TargetOpenCL")
    stream.println("import ppl.delite.framework.ops._")
    stream.println("import ppl.delite.framework.datastructures._")
    stream.println("import ppl.delite.framework.codegen.delite.overrides._")
  }
  
  def emitDeliteImports(stream: PrintWriter) {
    emitDelitePackageImports(stream)
    emitDeliteOpsImports(stream)
  }
  
  def emitDSLBaseImports(dsl: String, stream: PrintWriter) {
    stream.println("import " + dsl.toLowerCase() + "._")
  }
  
  def emitDSLOpsImports(dsl: String, stream: PrintWriter) {
    stream.println("import " + dsl.toLowerCase() + ".ops._")
  }
  
  def emitDSLImports(dsl: String, stream: PrintWriter) {
    emitDSLBaseImports(dsl, stream)
    emitDSLOpsImports(dsl, stream)
  }
  
  def emitAllImports(dsl: String, stream: PrintWriter) {
    emitScalaImports(stream)
    emitLMSImports(stream)
    emitDeliteImports(stream)
    emitDSLImports(dsl,stream)
  }  
}

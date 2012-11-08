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
      
  override def emitDSLImports(stream: PrintWriter) {
    super.emitDSLImports(stream)
    stream.println("import " + packageName + "._")
    stream.println("import " + packageName + ".classes._")
  }   
}
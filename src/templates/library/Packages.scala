package ppl.dsl.forge
package templates
package library

import java.io.PrintWriter
import scala.virtualization.lms.common._
import core._
import shared.BaseGenPackages
import Utilities._

trait LibGenPackages extends BaseGenPackages {  
  this: ForgeCodeGenInterpreter =>
  
  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._
 
  def emitApplicationRunner(stream: PrintWriter) {
    stream.println("trait " + dsl + "ApplicationInterpreter extends " + dsl + "Application with " + dsl+"Lib")
  }

  def emitDSLPackageDefinitions(dslOps: List[Ops], lmsCompOps: List[LMSOps], stream: PrintWriter) {
    emitBlockComment("dsl library definition", stream)
  
    // library impl sets Rep[T] to T and brings all of the library types in scope
    stream.println("trait " + dsl + "Lib extends " + dsl + " with " + dsl + "Classes {")
    stream.println("  this: " + dsl + "Application => ")
    stream.println("  type Rep[+T] = T")
    stream.println("}")
  }  
}
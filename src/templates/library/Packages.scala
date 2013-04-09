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
    stream.println("trait " + dsl + "ApplicationInterpreter extends " + dsl + "Application with " + dsl+"Lib {")
    stream.println("  var args: Rep[Array[String]] = _")
    stream.println("  final def main(argsIn: Array[String]) {")
    stream.println("    this.args = argsIn")  
    stream.println("    main()")
    stream.println("  }")    
    stream.println("}")
  }

  def emitDSLPackageDefinitions(ops: List[DSLOps], stream: PrintWriter) {
    emitBlockComment("dsl library definition", stream)
    
    // base trait sets Rep[T] to T and mixes in the necessary portions of the front-end, without
    // bringing the abstract impls in scope, since they can cause a recursive loop in the library    
    stream.println("trait " + dsl + "Base extends Base with OverloadHack {")
    stream.println("  type Rep[+T] = T")
    stream.println("  protected def unit[T:Manifest](x: T) = x")
    stream.println("}")
    stream.println()
    
    // library impl brings all of the library types in scope
    stream.println("trait " + dsl + "Lib extends " + dsl + " with " + dsl + "Base with " + dsl + "Classes {")
    stream.println("  this: " + dsl + "Application => ")
    stream.println()
    stream.println("  // override required due to mix-in")
    stream.println("  override type Rep[+T] = T")
    stream.println()
    emitBlockComment("dsl types", stream, indent=2)
    for (tpe <- Tpes) {
      if (OpsGrp.contains(tpe) && !isForgePrimitiveType(tpe)) {
        stream.println("  def m_" + tpe.name + makeTpeParsWithBounds(tpe.tpePars) + " = manifest[" + quote(tpe) + "]")      
      }
    }    
    stream.println("}")
  }  
}
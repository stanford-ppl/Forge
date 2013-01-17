package ppl.dsl.forge
package templates
package shared

import java.io.PrintWriter
import scala.virtualization.lms.common._
import core._
import Utilities._

trait BaseGenPackages extends ForgeCodeGenBase {  
  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._
 
  def emitApplicationRunnerBase(stream: PrintWriter) {
    emitComment("the trait that all " + dsl + " applications must extend", stream)
    stream.println("trait " + dsl + "Application extends " + dsl + " with " + dsl + "Lift {")
    stream.println("  var args: Rep[Array[String]]")
    stream.println("  def main()")    
    stream.println("}")
  }

  def emitDSLPackageDefinitionsBase(opsGrps: List[DSLOps], stream: PrintWriter) {
    emitBlockComment("dsl definition", stream)
    
    // Lift
    stream.println("trait " + dsl + "Lift")
    val liftOps = Lifts.keys.toList
    if (liftOps.length > 0) {
      stream.print("  extends Lift" + liftOps.head.name)
    }
    for (grp <- liftOps.drop(1)) {
      stream.print(" with Lift" + grp.name)
    }
    for (e <- Externs if e.withLift) {
      stream.print(" with Lift" + e.opsGrp.grp.name)
    }
    stream.print(" {")
    stream.println("  this: " + dsl + " =>")
    stream.println("}")
    stream.println()
    stream.println()
  
    // dsl interface    
    stream.println("trait " + dsl + " extends Base")
    for (opsGrp <- opsGrps) {
      stream.print(" with " + opsGrp.name)
    }
    for (e <- Externs) {
      stream.print(" with " + e.opsGrp.name)
    }    
    stream.println(" { this: " + dsl + "Application => ")
    stream.println()
    emitBlockComment("abstract types", stream)
    for (tpe <- Tpes) {
      if (OpsGrp.contains(tpe) && !isPrimitiveType(tpe)) {
        stream.println("  type " + quote(tpe))      
        stream.println("  implicit def m_" + tpe.name + makeTpeParsWithBounds(tpe.tpePars) + ": Manifest[" + quote(tpe) + "]")
      }
    }    
    stream.println("}")
    stream.println()  
  }

}  
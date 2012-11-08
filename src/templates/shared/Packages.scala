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

  def emitScalaPackageDefinitionsBase(appOps: List[LMSOps], compOps: List[LMSOps], stream: PrintWriter) {
    val allOps = appOps union compOps
  
    emitBlockComment("packages", stream)
  
    // Lift
    stream.println("trait " + dsl + "Lift")
    val liftOps = appOps.filter(_.lift.isDefined)
    if (liftOps.length > 0) {
      stream.print("  extends " + liftOps.head.lift.get)
    }
    for (op <- liftOps.drop(1)) {
      stream.print(" with " + op.lift.get)
    }
    stream.print(" {")
    stream.println("  this: " + dsl + " =>")
    stream.println("}")
    stream.println()
    stream.println()
  
    // Scala ops included in app
    val opsPkg = dsl + "ScalaOpsPkg"
    stream.println("trait " + opsPkg + " extends Base")
    for (op <- appOps) {
      stream.print(" with " + op.name)
    }
    stream.println()
    stream.println()    
  }  

  def emitDSLPackageDefinitionsBase(dslOps: List[Ops], lmsCompOps: List[LMSOps], stream: PrintWriter) {
    emitBlockComment("dsl definition", stream)
  
    // dsl interface    
    stream.println("trait " + dsl + " extends " + dsl + "ScalaOpsPkg")
    for (op <- dslOps) {
      stream.print(" with " + op.name)
    }
    stream.println(" { this: " + dsl + "Application => ")
    stream.println()
    emitBlockComment("abstract types", stream)
    for ((tpe,ops) <- OpsGrp) {
      stream.println("  type " + quote(tpe))      
      stream.println("  implicit def m_" + tpe.name + makeTpeArgsWithBounds(tpe.tpeArgs) + ": Manifest[" + quote(tpe) + "]")
    }    
    stream.println("}")
    stream.println()  
  }

}  
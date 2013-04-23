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

  def emitDSLPackageDefinitions(opsGrps: List[DSLOps], stream: PrintWriter) {
    emitBlockComment("dsl library definition", stream)
    
    // base trait sets Rep[T] to T and mixes in the necessary portions of the front-end, without
    // bringing the abstract impls in scope, since they can cause a recursive loop in the library    
    stream.println("trait " + dsl + "Base extends Base with OverloadHack {")
    stream.println("  type Rep[+T] = T")
    stream.println("  protected def unit[T:Manifest](x: T) = x")
    stream.println()
    stream.println("}")    
    stream.println()
    
    stream.println("trait DeliteCompatibility extends Base {")
    emitBlockComment("functionality that exists in Delite for DeliteArrays that we need an equivalent library implementation for", stream, indent=2)
    stream.println("  type DeliteArray[T] = Array[T]")
    // TODO: switch back to darray_copy when we implement data(..) to generate Delite structs instead of concrete back-end structs
    // stream.println("  def darray_copy[T:Manifest](src: Rep[Array[T]], srcPos: Rep[Int], dest: Rep[Array[T]], destPos: Rep[Int], size: Rep[Int]): Rep[Unit]")
    stream.println("  def darray_unsafe_copy[T:Manifest](src: Rep[Array[T]], srcPos: Rep[Int], dest: Rep[Array[T]], destPos: Rep[Int], size: Rep[Int]): Rep[Unit]")
    stream.println("  def darray_new[T:Manifest](n: Rep[Int]): Rep[Array[T]]")        
    stream.println("}")    
    stream.println()
    
    // compiler ops mixes in an application ops with compiler only ops
    stream.println("trait " + dsl + "CompilerOps extends " + dsl + "Application with DeliteCompatibility")
    for (opsGrp <- opsGrps) {
      if (opsGrp.ops.exists(_.style == compiler))
        stream.print(" with " + opsGrp.grp.name + "CompilerOps")      
    }      
    stream.println()
    stream.println()
    
    // library impl brings all of the library types in scope
    stream.println("trait " + dsl + "Lib extends " + dsl + "Base with " + dsl + "CompilerOps with " + dsl + "Classes {")
    stream.println("  this: " + dsl + "Application => ")
    stream.println()
    stream.println("  // override required due to mix-in")
    stream.println("  override type Rep[+T] = T")
    stream.println()
    emitBlockComment("dsl types", stream, indent=2)
    for (tpe <- Tpes) {
      if (!isTpeInst(tpe) && !isForgePrimitiveType(tpe)) {
        stream.println("  def m_" + tpe.name + makeTpeParsWithBounds(tpe.tpePars) + " = manifest[" + quote(tpe) + "]")      
      }
    }        
    stream.println()
    emitBlockComment("delite compatibility implementations", stream, indent=2)
    // stream.println("  def darray_copy[T:Manifest](src: Array[T], srcPos: Int, dest: Array[T], destPos: Int, size: Int): Unit = {")
    stream.println("  def darray_unsafe_copy[T:Manifest](src: Array[T], srcPos: Int, dest: Array[T], destPos: Int, size: Int): Unit = {")
    stream.println("    System.arraycopy(src,srcPos,dest,destPos,size)")
    stream.println("  }")    
    stream.println("  def darray_new[T:Manifest](n: Int): Array[T] = new Array[T](n)")        
    stream.println("}")
  }  
}
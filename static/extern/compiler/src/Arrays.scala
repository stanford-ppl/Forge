package LOWERCASE_DSL_NAME.compiler.extern

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._

// For compiler (Delite) implementation
trait ForgeArrayOpsExp extends DeliteArrayFatExp {
  this: DeliteOpsExp =>
  
  type ForgeArray[T] = DeliteArray[T]
  implicit def forgeArrayManifest[T:Manifest] = manifest[DeliteArray[T]]
  
  def array_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[T]] 
    = darray_new[T](__arg0)
  def array_copy[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[ForgeArray[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
    = darray_copy(__arg0,__arg1,__arg2,__arg3,__arg4)
  def array_update[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
    = darray_update(__arg0,__arg1,__arg2)
  def array_apply[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
    = darray_apply(__arg0,__arg1)
  def array_length[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[Int]          
    = darray_length(__arg0)
  def array_asimmutable[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = object_unsafe_immutable(__arg0)
      
  // avoid mixing in LMS Array ops due to conflicts. alternatively, we could refactor LMS array ops to 
  // put ArrayApply in an isolated trait that we can use.
  case class ArrayApply[T:Manifest](a: Exp[Array[T]], n: Exp[Int]) extends Def[T]
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case ArrayApply(a,x) => scala_array_apply(f(a),f(x))(mtype(manifest[A]),pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
  
  def scala_array_apply[T:Manifest](__arg0: Rep[Array[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T] 
    = ArrayApply(__arg0,__arg1)
}
trait ScalaGenForgeArrayOps extends ScalaGenDeliteArrayOps with ScalaGenPrimitiveOps with ScalaGenObjectOps { 
  val IR: ForgeArrayOpsExp with DeliteOpsExp 
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayApply(x,n) => emitValDef(sym, "" + quote(x) + "(" + quote(n) + ")")
    case _ => super.emitNode(sym,rhs)
  }
}
trait CLikeGenForgeArrayOps extends CLikeGenBase {  
  val IR: ForgeArrayOpsExp with DeliteOpsExp 
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayApply(x,n) => emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case _ => super.emitNode(sym, rhs)
  }  
}
trait CudaGenForgeArrayOps extends CudaGenDeliteArrayOps with CLikeGenForgeArrayOps with CudaGenPrimitiveOps with CudaGenObjectOps { val IR: ForgeArrayOpsExp with DeliteOpsExp }
trait OpenCLGenForgeArrayOps extends OpenCLGenDeliteArrayOps with CLikeGenForgeArrayOps with OpenCLGenPrimitiveOps with OpenCLGenObjectOps { val IR: ForgeArrayOpsExp with DeliteOpsExp }
trait CGenForgeArrayOps extends CGenDeliteArrayOps with CGenPrimitiveOps with CGenObjectOps { 
  val IR: ForgeArrayOpsExp with DeliteOpsExp 
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayApply(x,n) => emitValDef(sym, quote(x) + "->apply(" + quote(n) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

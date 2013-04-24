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
}
trait ScalaGenForgeArrayOps extends ScalaGenDeliteArrayOps with ScalaGenPrimitiveOps with ScalaGenObjectOps { val IR: DeliteArrayFatExp with DeliteOpsExp }
trait CudaGenForgeArrayOps extends CudaGenDeliteArrayOps with CudaGenPrimitiveOps with CudaGenObjectOps { val IR: DeliteArrayFatExp with DeliteOpsExp }
trait OpenCLGenForgeArrayOps extends OpenCLGenDeliteArrayOps with OpenCLGenPrimitiveOps with OpenCLGenObjectOps { val IR: DeliteArrayFatExp with DeliteOpsExp }
trait CGenForgeArrayOps extends CGenDeliteArrayOps with CGenPrimitiveOps with CGenObjectOps { val IR: DeliteArrayFatExp with DeliteOpsExp }

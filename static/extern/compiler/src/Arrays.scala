package LOWERCASE_DSL_NAME.compiler

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Config

// For compiler (Delite) implementation
trait ForgeArrayOpsExp extends DeliteArrayFatExp {
  this: DeliteOpsExp =>

  type ForgeArray[T] = DeliteArray[T]
  implicit def forgeArrayManifest[T:Manifest] = manifest[DeliteArray[T]]

  def array_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = darray_new[T](__arg0)
  def array_empty_imm[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = darray_new_immutable[T](__arg0)
  def array_copy[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[ForgeArray[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
    = darray_copy(__arg0,__arg1,__arg2,__arg3,__arg4)
  def array_update[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
    = darray_update(__arg0,__arg1,__arg2)
  def array_apply[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
    = darray_apply(__arg0,__arg1)
  def array_length[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[Int]
    = darray_length(__arg0)
  def array_clone[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]] = {
    val out = darray_new[T](darray_length(__arg0))
    darray_copy(__arg0, unit(0), out, unit(0), darray_length(__arg0))
    out.unsafeImmutable
  }
  def array_sort[T:Manifest:Ordering](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = darray_sort(__arg0)
  def array_fromseq[T:Manifest](__arg0: Seq[Rep[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]] = {
    val out = darray_new[T](unit(__arg0.length))
    for (i <- 0 until __arg0.length) {
      out(unit(i)) = __arg0(i)
    }
    out.unsafeImmutable
  }
  def array_string_split(__arg0: Rep[String], __arg1: Rep[String])(implicit __imp0: SourceContext): Rep[ForgeArray[String]]
    = reflectPure(ArrayStringSplit(__arg0, __arg1))

  // avoid mixing in LMS Array ops due to conflicts. alternatively, we could refactor LMS array ops to
  // put ArrayApply and ArrayLength in an isolated trait that we can use.
  case class ArrayApply[T:Manifest](a: Exp[Array[T]], n: Exp[Int]) extends Def[T]
  case class ArrayLength[T:Manifest](a: Exp[Array[T]]) extends Def[Int]
  case class ArrayStringSplit(str: Exp[String], split: Exp[String]) extends Def[DeliteArray[String]]

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case ArrayApply(a,x) => scala_array_apply(f(a),f(x))(mtype(manifest[A]),pos)
    case ArrayLength(a) => scala_array_length(f(a))(mtype(manifest[A]),pos)
    case ArrayStringSplit(a,b) => array_string_split(f(a),f(b))(pos)

    case Reflect(ArrayApply(a,x), u, es) => reflectMirrored(Reflect(ArrayApply(f(a),f(x))(mtype(manifest[A])), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(ArrayLength(a), u, es) => reflectMirrored(Reflect(ArrayLength(f(a))(mtype(manifest[A])), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(ArrayStringSplit(a,b), u, es) => reflectMirrored(Reflect(ArrayStringSplit(f(a),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

  def scala_array_apply[T:Manifest](__arg0: Rep[Array[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
    = ArrayApply(__arg0,__arg1)
  def scala_array_length[T:Manifest](__arg0: Rep[Array[T]])(implicit __imp0: SourceContext): Rep[Int]
    = ArrayLength(__arg0)
}
trait ScalaGenForgeArrayOps extends ScalaGenDeliteArrayOps with ScalaGenPrimitiveOps with ScalaGenObjectOps {
  val IR: ForgeArrayOpsExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayApply(x,n) => emitValDef(sym, "" + quote(x) + "(" + quote(n) + ")")
    case ArrayLength(x) => emitValDef(sym, "" + quote(x) + ".length")
    case ArrayStringSplit(a,b) if Config.generateSerializable => emitValDef(sym, "new ppl.delite.runtime.data.LocalDeliteArrayObject[String](" + quote(a) + ".split(" + quote(b) + "))")
    case ArrayStringSplit(a,b) => emitValDef(sym, quote(a) + ".split(" + quote(b) + ")")
    case _ => super.emitNode(sym,rhs)
  }
}
trait CLikeGenForgeArrayOps extends CLikeGenBase {
  val IR: ForgeArrayOpsExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayApply(x,n) => emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case ArrayLength(x) => emitValDef(sym, quote(x) + ".length")
    case _ => super.emitNode(sym, rhs)
  }
}
trait CudaGenForgeArrayOps extends CudaGenDeliteArrayOps with CLikeGenForgeArrayOps with CudaGenObjectOps { val IR: ForgeArrayOpsExp with DeliteOpsExp }
trait OpenCLGenForgeArrayOps extends OpenCLGenDeliteArrayOps with CLikeGenForgeArrayOps with OpenCLGenObjectOps { val IR: ForgeArrayOpsExp with DeliteOpsExp }
trait CGenForgeArrayOps extends CGenDeliteArrayOps with CGenObjectOps {
  val IR: ForgeArrayOpsExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayApply(x,n) => emitValDef(sym, quote(x) + "->apply(" + quote(n) + ")")
    case ArrayLength(x) => emitValDef(sym, quote(x) + "->length")
    case _ => super.emitNode(sym, rhs)
  }
}


trait ForgeArrayBufferOpsExp extends DeliteArrayBufferOpsExp {
  this: DeliteArrayOpsExpOpt with DeliteOpsExp =>

  type ForgeArrayBuffer[T] = DeliteArrayBuffer[T]

  def array_buffer_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
    = darray_buffer_new[T](__arg0)
  def array_buffer_copy[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int],__arg2: Rep[ForgeArrayBuffer[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
    = darray_copy(darray_buffer_raw_data(asDeliteArrayBuffer(__arg0)), __arg1, darray_buffer_raw_data(asDeliteArrayBuffer(__arg2)), __arg3, __arg4)
  def array_buffer_update[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
    = darray_buffer_update(__arg0,__arg1,__arg2)
  def array_buffer_apply[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
    = darray_buffer_apply(__arg0,__arg1)
  def array_buffer_length[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]])(implicit __imp0: SourceContext): Rep[Int]
    = darray_buffer_length(__arg0)
  def array_buffer_set_length[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
    = darray_buffer_set_length(__arg0,__arg1)
  def array_buffer_append[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
    = darray_buffer_append(__arg0,__arg1)
  def array_buffer_indexof[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T])(implicit __imp0: SourceContext): Rep[Int]
    = throw new UnsupportedOperationException("DeliteArrayBuffer indexOf not implemented yet")
}
trait ScalaGenForgeArrayBufferOps extends ScalaGenDeliteArrayBufferOps with ScalaGenOrderingOps with ScalaGenPrimitiveOps with ScalaGenObjectOps { val IR: DeliteArrayBufferOpsExp with DeliteOpsExp }
trait CudaGenForgeArrayBufferOps extends CudaGenDeliteArrayBufferOps with CudaGenOrderingOps with CudaGenPrimitiveOps with CudaGenObjectOps { val IR: DeliteArrayBufferOpsExp with DeliteOpsExp }
trait OpenCLGenForgeArrayBufferOps // TODO
trait CGenForgeArrayBufferOps extends /*CGenDeliteArrayBufferOps with */CGenOrderingOps with CGenPrimitiveOps with CGenObjectOps { val IR: DeliteArrayBufferOpsExp with DeliteOpsExp }


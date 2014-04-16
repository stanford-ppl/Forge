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
  this: DeliteOpsExp with ForgeHashMapOpsExp =>

  type ForgeArray[T] = DeliteArray[T]
  implicit def forgeArrayManifest[T:Manifest] = manifest[DeliteArray[T]]

  def farray_from_sarray[T:Manifest](__arg0: Rep[Array[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = darray_fromfunction(scala_array_length(__arg0), i => scala_array_apply(__arg0,i))
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
  def array_take[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int]): Rep[ForgeArray[T]]
    = darray_take(__arg0,__arg1)
  def array_map[T:Manifest,R:Manifest](__arg0: Rep[ForgeArray[T]], __arg1: Rep[T] => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArray[R]]
    = darray_map(__arg0,__arg1)
  //def array_flatmap[T:Manifest,R:Manifest](__arg0: Rep[ForgeArray[T]], __arg1: Rep[T] => Rep[ForgeArray[R]])(implicit __imp0: SourceContext): Rep[ForgeArray[R]]
  //  = darray_flatmap(__arg0,__arg1)
  def array_zip[T:Manifest,B:Manifest,R:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[ForgeArray[B]], __arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArray[R]]
    = darray_zipwith(__arg0,__arg1,__arg2)
  def array_reduce[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: (Rep[T],Rep[T]) => Rep[T],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[T]
    = darray_reduce(__arg0,__arg1,__arg2)
  def array_groupByReduce[T:Manifest,K:Manifest,V:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[T] => Rep[K], __arg2: Rep[T] => Rep[V], __arg3: (Rep[V],Rep[V]) => Rep[V])(implicit __imp0: SourceContext): Rep[ForgeHashMap[K,V]]
    = darray_groupByReduce(__arg0,__arg1,__arg2,__arg3)
  def array_filter[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[T] => Rep[Boolean])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = darray_filter(__arg0,__arg1)
  def array_sort[T:Manifest:Ordering](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = darray_sort(__arg0)
  def array_fromfunction[T:Manifest](__arg0: Rep[Int],__arg1: Rep[Int] => Rep[T])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = darray_fromfunction(__arg0,__arg1)
  def array_fromseq[T:Manifest](__arg0: Seq[Rep[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]] = {
    val out = darray_new[T](unit(__arg0.length))
    for (i <- 0 until __arg0.length) {
      out(unit(i)) = __arg0(i)
    }
    out.unsafeImmutable
  }
  def array_string_split(__arg0: Rep[String],__arg1: Rep[String],__arg2: Rep[Int] = unit(0))(implicit __imp0: SourceContext): Rep[ForgeArray[String]]
    = reflectPure(ArrayStringSplit(__arg0, __arg1, __arg2))

  // avoid mixing in LMS Array ops due to conflicts. alternatively, we could refactor LMS array ops to
  // put ArrayApply and ArrayLength in an isolated trait that we can use.
  case class ArrayApply[T:Manifest](a: Exp[Array[T]], n: Exp[Int]) extends Def[T]
  case class ArrayLength[T:Manifest](a: Exp[Array[T]]) extends Def[Int]
  case class ArrayStringSplit(str: Exp[String], split: Exp[String], lim: Exp[Int]) extends Def[DeliteArray[String]]

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case ArrayApply(a,x) => scala_array_apply(f(a),f(x))(mtype(manifest[A]),pos)
    case ArrayLength(a) => scala_array_length(f(a))(mtype(manifest[A]),pos)
    case ArrayStringSplit(a,b,l) => array_string_split(f(a),f(b),f(l))(pos)

    case Reflect(ArrayApply(a,x), u, es) => reflectMirrored(Reflect(ArrayApply(f(a),f(x))(mtype(manifest[A])), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(ArrayLength(a), u, es) => reflectMirrored(Reflect(ArrayLength(f(a))(mtype(manifest[A])), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(ArrayStringSplit(a,b,l), u, es) => reflectMirrored(Reflect(ArrayStringSplit(f(a),f(b),f(l)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
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
    // TODO
    case ArrayStringSplit(a,b,l) if Config.generateSerializable => emitValDef(sym, "new ppl.delite.runtime.data.LocalDeliteArrayObject[String](" + quote(a) + ".split(" + quote(b) + "))")
    case ArrayStringSplit(a,b,l) => emitValDef(sym, quote(a) + ".split(" + quote(b) + ", " + quote(l) + ")")
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
    //TODO: enable ArrayStringSplit in cluster mode
    case ArrayStringSplit(a,b,Const(0)) if (!Config.generateSerializable) => emitValDef(sym, "string_split(" + quote(a) + "," + quote(b) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}


trait ForgeArrayBufferOpsExp extends DeliteArrayBufferOpsExp {
  this: ForgeArrayOpsExp with DeliteArrayOpsExpOpt with DeliteOpsExp with DeliteMapOpsExp with ForgeHashMapOpsExp =>

  type ForgeArrayBuffer[T] = DeliteArrayBuffer[T]
  implicit def forgeArrayBufferManifest[T:Manifest] = manifest[DeliteArrayBuffer[T]]

  def array_buffer_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
    = darray_buffer_new[T](__arg0)
  def array_buffer_immutable[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
    = darray_buffer_immutable[T](__arg0)
  def array_buffer_strict_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
    = darray_buffer_new[T](__arg0,__arg0)
  def array_buffer_new_imm[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
    = darray_buffer_new_imm[T](__arg0,array_length(__arg0))
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
  def array_buffer_result[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = darray_buffer_result(__arg0)
  def array_buffer_map[T:Manifest,R:Manifest](__arg0: Rep[ForgeArrayBuffer[T]], __arg1: Rep[T] => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[R]]
    = darray_buffer_map(__arg0,__arg1)
  def array_buffer_flatmap[T:Manifest,R:Manifest](__arg0: Rep[ForgeArrayBuffer[T]], __arg1: Rep[T] => Rep[ForgeArrayBuffer[R]])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[R]]
    = darray_buffer_flatmap(__arg0,__arg1)
  def array_buffer_groupBy[T:Manifest,K:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T] => Rep[K])(implicit __imp0: SourceContext): Rep[ForgeHashMap[K,ForgeArrayBuffer[T]]]
    = darray_buffer_groupBy(__arg0,__arg1)
  def array_buffer_zip[T:Manifest,B:Manifest,R:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[ForgeArrayBuffer[B]], __arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[R]]
    = darray_buffer_zip(__arg0,__arg1,__arg2)
  def array_buffer_reduce[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: (Rep[T],Rep[T]) => Rep[T],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[T]
    = darray_buffer_reduce(__arg0,__arg1,__arg2)
  def array_buffer_groupByReduce[T:Manifest,K:Manifest,V:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T] => Rep[K], __arg2: Rep[T] => Rep[V], __arg3: (Rep[V],Rep[V]) => Rep[V])(implicit __imp0: SourceContext): Rep[ForgeHashMap[K,V]]
    = darray_buffer_groupByReduce(__arg0,__arg1,__arg2,__arg3)
  def array_buffer_filter[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T] => Rep[Boolean])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
    = darray_buffer_filter(__arg0,__arg1)
  def array_buffer_foreach[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T] => Rep[Unit])(implicit __imp0: SourceContext): Rep[Unit]
    = darray_buffer_foreach(__arg0,__arg1)
  def array_buffer_forIndices[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int] => Rep[Unit])(implicit __imp0: SourceContext): Rep[Unit]
    = darray_buffer_forIndices(__arg0,__arg1)
}
trait ScalaGenForgeArrayBufferOps extends ScalaGenDeliteArrayBufferOps with ScalaGenOrderingOps with ScalaGenPrimitiveOps with ScalaGenObjectOps { val IR: DeliteArrayBufferOpsExp with DeliteOpsExp }
trait CudaGenForgeArrayBufferOps extends CudaGenDeliteArrayBufferOps with CudaGenOrderingOps with CudaGenPrimitiveOps with CudaGenObjectOps { val IR: DeliteArrayBufferOpsExp with DeliteOpsExp }
trait OpenCLGenForgeArrayBufferOps // TODO
trait CGenForgeArrayBufferOps extends /*CGenDeliteArrayBufferOps with */CGenOrderingOps with CGenPrimitiveOps with CGenObjectOps { val IR: DeliteArrayBufferOpsExp with DeliteOpsExp }


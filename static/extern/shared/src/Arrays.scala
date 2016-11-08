package LOWERCASE_DSL_NAME.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack

// Front-end
trait ForgeArrayOps extends Base with OverloadHack {
  this: ForgeHashMapOps =>

  /**
   * We use ForgeArray[T] instead of T so we don't get any subtle errors related to shadowing Array[T]
   */
  type ForgeArray[T]
  implicit def forgeArrayManifest[T:Manifest]: Manifest[ForgeArray[T]]

  /**
   * Applications may need direct access to ForgeArrays, if, for example, they use string fsplit
   * How do we allow DSLs to only optionally include the Array API for end users?
   */
  implicit class ForgeArrayOps[T:Manifest](x: Rep[ForgeArray[T]]) {
    def apply(n: Rep[Int])(implicit ctx: SourceContext) = array_apply(x,n)
    def length = array_length(x)
  }

  def array_apply[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
  def array_length[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[Int]
  def array_empty_imm[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]

  /* Required for apps to be able use 'args' */
  implicit class ScalaArrayOps[T:Manifest](x: Rep[Array[T]]) {
    def apply(n: Rep[Int])(implicit ctx: SourceContext) = scala_array_apply(x,n)
    def length = scala_array_length(x)
  }
  // the implicit class method 'length' is not working for an unknown reason, possibly related to the problem mentioned in ForgeArrayCompilerOps below
  // omitting SourceContext is a hacky way to avoid conflicts with Forge DSLs without using an arbitrary Overloaded parameter
  def infix_length[T:Manifest](x: Rep[Array[T]])(implicit __imp0: SourceContext, o:Overloaded1) = scala_array_length(x)
  def scala_array_apply[T:Manifest](x: Rep[Array[T]], n: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
  def scala_array_length[T:Manifest](x: Rep[Array[T]])(implicit __imp0: SourceContext): Rep[Int]
}
trait ForgeArrayCompilerOps extends ForgeArrayOps {
  this: ForgeHashMapCompilerOps =>

  /**
   * There are some unfortunate scalac typer crashes when we try to use the nicer front-end from DSLs :(
   */
  object Array {
    def empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext) = array_empty[T](__arg0)
    def copy[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[ForgeArray[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __imp0: SourceContext) = array_copy(__arg0,__arg1,__arg2,__arg3,__arg4)
  }

  implicit class ForgeArrayCompilerOps[T:Manifest](x: Rep[ForgeArray[T]]) {
    def update(n: Rep[Int], y: Rep[T])(implicit ctx: SourceContext) = array_update(x,n,y)
    def map[R:Manifest](f: Rep[T] => Rep[R])(implicit ctx: SourceContext) = array_map[T,R](x,f)
    def Clone(implicit ctx: SourceContext) = array_clone(x)
  }

  def farray_from_sarray[T:Manifest](__arg0: Rep[Array[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
  def array_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
  def array_raw_alloc[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[T]] = array_empty[T](__arg1)
  def array_copy[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[ForgeArray[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
  def array_update[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
  def array_clone[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
  def array_soft_clone[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
  def array_take[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int]): Rep[ForgeArray[T]]
  def array_mkstring[A:Manifest](__arg0: Rep[ForgeArray[A]],__arg1: Rep[String])(implicit __imp0: SourceContext): Rep[String]
  def array_map[T:Manifest,R:Manifest](__arg0: Rep[ForgeArray[T]], __arg1: Rep[T] => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArray[R]]
  def array_flatmap[T:Manifest,R:Manifest](__arg0: Rep[ForgeArray[T]], __arg1: Rep[T] => Rep[ForgeArray[R]])(implicit __imp0: SourceContext): Rep[ForgeArray[R]]
  def array_zip[T:Manifest,B:Manifest,R:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[ForgeArray[B]], __arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArray[R]]
  def array_groupByReduce[T:Manifest,K:Manifest,V:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[T] => Rep[K], __arg2: Rep[T] => Rep[V], __arg3: (Rep[V],Rep[V]) => Rep[V])(implicit __imp0: SourceContext): Rep[ForgeHashMap[K,V]]
  def array_reduce[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: (Rep[T],Rep[T]) => Rep[T],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[T]
  def array_filter[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[T] => Rep[Boolean])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
  def array_sort[T:Manifest:Ordering](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
  def array_fromfunction[T:Manifest](__arg0: Rep[Int],__arg1: Rep[Int] => Rep[T])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
  def array_fromseq[T:Manifest](__arg0: Seq[Rep[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
  def array_string_split(__arg0: Rep[String],__arg1: Rep[String],__arg2: Rep[Int] = unit(0))(implicit __imp0: SourceContext): Rep[ForgeArray[String]]
  def array_sortIndices[R:Manifest:Ordering](__arg0: Rep[Int], __arg1: (Rep[Int] => Rep[R]))(implicit __imp0: SourceContext): Rep[ForgeArray[Int]]
  def array_sortIndices(__arg0: Rep[Int], __arg1: (Rep[Int],Rep[Int]) => Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[Int]]

  def scala_array_apply[T:Manifest](__arg0: Rep[Array[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
  def scala_array_length[T:Manifest](__arg0: Rep[Array[T]])(implicit __imp0: SourceContext): Rep[Int]
}

trait ForgeArrayBufferOps extends Base {
  type ForgeArrayBuffer[T]
  implicit def forgeArrayBufferManifest[T:Manifest]: Manifest[ForgeArrayBuffer[T]]
}

trait ForgeArrayBufferCompilerOps extends ForgeArrayBufferOps {
  this: ForgeArrayCompilerOps with ForgeHashMapCompilerOps =>

  def array_buffer_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
  def array_buffer_immutable[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
  def array_buffer_strict_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
  def array_buffer_new_imm[T:Manifest](__arg0: Rep[ForgeArray[T]], __arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
  def array_buffer_raw_alloc[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]] = array_buffer_empty[T](__arg1)
  def array_buffer_apply[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
  def array_buffer_length[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]])(implicit __imp0: SourceContext): Rep[Int]
  def array_buffer_copy[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int],__arg2: Rep[ForgeArrayBuffer[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
  def array_buffer_update[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
  def array_buffer_set_length[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
  def array_buffer_appendable[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]])(implicit __imp0: SourceContext): Rep[Boolean] = unit(true)
  def array_buffer_append[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
  def array_buffer_dcappend[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[Unit] = array_buffer_append(__arg0,__arg2)
  def array_buffer_indexof[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T])(implicit __imp0: SourceContext): Rep[Int]
  def array_buffer_result[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
  def array_buffer_unsafe_result[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
  def array_buffer_map[T:Manifest,R:Manifest](__arg0: Rep[ForgeArrayBuffer[T]], __arg1: Rep[T] => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[R]]
  def array_buffer_flatmap[T:Manifest,R:Manifest](__arg0: Rep[ForgeArrayBuffer[T]], __arg1: Rep[T] => Rep[ForgeArrayBuffer[R]])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[R]]
  def array_buffer_groupBy[T:Manifest,K:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T] => Rep[K])(implicit __imp0: SourceContext): Rep[ForgeHashMap[K,ForgeArrayBuffer[T]]]
  def array_buffer_zip[T:Manifest,B:Manifest,R:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[ForgeArrayBuffer[B]], __arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[R]]
  def array_buffer_reduce[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: (Rep[T],Rep[T]) => Rep[T],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[T]
  def array_buffer_groupByReduce[T:Manifest,K:Manifest,V:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T] => Rep[K], __arg2: Rep[T] => Rep[V], __arg3: (Rep[V],Rep[V]) => Rep[V])(implicit __imp0: SourceContext): Rep[ForgeHashMap[K,V]]
  def array_buffer_filter[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T] => Rep[Boolean])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
  def array_buffer_foreach[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T] => Rep[Unit])(implicit __imp0: SourceContext): Rep[Unit]
  def array_buffer_forIndices[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int] => Rep[Unit])(implicit __imp0: SourceContext): Rep[Unit]
  def array_buffer_fromfunction[T:Manifest](__arg0: Rep[Int], __arg1: Rep[Int] => Rep[T])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
}

package LOWERCASE_DSL_NAME.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack

// Front-end
trait ForgeMultiArrayOps extends Base with OverloadHack {
  this: ForgeHashMapOps =>

  type ForgeMultiArray[T]
  implicit def forgeMultiArrayManifest[T:Manifest]: Manifest[ForgeMultiArray[T]]

  /**
   * Applications may need direct access to MultiArrays, if, for example, they use string fsplit
   * How do we allow DSLs to only optionally include the Array API for end users?
   */
  implicit class ForgeMultiArrayOps[T:Manifest](ma: Rep[ForgeMultiArray[T]]) {
    def apply(i: Rep[Int]*)(implicit ctx: SourceContext) = multia_apply(x,i)
    def length = multia_size(x)
  }
  def multia_apply[T:Manifest](ma: Rep[ForgeMultiArray[T]],i: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[T]
  def multia_size[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[Int]
}
trait ForgeMultiArrayCompilerOps extends ForgeMultiArrayOps {
  this: ForgeHashMapCompilerOps =>

  object MultiArray {
    def empty[T:Manifest](dims: Rep[Int]*)(implicit ctx: SourceContext) = multia_empty[T](dims)
    def copy[T:Manifest](dest: Rep[ForgeMultiArray[T]],destPos: Rep[Int],src: Rep[ForgeMultiArray[T]],srcPos: Rep[Int],len: Rep[Int])(implicit ctx: SourceContext) = multia_copy(dest,Seq(destPos),src,Seq(srcPos),Seq(len))
  }

  implicit class ForgeMultiArrayCompilerOps[T:Manifest](ma: Rep[ForgeMultiArray[T]]) {
    def update(i: Seq[Rep[Int]], x: Rep[T])(implicit ctx: SourceContext) = multia_update(ma,i,x)    
    def map[R:Manifest](f: Rep[T] => Rep[R])(implicit ctx: SourceContext) = multia_map[T,R](ma,f)
    def Clone(implicit ctx: SourceContext) = multia_clone(ma)
  }

  // --- Constructors
  def multia_empty[T:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_empty_imm[T:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_fromfunction[T:Manifest](dims: Seq[Rep[Int]], func: Rep[Seq[Rep[Int]]] => Rep[T])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_from_sarray[T:Manifest](x: Rep[Array[T]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  
  // --- Properties
  def multia_rank[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[Int]
  def multia_shape[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[Seq[Rep[Int]]]

  // --- Single element operators
  def multia_update[T:Manifest](ma: Rep[ForgeMultiArray[T]],i: Seq[Rep[Int]],x: Rep[T])(implicit ctx: SourceContext): Rep[Unit]
  
  // --- Copies / Mutating / Permuting
  def multia_clone[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_permute[T:Manifest](ma: Rep[ForgeMultiArray[T]], config: Seq[Int])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_copy[T:Manifest](dest: Rep[ForgeMultiArray[T]], destOfs: Seq[Rep[Int]], src: Rep[ForgeMultiArray[T]], srcOfs: Seq[Rep[Int]], lens: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[Unit]

  // --- Parallel ops
  def multia_map[T:Manifest,R:Manifest](in: Rep[ForgeMultiArray[T]], func: Rep[T] => Rep[R])(implicit ctx: SourceContext): Rep[ForgeMultiArray[R]]
  //def array_flatmap[T:Manifest,R:Manifest](__arg0: Rep[ForgeArray[T]], __arg1: Rep[T] => Rep[ForgeArray[R]])(implicit __imp0: SourceContext): Rep[ForgeArray[R]]
  def multia_zipwith[T:Manifest,B:Manifest,R:Manifest](inA: Rep[ForgeMultiArray[T]],inB: Rep[ForgeMultiArray[B]], func: (Rep[T],Rep[B]) => Rep[R])(implicit ctx: SourceContext): Rep[ForgeMultiArray[R]]
  def multia_groupByReduce[T:Manifest,K:Manifest,V:Manifest](in: Rep[ForgeMultiArray[T]],key: Rep[T] => Rep[K], value: Rep[T] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext): Rep[ForgeHashMap[K,V]]
  def multia_reduce[T:Manifest](in: Rep[ForgeMultiArray[T]], func: (Rep[T],Rep[T]) => Rep[T], zero: Rep[T])(implicit __imp0: SourceContext): Rep[T]
  def multia_filter[T:Manifest](in: Rep[ForgeMultiArray[T]], cond: Rep[T] => Rep[Boolean])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  
  def multia_mmap[T:Manifest](in: Rep[ForgeMultiArray[T]], func: Rep[T] => Rep[T])(implicit ctx: SourceContext): Rep[Unit]
  def multia_mzipwith[T:Manifest](inA: Rep[ForgeMultiArray[T]], inB: Rep[ForgeMultiArray[T]], func: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[Unit]

  // --- 1D Operations
  def multia_fromseq[T:Manifest](sq: Seq[Rep[T]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_string_split(str: Rep[String], pat: Rep[String], ofs: Rep[Int] = unit(0))(implicit ctx: SourceContext): Rep[ForgeMultiArray[String]]

  def multia_mkstring[T:Manifest](ma: Rep[ForgeMultiArray[T]], del: Rep[String])(implicit ctx: SourceContext): Rep[String]
  def multia_sort[T:Manifest:Ordering](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_sortwith[T:Manifest](ma: Rep[ForgeMultiArray[T]], comp: (Rep[T],Rep[T]) => Rep[Int])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  // note: version of sortIndices used for DSL (slightly different in Delite)
  def multia_sortIndices[R:Manifest:Ordering](len: Rep[Int], comp: (Rep[Int] => Rep[R]))(implicit ctx: SourceContext): Rep[ForgeMultiArray[Int]]

  // --- 2D Operations
  def multia_matmult[T:Manifest:Numeric](lhs: Rep[ForgeMultiArray[T]], rhs: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]

  // --- Removed
  // This can probably be implemented at the DSL level (hopefully without loss of performance?)
  // native_array.take(n) 
  //  versus 
  // DeliteMultiArrayView(ma, start = Seq(0), stride = Seq(1), dims = Seq(n)).toDense
  //def array_take[T:Manifest](ma: Rep[ForgeArray[T]], n: Rep[Int]): Rep[ForgeArray[T]]
  
  // TBD: What was the purpose of this?
  //def array_raw_alloc[T:Manifest](x: Rep[ForgeArray[T]], len: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[T]] = array_empty[T](len)
}

/*
  // Added:
  def array_buffer_raw_alloc[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]] = array_buffer_empty[T](__arg1)
  def array_buffer_apply[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
  def array_buffer_length[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]])(implicit __imp0: SourceContext): Rep[Int]
  def array_buffer_copy[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int],__arg2: Rep[ForgeArrayBuffer[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
  def array_buffer_update[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
  def array_buffer_new_imm[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
  def array_buffer_map[T:Manifest,R:Manifest](__arg0: Rep[ForgeArrayBuffer[T]], __arg1: Rep[T] => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[R]]
  def array_buffer_flatmap[T:Manifest,R:Manifest](__arg0: Rep[ForgeArrayBuffer[T]], __arg1: Rep[T] => Rep[ForgeArrayBuffer[R]])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[R]]
  def array_buffer_groupBy[T:Manifest,K:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T] => Rep[K])(implicit __imp0: SourceContext): Rep[ForgeHashMap[K,ForgeArrayBuffer[T]]]
  def array_buffer_zip[T:Manifest,B:Manifest,R:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[ForgeArrayBuffer[B]], __arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[R]]
  def array_buffer_reduce[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: (Rep[T],Rep[T]) => Rep[T],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[T]
  def array_buffer_groupByReduce[T:Manifest,K:Manifest,V:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T] => Rep[K], __arg2: Rep[T] => Rep[V], __arg3: (Rep[V],Rep[V]) => Rep[V])(implicit __imp0: SourceContext): Rep[ForgeHashMap[K,V]]
  def array_buffer_filter[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T] => Rep[Boolean])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
  def array_buffer_foreach[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T] => Rep[Unit])(implicit __imp0: SourceContext): Rep[Unit]
  def array_buffer_forIndices[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int] => Rep[Unit])(implicit __imp0: SourceContext): Rep[Unit]
  def array_buffer_append[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
      
  // Do we need?
  def array_buffer_strict_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
  
  // Probably don't need?
  def array_buffer_result[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
  def array_buffer_unsafe_result[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
  def array_buffer_set_length[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
  
  // Almost definitely don't need
  def array_buffer_appendable[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]])(implicit __imp0: SourceContext): Rep[Boolean] = unit(true)
  def array_buffer_indexof[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T])(implicit __imp0: SourceContext): Rep[Int]
  def array_buffer_dcappend[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[Unit] = array_buffer_append(__arg0,__arg2)
  */

trait ForgeMultiArrayViewOps extends Base {
  type ForgeMultiArrayView[T]

  implicit def forgeMultiArrayViewManifest[T:Manifest]: Manifest[ForgeMultiArrayView[T]]

  implicit class ForgeMultiArrayViewOps[T:Manifest](ma: Rep[ForgeMultiArrayView[T]]) {
    def apply(i: Rep[Int]*)(implicit ctx: SourceContext) = multia_view_apply(x,i)
    def length = multia_view_size(x)
  }
  def multia_view_apply[T:Manifest](v: Rep[ForgeMultiArrayView[T]], i: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[T]
  def multia_view_size[T:Manifest](v: Rep[ForgeMultiArrayView[T]])(implicit ctx: SourceContext): Rep[Int]
}

trait ForgeMultiArrayViewCompilerOps extends ForgeMultiArrayViewOps {
  this: ForgeMultiArrayCompilerOps with ForgeHashMapCompilerOps =>

  implicit class ForgeMultiArrayViewCompilerOps[T:Manifest](v: Rep[ForgeMultiArrayView[T]]) {
    def update(i: Seq[Rep[Int]], x: Rep[T])(implicit ctx: SourceContext) = multia_view_update(v,i,x)    
    def map[R:Manifest](f: Rep[T] => Rep[R])(implicit ctx: SourceContext) = multia_view_map[T,R](v,f)
    //def Clone(implicit ctx: SourceContext) = multia_view_todense(v)
    def toDense(implicit ctx: SourceContext) = multia_view_todense(v)
  }

  // --- View creation
  def multia_view_new[T:Manifest](ma: Rep[ForgeMultiArray[T]], start: Seq[Rep[Int]], stride: Seq[Rep[Int]], dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ForgeMultiArrayView[T]]
  def multia_view_new_imm[T:Manifest](ma: Rep[ForgeMultiArray[T]], start: Seq[Rep[Int]], stride: Seq[Rep[Int]], dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ForgeMultiArrayView[T]]

  // --- View properties
  def multia_view_rank[A:Manifest](v: Rep[ForgeMultiArrayView[A]])(implicit ctx: SourceContext): Rep[Int]
  def multia_view_shape[A:Manifest](v: Rep[ForgeMultiArrayView[A]])(implicit ctx: SourceContext): Rep[Seq[Rep[Int]]]

  // --- Single element ops
  def multia_view_update[A:Manifest](v: Rep[ForgeMultiArrayView[A]], i: Seq[Rep[Int]], x: Rep[A])(implicit ctx: SourceContext): Rep[Unit]

  // --- Copy
  def multia_view_todense[A:Manifest](v: Rep[ForgeMultiArrayView[A]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[A]]

  // --- Parallel ops 
  def multia_view_map[A:Manifest,B:Manifest](v: Rep[ForgeMultiArrayView[A]], f: Rep[A] => Rep[B])(implicit ctx: SourceContext): Rep[ForgeMultiArray[A]]
  def multia_view_zipwith[A:Manifest,B:Manifest,R:Manifest](v1: Rep[ForgeMultiArrayView[A]], v2: Rep[ForgeMultiArray[B]], f: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext): Rep[ForgeMultiArray[R]]
  def multia_view_reduce[A:Manifest](v: Rep[ForgeMultiArrayView[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A])(implicit ctx: SourceContext): Rep[A]
  def multia_view_foreach[A:Manifest](v: Rep[ForgeMultiArrayView[A]], f: Rep[A] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]

  def multia_view_mmap[A:Manifest](v: Rep[ForgeMultiArrayView[A]], f: Rep[T] => Rep[R])(implicit ctx: SourceContext): Rep[Unit]
  def multia_view_mzipwith[A:Manifest](v: Rep[ForgeMultiArrayView[A]], rhs: Rep[ForgeMultiArray[A]], f: (Rep[A],Rep[A]) => Rep[A])(implicit ctx: SourceContext): Rep[Unit] 
  def multia_view_copy[A:Manifest](dest: Rep[ForgeMultiArrayView[A]], src: Rep[ForgeMultiArrayView[A]])(implicit ctx: SourceContext): Rep[Unit]

}
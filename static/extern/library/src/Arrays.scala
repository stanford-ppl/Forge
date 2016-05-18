package LOWERCASE_DSL_NAME.library

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

trait ForgeArrayWrapper extends HUMAN_DSL_NAMEBase {
  this: ForgeHashMapWrapper =>

  type ForgeArray[T] = scala.Array[T]
  implicit def forgeArrayManifest[T:Manifest] = manifest[Array[T]]

  def farray_from_sarray[T:Manifest](__arg0: Rep[Array[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = __arg0
  def array_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = new ForgeArray[T](__arg0)
  def array_empty_imm[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = array_empty[T](__arg0)
  def array_copy[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[ForgeArray[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
    = System.arraycopy(__arg0,__arg1,__arg2,__arg3,__arg4)
  def array_update[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
    = __arg0(__arg1) = __arg2
  def array_apply[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
    = __arg0(__arg1)
  def array_length[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[Int]
    = __arg0.length
  def array_clone[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = __arg0.clone
  def array_soft_clone[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = array_clone(__arg0)
  def array_take[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int]): Rep[ForgeArray[T]]
    = __arg0.take(__arg1)
  def array_mkstring[A:Manifest](__arg0: Rep[ForgeArray[A]],__arg1: Rep[String])(implicit __imp0: SourceContext): Rep[String]
    = __arg0.mkString(__arg1)
  def array_map[T:Manifest,R:Manifest](__arg0: Rep[ForgeArray[T]], __arg1: Rep[T] => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArray[R]]
    = __arg0.map(__arg1)
  def array_flatmap[T:Manifest,R:Manifest](__arg0: Rep[ForgeArray[T]], __arg1: Rep[T] => Rep[ForgeArray[R]])(implicit __imp0: SourceContext): Rep[ForgeArray[R]]
    = __arg0.flatMap(e => __arg1(e))
  def array_zip[T:Manifest,B:Manifest,R:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[ForgeArray[B]], __arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArray[R]]
    = __arg0.zip(__arg1).map(t => __arg2(t._1,t._2))
  def array_reduce[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: (Rep[T],Rep[T]) => Rep[T],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[T]
    = if (array_length(__arg0) == 0) __arg2 else __arg0.reduce(__arg1)
  def array_groupByReduce[T:Manifest,K:Manifest,V:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[T] => Rep[K], __arg2: Rep[T] => Rep[V], __arg3: (Rep[V],Rep[V]) => Rep[V])(implicit __imp0: SourceContext): Rep[ForgeHashMap[K,V]] = {
    val grp = __arg0.groupBy[K](__arg1)
    val m = scala.collection.mutable.HashMap[K,V]()
    grp.foreach{ a =>
      m.put(a._1,array_map(a._2,__arg2).reduce(__arg3))
    }
    m
  }
  def array_filter[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[T] => Rep[Boolean])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = __arg0.filter(__arg1)
  def array_sort[T:Manifest:Ordering](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]] = {
    val d = array_empty[T](__arg0.length)
    array_copy(__arg0,0,d,0,__arg0.length)
    scala.util.Sorting.quickSort(d)
    d
  }
  def array_sortIndices[R:Manifest:Ordering](__arg0: Rep[Int], __arg1: (Rep[Int] => Rep[R]))(implicit __imp0: SourceContext): Rep[ForgeArray[Int]]
    = array_empty[Int](__arg0).indices.toArray.sortBy(__arg1)
  def array_sortIndices(__arg0: Rep[Int], __arg1: (Rep[Int],Rep[Int]) => Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[Int]]
    = array_empty[Int](__arg0).indices.toArray.sortWith((a,b) => __arg1(a,b) < 0)
  def array_fromfunction[T:Manifest](__arg0: Rep[Int],__arg1: Rep[Int] => Rep[T])(implicit __imp0: SourceContext): Rep[ForgeArray[T]] = {
    Array.tabulate[T](__arg0)(__arg1)
  }
  def array_fromseq[T:Manifest](__arg0: Seq[Rep[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = __arg0.toArray
  def array_string_split(__arg0: Rep[String],__arg1: Rep[String],__arg2: Rep[Int] = unit(0))(implicit __imp0: SourceContext): Rep[ForgeArray[String]]
    = __arg0.split(__arg1,__arg2)

  def scala_array_apply[T:Manifest](__arg0: Rep[Array[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
    = array_apply(__arg0,__arg1)
  def scala_array_length[T:Manifest](__arg0: Rep[Array[T]])(implicit __imp0: SourceContext): Rep[Int]
    = array_length(__arg0)
}

trait ForgeArrayBufferWrapper extends HUMAN_DSL_NAMEBase {
  this: ForgeArrayWrapper with ForgeHashMapWrapper =>

  type ForgeArrayBuffer[T] = scala.collection.mutable.ArrayBuffer[T]
  implicit def forgeArrayBufferManifest[T:Manifest] = manifest[ForgeArrayBuffer[T]]

  def array_buffer_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
    = new scala.collection.mutable.ArrayBuffer[T]()
  def array_buffer_immutable[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
    = __arg0
  def array_buffer_strict_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
    = (new scala.collection.mutable.ArrayBuffer[T]()) ++ (new Array[T](__arg0))
  def array_buffer_new_imm[T:Manifest](__arg0: Rep[ForgeArray[T]], __arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]] = {
    assert(__arg0.length >= __arg1, "array_buffer_new_imm requires the initial array to be at least as large as the array buffer size")
    val out = new scala.collection.mutable.ArrayBuffer[T](__arg1)
    for (i <- 0 until __arg1) {
      out += __arg0(i)
    }
    out
  }
  def array_buffer_copy[T:Manifest](src: Rep[ForgeArrayBuffer[T]], srcPos: Rep[Int], dest: Rep[ForgeArrayBuffer[T]], destPos: Rep[Int], length: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit] = {
    for (i <- 0 until length) {
      dest(destPos+i) = src(srcPos+i)
    }
  }
  def array_buffer_update[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
    = __arg0(__arg1) = __arg2
  def array_buffer_apply[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
    = __arg0(__arg1)
  def array_buffer_length[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]])(implicit __imp0: SourceContext): Rep[Int]
    = __arg0.length
  def array_buffer_set_length[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
    = __arg0.slice(0,__arg1)
  def array_buffer_append[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
    = { __arg0 += __arg1 }
  def array_buffer_indexof[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T])(implicit __imp0: SourceContext): Rep[Int]
    = __arg0.indexOf(__arg1)
  def array_buffer_result[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = __arg0.toArray
  def array_buffer_unsafe_result[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = __arg0.toArray
  def array_buffer_map[T:Manifest,R:Manifest](__arg0: Rep[ForgeArrayBuffer[T]], __arg1: Rep[T] => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[R]]
    = __arg0.map(__arg1)
  def array_buffer_flatmap[T:Manifest,R:Manifest](__arg0: Rep[ForgeArrayBuffer[T]], __arg1: Rep[T] => Rep[ForgeArrayBuffer[R]])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[R]]
    = __arg0.flatMap(__arg1)
  def array_buffer_groupBy[T:Manifest,K:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T] => Rep[K])(implicit __imp0: SourceContext): Rep[ForgeHashMap[K,ForgeArrayBuffer[T]]] ={
    val grp = __arg0.groupBy[K](__arg1)
    (new scala.collection.mutable.HashMap[K,ForgeArrayBuffer[T]]()) ++ (grp)
  }
  def array_buffer_zip[T:Manifest,B:Manifest,R:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[ForgeArrayBuffer[B]], __arg2: (Rep[T],Rep[B]) => Rep[R])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[R]]
    = __arg0.zip(__arg1).map(t => __arg2(t._1,t._2))
  def array_buffer_reduce[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: (Rep[T],Rep[T]) => Rep[T],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[T]
    = if (array_buffer_length(__arg0) == 0) __arg2 else __arg0.reduce(__arg1)
  def array_buffer_groupByReduce[T:Manifest,K:Manifest,V:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T] => Rep[K], __arg2: Rep[T] => Rep[V], __arg3: (Rep[V],Rep[V]) => Rep[V])(implicit __imp0: SourceContext): Rep[ForgeHashMap[K,V]] = {
    val grp = __arg0.groupBy[K](__arg1)
    val hm = scala.collection.mutable.HashMap[K,V]()
    grp.foreach{ a =>
      hm.put(a._1,array_buffer_map(a._2,__arg2).reduce(__arg3))
    }
    hm
  }
  def array_buffer_filter[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T] => Rep[Boolean])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]]
    = __arg0.filter(__arg1)
  def array_buffer_foreach[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[T] => Rep[Unit])(implicit __imp0: SourceContext): Rep[Unit]
    = __arg0.foreach(__arg1)
  def array_buffer_forIndices[T:Manifest](__arg0: Rep[ForgeArrayBuffer[T]],__arg1: Rep[Int] => Rep[Unit])(implicit __imp0: SourceContext): Rep[Unit] = {
    var i = 0
    while(i < array_buffer_length(__arg0)){
      __arg1(i)
      i += 1
    }
  }
  def array_buffer_fromfunction[T:Manifest](__arg0: Rep[Int], __arg1: Rep[Int] => Rep[T])(implicit __imp0: SourceContext): Rep[ForgeArrayBuffer[T]] = {
    scala.collection.mutable.ArrayBuffer.tabulate[T](__arg0)(__arg1)
  }
}





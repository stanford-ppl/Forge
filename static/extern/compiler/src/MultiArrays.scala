package LOWERCASE_DSL_NAME.compiler

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Config

// For compiler (Delite) implementation
trait ForgeMultiArrayOpsExp extends DeliteMultiArrayOpsExp {
  this: DeliteOpsExp with ForgeMultiMapOpsExp =>

  type ForgeMultiArray[T] = DeliteMultiArray[T]
  type ForgeArray1D[T] = DeliteArray1D[T]
  type ForgeArray2D[T] = DeliteArray2D[T]
  type ForgeArray3D[T] = DeliteArray3D[T]
  type ForgeArray4D[T] = DeliteArray4D[T]
  type ForgeArray5D[T] = DeliteArray5D[T]

  // --- implicit manifests
  implicit def forgeMultiArrayManifest[T:Manifest] = manifest[DeliteMultiArray[T]]
  implicit def forgeArray1DManifest[T:Manifest] = manifest[DeliteArray1D[T]]
  implicit def forgeArray2DManifest[T:Manifest] = manifest[DeliteArray2D[T]]
  implicit def forgeArray3DManifest[T:Manifest] = manifest[DeliteArray3D[T]]
  implicit def forgeArray4DManifest[T:Manifest] = manifest[DeliteArray4D[T]]
  implicit def forgeArray5DManifest[T:Manifest] = manifest[DeliteArray5D[T]]

  // --- inheritance casts
  implicit def forgeArray1DToMultiArray[T:Manifest](ma: Rep[ForgeArray1D[T]]) = ma.asInstanceOf[Rep[ForgeMultiArray[T]]]
  implicit def forgeArray2DToMultiArray[T:Manifest](ma: Rep[ForgeArray2D[T]]) = ma.asInstanceOf[Rep[ForgeMultiArray[T]]]
  implicit def forgeArray3DToMultiArray[T:Manifest](ma: Rep[ForgeArray3D[T]]) = ma.asInstanceOf[Rep[ForgeMultiArray[T]]]
  implicit def forgeArray4DToMultiArray[T:Manifest](ma: Rep[ForgeArray4D[T]]) = ma.asInstanceOf[Rep[ForgeMultiArray[T]]]
  implicit def forgeArray5DToMultiArray[T:Manifest](ma: Rep[ForgeArray5D[T]]) = ma.asInstanceOf[Rep[ForgeMultiArray[T]]]

  // --- Rank type casts
  def multia_as_1D[T:Manifest](ma: Rep[ForgeMultiArray[T]]) = dmultia_as_1D(ma)
  def multia_as_2D[T:Manifest](ma: Rep[ForgeMultiArray[T]]) = dmultia_as_2D(ma)
  def multia_as_3D[T:Manifest](ma: Rep[ForgeMultiArray[T]]) = dmultia_as_3D(ma)
  def multia_as_4D[T:Manifest](ma: Rep[ForgeMultiArray[T]]) = dmultia_as_4D(ma)
  def multia_as_5D[T:Manifest](ma: Rep[ForgeMultiArray[T]]) = dmultia_as_5D(ma)

  // --- Constructors
  def multia_empty[T:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_empty_imm[T:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_fromfunction[T:Manifest](dims: Seq[Rep[Int]], func: Rep[Seq[Rep[Int]]] => Rep[T])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  //def multia_from_sarray[T:Manifest](x: Rep[Array[T]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  
  def multia_view[T:Manifest](ma: Rep[ForgeMultiArray[T]], start: Seq[Rep[Int]], stride: Seq[Rep[Int]], dims: Seq[Rep[Int]]): Rep[ForgeMultiArray[T]]

  // --- Properties
  def multia_rank[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[Int]
  def multia_shape[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[Seq[Rep[Int]]]
  def multia_size[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[Int]

  // --- Single element operators
  def multia_apply[T:Manifest](ma: Rep[ForgeMultiArray[T]],i: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[T]
  def multia_update[T:Manifest](ma: Rep[ForgeMultiArray[T]],i: Seq[Rep[Int]],x: Rep[T])(implicit ctx: SourceContext): Rep[Unit]
  
  // --- Copies / Mutating / Permuting
  def multia_clone[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_permute[T:Manifest](ma: Rep[ForgeMultiArray[T]], config: Seq[Int])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_reshape[T:Manifest](ma: Rep[ForgeMultiArray[T]], shape: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]

  // --- Parallel ops
  def multia_map[T:Manifest,R:Manifest](in: Rep[ForgeMultiArray[T]], func: Rep[T] => Rep[R])(implicit ctx: SourceContext): Rep[ForgeMultiArray[R]]
  def multia_zipwith[T:Manifest,B:Manifest,R:Manifest](inA: Rep[ForgeMultiArray[T]],inB: Rep[ForgeMultiArray[B]], func: (Rep[T],Rep[B]) => Rep[R])(implicit ctx: SourceContext): Rep[ForgeMultiArray[R]]
  def multia_reduce[T:Manifest](in: Rep[ForgeMultiArray[T]], func: (Rep[T],Rep[T]) => Rep[T], zero: Rep[T])(implicit ctx: SourceContext): Rep[T]

  def multia_mmap[T:Manifest](in: Rep[ForgeMultiArray[T]], func: Rep[T] => Rep[T])(implicit ctx: SourceContext): Rep[Unit]
  def multia_mzipwith[T:Manifest](inA: Rep[ForgeMultiArray[T]], inB: Rep[ForgeMultiArray[T]], func: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[Unit]

  // --- Bulk
  def multia_mkstring[T:Manifest](ma: Rep[ForgeMultiArray[T]], del: Rep[String])(implicit ctx: SourceContext): Rep[String]
  
  // --- 1D Operations
  def multia_sort[T:Manifest:Ordering](ma: Rep[ForgeArray1D[T]])(implicit ctx: SourceContext): Rep[ForgeArray1D[T]] = {
    val indices = multia_sortIndices(ma.size, (i: Rep[Int]) => ma(i))
    indices.map{i => ma(i)}
  }
  def multia_sortwith[T:Manifest](ma: Rep[ForgeArray1D[T]], comp: (Rep[T],Rep[T]) => Rep[Int])(implicit ctx: SourceContext): Rep[ForgeArray1D[T]] 
  def multia_sortIndices[R:Manifest:Ordering](len: Rep[Int], comp: (Rep[Int] => Rep[R]))(implicit ctx: SourceContext): Rep[ForgeArray1D[Int]]

  def multia_flatmap[T:Manifest,R:Manifest](in: Rep[ForgeArray1D[T]], func: Rep[T] => Rep[ForgeArray1D[R]])(implicit ctx: SourceContext): Rep[ForgeArray1D[R]]
  def multia_filter[T:Manifest](in: Rep[ForgeArray1D[T]], cond: Rep[T] => Rep[Boolean])(implicit ctx: SourceContext): Rep[ForgeArray1D[T]]
  def multia_groupBy[T:Manifest,K:Manifest](in: Rep[ForgeArray1D[T]], key: Rep[T] => Rep[K])(implicit ctx: SourceContext): Rep[ForgeMultiMap[K,ForgeArray1D[T]]]
  def multia_groupByReduce[T:Manifest,K:Manifest,V:Manifest](in: Rep[ForgeArray1D[T]],key: Rep[T] => Rep[K], value: Rep[T] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext): Rep[ForgeMultiMap[K,V]]

  def multia_fromseq[T:Manifest](sq: Seq[Rep[T]])(implicit ctx: SourceContext): Rep[ForgeArray1D[T]]
  def multia_string_split(str: Rep[String], pat: Rep[String], ofs: Rep[Int] = unit(0))(implicit ctx: SourceContext): Rep[ForgeArray1D[String]]

  // --- 2D Operations
  def multia_matmult[T:Manifest:Numeric](lhs: Rep[ForgeArray2D[T]], rhs: Rep[ForgeArray2D[T]])(implicit ctx: SourceContext): Rep[ForgeArray2D[T]]
  def multia_matvecmult[T:Manifest:Numeric](lhs: Rep[ForgeArray2D[T]], rhs: Rep[ForgeArray1D[T]])(implicit ctx: SourceContext): Rep[ForgeArray1D[T]]
}

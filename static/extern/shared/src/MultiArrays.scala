package LOWERCASE_DSL_NAME.shared

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

// Front-end

trait ForgeLayouts {
  type FLayout[T,R]
}

trait ForgeMultiArrayOps extends Base with ForgeLayouts {
  type ForgeMultiArray[T]
  type ForgeArray1D[T]
  type ForgeArray2D[T]
  type ForgeArray3D[T]
  type ForgeArray4D[T]
  type ForgeArray5D[T]

  // --- implicit manifests
  implicit def forgeMultiArrayManifest[T:Manifest]: Manifest[ForgeMultiArray[T]]
  implicit def forgeArray1DManifest[T:Manifest]: Manifest[ForgeArray1D[T]]
  implicit def forgeArray2DManifest[T:Manifest]: Manifest[ForgeArray2D[T]]
  implicit def forgeArray3DManifest[T:Manifest]: Manifest[ForgeArray3D[T]]
  implicit def forgeArray4DManifest[T:Manifest]: Manifest[ForgeArray4D[T]]
  implicit def forgeArray5DManifest[T:Manifest]: Manifest[ForgeArray5D[T]]

  // --- inheritance casts
  implicit def forgeArray1DToMultiArray[T:Manifest](ma: Rep[ForgeArray1D[T]]): Rep[ForgeMultiArray[T]]
  implicit def forgeArray2DToMultiArray[T:Manifest](ma: Rep[ForgeArray2D[T]]): Rep[ForgeMultiArray[T]]
  implicit def forgeArray3DToMultiArray[T:Manifest](ma: Rep[ForgeArray3D[T]]): Rep[ForgeMultiArray[T]]
  implicit def forgeArray4DToMultiArray[T:Manifest](ma: Rep[ForgeArray4D[T]]): Rep[ForgeMultiArray[T]]
  implicit def forgeArray5DToMultiArray[T:Manifest](ma: Rep[ForgeArray5D[T]]): Rep[ForgeMultiArray[T]]

  /**
   * Applications may need direct access to MultiArrays, if, for example, they use string fsplit
   * How do we allow DSLs to only optionally include the Array API for end users?
   */
  implicit class ForgeMultiArrayOps[T:Manifest](ma: Rep[ForgeMultiArray[T]]) {
    def apply(i: Rep[Int]*)(implicit ctx: SourceContext) = multia_apply(x,i)
    def size = multia_size(x)
  }
  def multia_apply[T:Manifest](ma: Rep[ForgeMultiArray[T]],i: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[T]
  def multia_size[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[Int]
}

trait ForgeMultiArrayCompilerOps extends ForgeMultiArrayOps {
  this: ForgeMultiMapCompilerOps with ForgeArrayCompilerOps =>

  object MultiArray {
    def empty[T:Manifest](dims: Rep[Int]*)(implicit ctx: SourceContext) = multia_empty[T](dims)
  }

  implicit class ForgeMultiArrayCompilerOps[T:Manifest](ma: Rep[ForgeMultiArray[T]]) {
    // --- rank casts
    def as1D: Rep[ForgeArray1D[T]] = multia_as_1D(ma)
    def as2D: Rep[ForgeArray2D[T]] = multia_as_2D(ma)
    def as3D: Rep[ForgeArray3D[T]] = multia_as_3D(ma)
    def as4D: Rep[ForgeArray4D[T]] = multia_as_4D(ma)
    def as5D: Rep[ForgeArray5D[T]] = multia_as_5D(ma)

    def update(i: Seq[Rep[Int]], x: Rep[T])(implicit ctx: SourceContext) = multia_update(ma,i,x)    
    def map[R:Manifest](f: Rep[T] => Rep[R])(implicit ctx: SourceContext) = multia_map[T,R](ma,f)
    def Clone(implicit ctx: SourceContext) = multia_clone(ma)
  }

  // --- Rank type casts
  def multia_as_1D[T:Manifest](ma: Rep[ForgeMultiArray[T]]): Rep[ForgeArray1D[T]]
  def multia_as_2D[T:Manifest](ma: Rep[ForgeMultiArray[T]]): Rep[ForgeArray2D[T]]
  def multia_as_3D[T:Manifest](ma: Rep[ForgeMultiArray[T]]): Rep[ForgeArray3D[T]]
  def multia_as_4D[T:Manifest](ma: Rep[ForgeMultiArray[T]]): Rep[ForgeArray4D[T]]
  def multia_as_5D[T:Manifest](ma: Rep[ForgeMultiArray[T]]): Rep[ForgeArray5D[T]]

  // --- Constructors
  def multia_empty[T:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_empty_imm[T:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_fromfunction[T:Manifest](dims: Seq[Rep[Int]], func: Rep[Seq[Rep[Int]]] => Rep[T])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]

  def multia_view[T:Manifest](ma: Rep[ForgeMultiArray[T]], start: Seq[Rep[Int]], stride: Seq[Rep[Int]], dims: Seq[Rep[Int]]): Rep[ForgeMultiArray[T]]

  // --- Properties
  def multia_rank[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[Int]
  def multia_shape[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[Seq[Rep[Int]]]

  // --- Single element operators
  def multia_update[T:Manifest](ma: Rep[ForgeMultiArray[T]],i: Seq[Rep[Int]],x: Rep[T])(implicit ctx: SourceContext): Rep[Unit]
  
  // --- Copies / Mutating / Permuting
  def multia_clone[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_permute[T:Manifest](ma: Rep[ForgeMultiArray[T]], config: Seq[Int])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
  def multia_reshape[T:Manifest](ma: Rep[ForgeMultiArray[T]], shape: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]

  // --- Parallel ops
  def multia_map[T:Manifest,R:Manifest](ma: Rep[ForgeMultiArray[T]], func: Rep[T] => Rep[R])(implicit ctx: SourceContext): Rep[ForgeMultiArray[R]]
  def multia_zipwith[T:Manifest,B:Manifest,R:Manifest](ma: Rep[ForgeMultiArray[T]],mb: Rep[ForgeMultiArray[B]], func: (Rep[T],Rep[B]) => Rep[R])(implicit ctx: SourceContext): Rep[ForgeMultiArray[R]]
  def multia_reduce[T:Manifest](ma: Rep[ForgeMultiArray[T]], func: (Rep[T],Rep[T]) => Rep[T], zero: Rep[T])(implicit ctx: SourceContext): Rep[T]
  def multia_forindices[A:Manifest](ma: Rep[ForgeMultiArray[A]], f: Rep[Seq[Rep[Int]]] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
  def multia_foreach[A:Manifest](ma: Rep[ForgeMultiArray[A]], f: Rep[A] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]

  def multia_mmap[T:Manifest](ma: Rep[ForgeMultiArray[T]], func: Rep[T] => Rep[T])(implicit ctx: SourceContext): Rep[Unit]
  def multia_mzipwith[T:Manifest](ma: Rep[ForgeMultiArray[T]], mb: Rep[ForgeMultiArray[T]], func: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[Unit]

  def multia_NDmap[A:Manifest,B:Manifest](ma: Rep[ForgeMultiArray[A]], mdims: Seq[Int], func: Rep[ForgeMultiArray[A]] => Rep[ForgeMultiArray[B]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[B]]

  // --- Buffer ops
  def multia_NDinsert[A:Manifest](ma: Rep[ForgeMultiArray[A]], rhs: Rep[ForgeMultiArray[A]], axis: Int, index: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def multia_NDappend[A:Manifest](ma: Rep[ForgeMultiArray[A]], rhs: Rep[ForgeMultiArray[A]], axis: Int)(implicit ctx: SourceContext): Rep[Unit]

  def multia_insertAll[A:Manifest](ma: Rep[ForgeMultiArray[A]], rhs: Rep[ForgeMultiArray[A]], axis: Int, index: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def multia_appendAll[A:Manifest](ma: Rep[ForgeMultiArray[A]], rhs: Rep[ForgeMultiArray[A]], axis: Int)(implicit ctx: SourceContext): Rep[Unit]

  // --- Misc
  def multia_mkstring[T:Manifest](ma: Rep[ForgeMultiArray[T]], dels: Seq[Rep[String]])(implicit ctx: SourceContext): Rep[String]
  def multia_string_split(str: Rep[String], pat: Rep[String], ofs: Rep[Int] = unit(0))(implicit ctx: SourceContext): Rep[ForgeArray1D[String]]

  /* Possible alternative? 

  def multia_string_split(str: Exp[String], pat: Exp[String], ofs: Exp[Int] = unit(0))(implicit ctx: SourceContext) = {
    val result = array_string_split(str,pat,ofs)
    multia_unpin(result, FlatLayout[String](1), Seq(array_length(result))).as1D
  }
  */

  // --- 1D Operations
  def multia_sort[T:Manifest:Ordering](ma: Rep[ForgeArray1D[T]])(implicit ctx: SourceContext): Rep[ForgeArray1D[T]] = {
    val indices = multia_sortIndices(ma.size, {i => ma(i)})
    indices.map{i => ma(i)}
  }
  def multia_sortwith[T:Manifest](ma: Rep[ForgeArray1D[T]], comp: (Rep[T],Rep[T]) => Rep[Int])(implicit ctx: SourceContext): Rep[ForgeArray1D[T]] 
  def multia_sortIndices[R:Manifest:Ordering](len: Rep[Int], func: (Rep[Int] => Rep[R]))(implicit ctx: SourceContext): Rep[ForgeArray1D[Int]]

  def multia_flatmap[T:Manifest,R:Manifest](ma: Rep[ForgeArray1D[T]], func: Rep[T] => Rep[ForgeArray1D[R]])(implicit ctx: SourceContext): Rep[ForgeArray1D[R]]
  def multia_filter[T:Manifest](ma: Rep[ForgeArray1D[T]], cond: Rep[T] => Rep[Boolean])(implicit ctx: SourceContext): Rep[ForgeArray1D[T]]
  def multia_groupBy[T:Manifest,K:Manifest](ma: Rep[ForgeArray1D[T]], key: Rep[T] => Rep[K])(implicit ctx: SourceContext): Rep[ForgeMultiMap[K,ForgeArray1D[T]]]
  def multia_groupByReduce[T:Manifest,K:Manifest,V:Manifest](ma: Rep[ForgeArray1D[T]],key: Rep[T] => Rep[K], value: Rep[T] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext): Rep[ForgeMultiMap[K,V]]

  def multia_fromseq[T:Manifest](sq: Seq[Rep[T]])(implicit ctx: SourceContext): Rep[ForgeArray1D[T]]

  def multia_insert[A:Manifest](ma: Rep[ForgeArray1D[A]], x: Rep[A], index: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def multia_append[A:Manifest](ma: Rep[ForgeArray1D[A]], x: Rep[A])(implicit ctx: SourceContext): Rep[Unit]

  // --- 2D Operations
  def multia_matmult[T:Manifest:Numeric](lhs: Rep[ForgeArray2D[T]], rhs: Rep[ForgeArray2D[T]])(implicit ctx: SourceContext): Rep[ForgeArray2D[T]]
  def multia_matvecmult[T:Manifest:Numeric](mat: Rep[ForgeArray2D[T]], vec: Rep[ForgeArray1D[T]])(implicit ctx: SourceContext): Rep[ForgeArray1D[T]]

  // --- Pinning
  def multia_pin[T:Manifest,R:Manifest](ma: Rep[ForgeMultiArray[T]], layout: FLayout[T,R])(implicit ctx: SourceContext): Rep[ForgeArray[R]]
  def multia_unpin[T:Manifest,R:Manifest](in: Rep[ForgeArray[R]], layout: FLayout[T,R], shape: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ForgeMultiArray[T]]
}

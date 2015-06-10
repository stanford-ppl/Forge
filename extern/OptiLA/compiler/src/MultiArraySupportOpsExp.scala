package optila.compiler

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

import ppl.delite.framework.datastructures._
import ppl.delite.framework.ops._

trait ForgeIndicesOpsExp extends IndicesOpsExp { 
	type ForgeAbstractIndices = AbstractIndices
  type ForgeLoopIndices = LoopIndices
  type ForgeIndices = Indices

  def findices_new(xs: Seq[Rep[Int]]): Rep[ForgeIndices] = indices_new(xs)

  implicit def manifestAbstInds: Manifest[ForgeAbstractIndices] = manifest[AbstractIndices]
  implicit def manifestLoopInds: Manifest[ForgeLoopIndices] = manifest[LoopIndices]
  implicit def manifestInds: Manifest[ForgeIndices] = manifest[Indices]

  implicit def loopInds_to_abstInds(inds: Rep[ForgeLoopIndices]) = inds.asInstanceOf[Rep[ForgeAbstractIndices]]
  implicit def inds_to_abstInds(inds: Rep[ForgeIndices]) = inds.asInstanceOf[Rep[ForgeAbstractIndices]]
}

// For compiler (Delite) implementation
trait MultiArraySupportOpsExp extends DeliteMultiArrayOpsExp with ForgeIndicesOpsExp { 
	this: DeliteOpsExp  =>

  type ArrayMD[T] = DeliteMultiArray[T]
  type Array1D[T] = DeliteArray1D[T]
  type Array2D[T] = DeliteArray2D[T]
  type Array3D[T] = DeliteArray3D[T]
  type Array4D[T] = DeliteArray4D[T]
  type Array5D[T] = DeliteArray5D[T]
  type ForgeMap[K,V] = DeliteMultiMap[K,V]

  // Implicit manifests
  implicit def manifestMD[T:Manifest] = manifest[DeliteMultiArray[T]]
  implicit def manifest1D[T:Manifest] = manifest[DeliteArray1D[T]]
  implicit def manifest2D[T:Manifest] = manifest[DeliteArray2D[T]]
  implicit def manifest3D[T:Manifest] = manifest[DeliteArray3D[T]]
  implicit def manifest4D[T:Manifest] = manifest[DeliteArray4D[T]]
  implicit def manifest5D[T:Manifest] = manifest[DeliteArray5D[T]]
  implicit def manifestForgeMap[K:Manifest,V:Manifest] = manifest[ForgeMap[K,V]]

  // Implicit casts
  implicit def f1D_to_fND[T:Manifest](ma: Rep[Array1D[T]]) = ma.asInstanceOf[Rep[ArrayMD[T]]]
  implicit def f2D_to_fND[T:Manifest](ma: Rep[Array2D[T]]) = ma.asInstanceOf[Rep[ArrayMD[T]]]
  implicit def f3D_to_fND[T:Manifest](ma: Rep[Array3D[T]]) = ma.asInstanceOf[Rep[ArrayMD[T]]]
  implicit def f4D_to_fND[T:Manifest](ma: Rep[Array4D[T]]) = ma.asInstanceOf[Rep[ArrayMD[T]]]
  implicit def f5D_to_fND[T:Manifest](ma: Rep[Array5D[T]]) = ma.asInstanceOf[Rep[ArrayMD[T]]]

  // --- MultiMap
  def fmulmap_size[K:Manifest,V:Manifest](fm: Rep[ForgeMap[K,V]])(implicit ctx: SourceContext): Rep[Int]
    = dmulmap_size(fm)
  def fmulmap_get[K:Manifest,V:Manifest](fm: Rep[ForgeMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[V]
    = dmulmap_get(fm, key)
  def fmulmap_contains[K:Manifest,V:Manifest](fm: Rep[ForgeMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[Boolean]
    = dmulmap_contains(fm, key)
  def fmulmap_keys[K:Manifest,V:Manifest](fm: Rep[ForgeMap[K,V]])(implicit ctx: SourceContext): Rep[Array1D[K]]
    = dmulmap_keys(fm)
  def fmulmap_values[K:Manifest,V:Manifest](fm: Rep[ForgeMap[K,V]])(implicit ctx: SourceContext): Rep[Array1D[V]]
    = dmulmap_values(fm)
  def fmulmap_from_1d_arrays[K:Manifest,V:Manifest](keys: Rep[Array1D[K]], vals: Rep[Array1D[V]])(implicit ctx: SourceContext): Rep[ForgeMap[K,V]]
    = dmulmap_from_1d_arrays(keys, vals)

  // --- Rank type casts
  def fmultia_as_1D[T:Manifest](ma: Rep[ArrayMD[T]]) = dmultia_as_1D(ma)
  def fmultia_as_2D[T:Manifest](ma: Rep[ArrayMD[T]]) = dmultia_as_2D(ma)
  def fmultia_as_3D[T:Manifest](ma: Rep[ArrayMD[T]]) = dmultia_as_3D(ma)
  def fmultia_as_4D[T:Manifest](ma: Rep[ArrayMD[T]]) = dmultia_as_4D(ma)
  def fmultia_as_5D[T:Manifest](ma: Rep[ArrayMD[T]]) = dmultia_as_5D(ma)

 // --- File reading/writing
  def fmultia_readfile[A:Manifest](path: Rep[String], dels: Seq[Rep[String]], func: Rep[String] => Rep[A])(implicit ctx: SourceContext): Rep[ArrayMD[A]] 
    = dmultia_readfile(path,dels,func)
  def fmultia_writefile[A:Manifest](ma: Rep[ArrayMD[A]], dels: Seq[Rep[String]], path: Rep[String], func: Rep[A] => Rep[String])(implicit ctx: SourceContext): Rep[Unit] 
    = dmultia_writefile(ma,dels,path,func)

  // --- Array constructors
  def fmultia_new[A:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ArrayMD[A]] 
    = dmultia_new[A](dims)
  def fmultia_new_immutable[A:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ArrayMD[A]] 
    = dmultia_new_immutable[A](dims)
  def fmultia_view[A:Manifest](ma: Rep[ArrayMD[A]], start: Seq[Rep[Int]], stride: Seq[Rep[Int]], dims: Seq[Rep[Int]], unitDims: Seq[Int] = Nil)(implicit ctx: SourceContext): Rep[ArrayMD[A]]
    = dmultia_view(ma,start,stride,dims,unitDims)

  // --- Array properties
  def fmultia_rank[A:Manifest](ma: Rep[ArrayMD[A]])(implicit ctx: SourceContext): Rep[Int]
    = dmultia_rank(ma)
  def fmultia_dim[A:Manifest](ma: Rep[ArrayMD[A]], n: Int)(implicit ctx: SourceContext): Rep[Int]
    = dmultia_dim(ma, n)
  def fmultia_size[A:Manifest](ma: Rep[ArrayMD[A]])(implicit ctx: SourceContext): Rep[Int]
    = dmultia_size(ma)

  // --- Array single element
  def fmultia_apply[A:Manifest](ma: Rep[ArrayMD[A]], i: Rep[ForgeAbstractIndices])(implicit ctx: SourceContext): Rep[A]
    = dmultia_apply(ma, i)
  def fmultia_update[A:Manifest](ma: Rep[ArrayMD[A]], i: Rep[ForgeAbstractIndices], x: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
    = dmultia_update(ma,i,x)

  // --- Array copies / reshaping
  def fmultia_mutable[A:Manifest](ma: Rep[ArrayMD[A]])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
    = dmultia_mutable(ma)
  def fmultia_immutable[A:Manifest](ma: Rep[ArrayMD[A]])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
    = dmultia_immutable(ma)
  def fmultia_permute[A:Manifest](ma: Rep[ArrayMD[A]], config: Seq[Int])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
    = dmultia_permute(ma, config)
  def fmultia_reshape[A:Manifest](ma: Rep[ArrayMD[A]], shape: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
    = dmultia_reshape(ma, shape)
  def fmultia_permute_view[A:Manifest](ma: Rep[ArrayMD[A]], config: Seq[Int])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
    = dmultia_permute_view(ma, config)
  def fmultia_reshape_view[A:Manifest](ma: Rep[ArrayMD[A]], shape: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
    = dmultia_reshape_view(ma, shape)

  // --- Parallel Ops
  def fmultia_fromfunction[A:Manifest](dims: Seq[Rep[Int]], f: Rep[ForgeLoopIndices] => Rep[A])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
    = dmultia_fromfunction(dims,f)
  def fmultia_map[A:Manifest,B:Manifest](ma: Rep[ArrayMD[A]], f: Rep[A] => Rep[B])(implicit ctx: SourceContext): Rep[ArrayMD[B]]
    = dmultia_map(ma,f)
  def fmultia_zipwith[A:Manifest,B:Manifest,R:Manifest](ma: Rep[ArrayMD[A]], mb: Rep[ArrayMD[B]], f: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext): Rep[ArrayMD[R]]
    = dmultia_zipwith(ma,mb,f)
  def fmultia_reduce[A:Manifest](ma: Rep[ArrayMD[A]], f: (Rep[A],Rep[A]) => Rep[A])(implicit ctx: SourceContext): Rep[A]
    = dmultia_reduce(ma,f)
  def fmultia_fold[A:Manifest](ma: Rep[ArrayMD[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A])(implicit ctx: SourceContext): Rep[A]
    = dmultia_fold(ma,f,zero)
  def fmultia_forindices[A:Manifest](ma: Rep[ArrayMD[A]], f: Rep[ForgeLoopIndices] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
    = dmultia_forindices[ma,f]
  def fmultia_foreach[A:Manifest](ma: Rep[ArrayMD[A]], f: Rep[A] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
    = dmultia_foreach(ma,f)
  def fmultia_NDmap[A:Manifest,B:Manifest](ma: Rep[ArrayMD[A]], mdims: Seq[Int], func: Rep[ArrayMD[A]] => Rep[ArrayMD[B]])(implicit ctx: SourceContext): Rep[ArrayMD[B]]
    = dmultia_NDmap(ma,mdims,func)

  def fmultia_groupBy[A:Manifest,K:Manifest,V:Manifest](ma: Rep[ArrayMD[A]], key: Rep[A] => Rep[K], value: Rep[A] => Rep[V])(implicit ctx: SourceContext): Rep[DeliteHashMap[K,Array1D[V]]]
    = dmultia_groupBy(ma,key,value)
  def fmultia_groupByReduce[A:Manifest,K:Manifest,V:Manifest](ma: Rep[ArrayMD[A]], key: Rep[A] => Rep[K], value: Rep[A] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext): Rep[DeliteHashMap[K,V]]
    = dmultia_groupByReduce(ma,key,value,reduce)

  // --- 1D Parallel Ops
  def fmultia_fromseq[A:Manifest](seq: Seq[Rep[A]])(implicit ctx: SourceContext): Rep[Array1D[A]] = {
    val data = fmultia_new[A](Seq(seq.length))
    for (i <- 0 until seq.length) {
      fmultia_update(data, Indices(i), seq(i))
    }
    delite_unsafe_immutable(data)
  }
  def fmultia_flatmap[A:Manifest,B:Manifest](ma: Rep[Array1D[A]], f: Rep[A] => Rep[Array1D[B]])(implicit ctx: SourceContext): Rep[Array1D[B]]
    = dmultia_flatmap(ma,f)
  def fmultia_filter[A:Manifest](ma: Rep[Array1D[A]], f: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext): Rep[Array1D[A]]
    = dmultia_filter(ma,f)

  // --- Buffer ops
  def fmultia_insert[A:Manifest](ma: Rep[Array1D[A]], x: Rep[A], index: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
    = dmultia_insert(ma,x,index)
  def fmultia_append[A:Manifest](ma: Rep[Array1D[A]], x: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
    = dmultia_append(ma,x)
  def fmultia_insertAll[A:Manifest](ma: Rep[ArrayMD[A]], rhs: Rep[ArrayMD[A]], axis: Int, index: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
    = dmultia_insertAll(ma,rhs,axis,index)
  def fmultia_appendAll[A:Manifest](ma: Rep[ArrayMD[A]], rhs: Rep[ArrayMD[A]], axis: Int)(implicit ctx: SourceContext): Rep[Unit]
    = dmultia_appendAll(ma,rhs,axis)
  def fmultia_remove[A:Manifest](ma: Rep[ArrayMD[A]], axis: Int, start: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]  
    = dmultia_remove(ma,axis,start,len)

  // --- Misc.
  def fmultia_mkstring[A:Manifest](ma: Rep[ArrayMD[A]], dels: Seq[Rep[String]], func: Option[Rep[A] => Rep[String]])(implicit ctx: SourceContext): Rep[String]
    = dmultia_mkstring(ma,dels,func)

  // --- 1D Ops
  def fmultia_sortIndices(len: Rep[Int], comp: (Rep[Int],Rep[Int]) => Rep[Int])(implicit ctx: SourceContext): Rep[Array1D[Int]]
    = dmultia_sortIndices(len,comp)
  def fmultia_sort[A:Manifest:Ordering](ma: Rep[Array1D[A]])(implicit ctx: SourceContext): Rep[Array1D[A]]
    = dmultia_sort(ma)
  def fmultia_sortWith[A:Manifest](ma: Rep[Array1D[A]], func: (Rep[A], Rep[A]) => Rep[Int])(implicit ctx: SourceContext): Rep[Array1D[A]]
    = dmultia_sortWith(ma,func)
  def fmultia_string_split(str: Rep[String],pat: Rep[String],lim: Rep[Int] = unit(0))(implicit ctx: SourceContext): Rep[Array1D[String]]
    = dmultia_string_split(str,pat,ofs)

  // --- 2D Ops
  def fmultia_matmult[A:Manifest:Numeric](lhs: Rep[Array2D[A]], rhs: Rep[Array2D[A]])(implicit ctx: SourceContext): Rep[Array2D[A]]
    = dmultia_matmult(lhs,rhs)
  def fmultia_matvecmult[A:Manifest:Numeric](mat: Rep[Array2D[A]], vec: Rep[Array1D[A]])(implicit ctx: SourceContext): Rep[Array1D[A]]
    = dmultia_matvecmult(mat,vec)
}

// Need these to make MultiArrays match other extern, but should not generate code for MultiArrays
trait ScalaGenMultiArraySupportOps
trait CudaGenMultiArraySupportOps
trait OpenCLGenMultiArraySupportOps
trait CGenMultiArraySupportOps

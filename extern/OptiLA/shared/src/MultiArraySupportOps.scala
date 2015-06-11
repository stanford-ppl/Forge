package optila.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

// Support for accessing Delite MultiArrays
trait ForgeIndicesOps extends Base { 
  type ForgeAbstractIndices
  type ForgeLoopIndices
  type ForgeIndices

  def findices_new(xs: Seq[Rep[Int]]): Rep[ForgeIndices]

  implicit def manifestAbstInds: Manifest[ForgeAbstractIndices]
  implicit def manifestLoopInds: Manifest[ForgeLoopIndices]
  implicit def manifestInds: Manifest[ForgeIndices]

  // (Don't yet know subtype info here)
  implicit def loopInds_to_abstInds(inds: Rep[ForgeLoopIndices]): Rep[ForgeAbstractIndices]
  implicit def inds_to_abstInds(inds: Rep[ForgeIndices]): Rep[ForgeAbstractIndices]
}

trait MultiArraySupportOps extends ForgeIndicesOps with Base {
	this: OptiLA with ForgeArrayOps =>

  type ArrayMD[T]
  type Array1D[T]
  type Array2D[T]
  type Array3D[T]
  type Array4D[T]
  type Array5D[T]
  type ForgeMap[K,V]

  // Implicit manifests
	implicit def manifestMD[T:Manifest]: Manifest[ArrayMD[T]]
  implicit def manifest1D[T:Manifest]: Manifest[Array1D[T]]
  implicit def manifest2D[T:Manifest]: Manifest[Array2D[T]]
  implicit def manifest3D[T:Manifest]: Manifest[Array3D[T]]
  implicit def manifest4D[T:Manifest]: Manifest[Array4D[T]]
  implicit def manifest5D[T:Manifest]: Manifest[Array5D[T]]
  implicit def manifestForgeMap[K:Manifest,V:Manifest]: Manifest[ForgeMap[K,V]]

  // Implicit casts
  // (We don't know here that Array1D etc. are subtypes of ArrayMD)
  implicit def f1D_to_fND[T:Manifest](ma: Rep[Array1D[T]]): Rep[ArrayMD[T]]
  implicit def f2D_to_fND[T:Manifest](ma: Rep[Array2D[T]]): Rep[ArrayMD[T]]
  implicit def f3D_to_fND[T:Manifest](ma: Rep[Array3D[T]]): Rep[ArrayMD[T]]
  implicit def f4D_to_fND[T:Manifest](ma: Rep[Array4D[T]]): Rep[ArrayMD[T]]
  implicit def f5D_to_fND[T:Manifest](ma: Rep[Array5D[T]]): Rep[ArrayMD[T]]

  // --- MultiMap
  def fmulmap_size[K:Manifest,V:Manifest](fm: Rep[ForgeMap[K,V]])(implicit ctx: SourceContext): Rep[Int]
  def fmulmap_get[K:Manifest,V:Manifest](fm: Rep[ForgeMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[V]
  def fmulmap_contains[K:Manifest,V:Manifest](fm: Rep[ForgeMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[Boolean]
  def fmulmap_keys[K:Manifest,V:Manifest](fm: Rep[ForgeMap[K,V]])(implicit ctx: SourceContext): Rep[Array1D[K]]
  def fmulmap_values[K:Manifest,V:Manifest](fm: Rep[ForgeMap[K,V]])(implicit ctx: SourceContext): Rep[Array1D[V]]
  def fmulmap_from_1d_arrays[K:Manifest,V:Manifest](keys: Rep[Array1D[K]], vals: Rep[Array1D[V]])(implicit ctx: SourceContext): Rep[ForgeMap[K,V]]

  // --- Rank type casts
  def fmultia_as_1D[T:Manifest](ma: Rep[ArrayMD[T]]): Rep[Array1D[T]]
  def fmultia_as_2D[T:Manifest](ma: Rep[ArrayMD[T]]): Rep[Array2D[T]]
  def fmultia_as_3D[T:Manifest](ma: Rep[ArrayMD[T]]): Rep[Array3D[T]]
  def fmultia_as_4D[T:Manifest](ma: Rep[ArrayMD[T]]): Rep[Array4D[T]]
  def fmultia_as_5D[T:Manifest](ma: Rep[ArrayMD[T]]): Rep[Array5D[T]]

  // --- File reading/writing
  def fmultia_readfile[A:Manifest](path: Rep[String], dels: Seq[Rep[String]], func: Rep[String] => Rep[A])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
  def fmultia_writefile[A:Manifest](ma: Rep[ArrayMD[A]], dels: Seq[Rep[String]], path: Rep[String], func: Rep[A] => Rep[String])(implicit ctx: SourceContext): Rep[Unit]

  // --- Array constructors
  def fmultia_new[A:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
  def fmultia_new_immutable[A:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
  def fmultia_view[A:Manifest](ma: Rep[ArrayMD[A]], start: Seq[Rep[Int]], stride: Seq[Rep[Int]], dims: Seq[Rep[Int]], unitDims: Seq[Int] = Nil)(implicit ctx: SourceContext): Rep[ArrayMD[A]]

  // --- Array properties
  def fmultia_rank[A:Manifest](ma: Rep[ArrayMD[A]])(implicit ctx: SourceContext): Rep[Int]
  def fmultia_dim[A:Manifest](ma: Rep[ArrayMD[A]], n: Int)(implicit ctx: SourceContext): Rep[Int]
  def fmultia_size[A:Manifest](ma: Rep[ArrayMD[A]])(implicit ctx: SourceContext): Rep[Int]

  // --- Array single element
  def fmultia_apply[A:Manifest](ma: Rep[ArrayMD[A]], i: Rep[ForgeAbstractIndices])(implicit ctx: SourceContext): Rep[A]
  def fmultia_update[A:Manifest](ma: Rep[ArrayMD[A]], i: Rep[ForgeAbstractIndices], x: Rep[A])(implicit ctx: SourceContext): Rep[Unit]

  // --- Array copies / reshaping
  def fmultia_mutable[A:Manifest](ma: Rep[ArrayMD[A]])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
  def fmultia_immutable[A:Manifest](ma: Rep[ArrayMD[A]])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
  def fmultia_permute[A:Manifest](ma: Rep[ArrayMD[A]], config: Seq[Int])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
  def fmultia_reshape[A:Manifest](ma: Rep[ArrayMD[A]], shape: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
  def fmultia_permute_view[A:Manifest](ma: Rep[ArrayMD[A]], config: Seq[Int])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
  def fmultia_reshape_view[A:Manifest](ma: Rep[ArrayMD[A]], shape: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ArrayMD[A]]

  // --- Parallel Ops
  def fmultia_fromfunction[A:Manifest](dims: Seq[Rep[Int]], f: Rep[ForgeLoopIndices] => Rep[A])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
  def fmultia_map[A:Manifest,B:Manifest](ma: Rep[ArrayMD[A]], f: Rep[A] => Rep[B])(implicit ctx: SourceContext): Rep[ArrayMD[B]]
  def fmultia_zipwith[A:Manifest,B:Manifest,R:Manifest](ma: Rep[ArrayMD[A]], mb: Rep[ArrayMD[B]], f: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext): Rep[ArrayMD[R]]
  def fmultia_reduce[A:Manifest](ma: Rep[ArrayMD[A]], f: (Rep[A],Rep[A]) => Rep[A])(implicit ctx: SourceContext): Rep[A]
  def fmultia_fold[A:Manifest](ma: Rep[ArrayMD[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A])(implicit ctx: SourceContext): Rep[A]
  def fmultia_forindices[A:Manifest](ma: Rep[ArrayMD[A]], f: Rep[ForgeLoopIndices] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
  def fmultia_foreach[A:Manifest](ma: Rep[ArrayMD[A]], f: Rep[A] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
  def fmultia_NDmap[A:Manifest,B:Manifest](ma: Rep[ArrayMD[A]], mdims: Seq[Int], func: Rep[ArrayMD[A]] => Rep[ArrayMD[B]])(implicit ctx: SourceContext): Rep[ArrayMD[B]]

  def fmultia_groupBy[A:Manifest,K:Manifest,V:Manifest](ma: Rep[ArrayMD[A]], key: Rep[A] => Rep[K], value: Rep[A] => Rep[V])(implicit ctx: SourceContext): Rep[DeliteHashMap[K,Array1D[V]]]
  def fmultia_groupByReduce[A:Manifest,K:Manifest,V:Manifest](ma: Rep[ArrayMD[A]], key: Rep[A] => Rep[K], value: Rep[A] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext): Rep[DeliteHashMap[K,V]]

  // --- 1D Parallel Ops
  def fmultia_fromseq[A:Manifest](seq: Seq[Rep[A]])(implicit ctx: SourceContext): Rep[Array1D[A]]
  def fmultia_flatmap[A:Manifest,B:Manifest](ma: Rep[Array1D[A]], f: Rep[A] => Rep[Array1D[B]])(implicit ctx: SourceContext): Rep[Array1D[B]]
  def fmultia_filter[A:Manifest](ma: Rep[Array1D[A]], f: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext): Rep[Array1D[A]]
  
  // --- Buffer ops
  def fmultia_insert[A:Manifest](ma: Rep[Array1D[A]], x: Rep[A], index: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def fmultia_append[A:Manifest](ma: Rep[Array1D[A]], x: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def fmultia_insertAll[A:Manifest](ma: Rep[ArrayMD[A]], rhs: Rep[ArrayMD[A]], axis: Int, index: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def fmultia_appendAll[A:Manifest](ma: Rep[ArrayMD[A]], rhs: Rep[ArrayMD[A]], axis: Int)(implicit ctx: SourceContext): Rep[Unit]
  def fmultia_remove[A:Manifest](ma: Rep[ArrayMD[A]], axis: Int, start: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]  

  // --- Misc.
  def fmultia_mkstring[A:Manifest](ma: Rep[ArrayMD[A]], dels: Seq[Rep[String]], func: Option[Rep[A] => Rep[String]])(implicit ctx: SourceContext): Rep[String]

  // --- 1D Ops
  def fmultia_sortIndices(len: Rep[Int], comp: (Rep[Int],Rep[Int]) => Rep[Int])(implicit ctx: SourceContext): Rep[Array1D[Int]]
  def fmultia_sort[A:Manifest:Ordering](ma: Rep[Array1D[A]])(implicit ctx: SourceContext): Rep[Array1D[A]]
  def fmultia_sortWith[A:Manifest](ma: Rep[Array1D[A]], func: (Rep[A], Rep[A]) => Rep[Int])(implicit ctx: SourceContext): Rep[Array1D[A]]
  def fmultia_string_split(str: Rep[String],pat: Rep[String],lim: Rep[Int] = unit(0))(implicit ctx: SourceContext): Rep[Array1D[String]]

  // --- 2D Ops
  def fmultia_matmult[A:Manifest](lhs: Rep[Array2D[A]], rhs: Rep[Array2D[A]], mult: (Rep[A],Rep[A]) => Rep[A], add: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A], one: Rep[A])(implicit ctx: SourceContext): Rep[Array2D[A]]
  def fmultia_matvecmult[A:Manifest](mat: Rep[Array2D[A]], vec: Rep[Array1D[A]], mult: (Rep[A],Rep[A]) => Rep[A], add: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A], one: Rep[A])(implicit ctx: SourceContext): Rep[Array1D[A]]

  // --- Pinning
  def fmultia_pin_1D_hack[A:Manifest](arr: Rep[ForgeArray[A]])(implicit ctx: SourceContext): Rep[Array1D[A]]
  def fmultia_flatpin_1d_hack[A:Manifest](ma: Rep[Array1D[A]])(implicit ctx: SourceContext): Rep[ForgeArray[A]]
}

trait MultiArraySupportCompilerOps extends MultiSupportOps {
	this: OptiLA2 with ForgeArrayOps =>

  // HashMap
  implicit def repForgeMapToForgeMapOpsCls[K:Manifest,V:Manifest](fm: Rep[ForgeMap[K,V]]) = new ForgeMapOpsCls(fm)
  class ForgeMapOpsCls[K:Manifest,V:Manifest](fm: Rep[ForgeMap[K,V]]) {
    def size: Rep[Int] = fmulmap_size(fm)
    def get(key: Rep[K]): Rep[V] = fmulmap_get(fm, key)
    def apply(key: Rep[K]): Rep[V] = fmulmap_get(fm, key)
    def contains(key: Rep[K]): Rep[Boolean] = fmulmap_contains(fm, key)
    def keys: Rep[Array1D[K]] = fmulmap_keys(fm)
    def values: Rep[Array1D[V]] = fmulmap_values(fm)
  }
  
  object ArrayMD {
    def apply[T:Manifest](dims: Rep[Int]*)(implicit ctx: SourceContext) = fmultia_new[T](dims)
    def imm[T:Manifest](dims: Rep[Int]*)(implicit ctx: SourceContext) = fmultia_new_immutable[T](dims)
    def fromFunction[T:Manifest](dims: Rep[Int]*)(func: Rep[ForgeLoopIndices] => Rep[T])(implicit ctx: SourceContext) = fmultia_fromfunction(dims, func)
  }

  implicit def repMultiAtoMultiAOps[T:Manifest](ma: Rep[ArrayMD[T]])(implicit ctx: SourceContext) = new ArrayMDOpsCls(ma)
  class ArrayMDOpsCls[T:Manifest](ma: Rep[ArrayMD[T]])(implicit ctx: SourceContext) {
    // --- rank casts
    def as1D: Rep[Array1D[T]] = fmultia_as_1D(ma)
    def as2D: Rep[Array2D[T]] = fmultia_as_2D(ma)
    def as3D: Rep[Array3D[T]] = fmultia_as_3D(ma)
    def as4D: Rep[Array4D[T]] = fmultia_as_4D(ma)
    def as5D: Rep[Array5D[T]] = fmultia_as_5D(ma)

    // --- properties
    def rank: Rep[Int] = fmultia_rank(ma)
    def size: Rep[Int] = fmultia_size(ma)
    def dim(i: Int): Rep[Int] = fmultia_dim(ma, i)

    // --- copies
    def Clone: Rep[ArrayMD[T]] = this.map{x => x}
    def mutable: Rep[ArrayMD[T]] = fmultia_mutable(ma)
    def immutable: Rep[ArrayMD[T]] = fmultia_immutable(ma)
    
    // --- Reshaping
    def reshape(d0: Rep[Int]): Rep[Array1D[T]] = fmultia_reshape(ma, Seq(d0)).as1D
    def reshape(d0: Rep[Int], d1: Rep[Int]): Rep[Array2D[T]] = fmultia_reshape(ma, Seq(d0, d1)).as2D
    def reshape(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int]): Rep[Array3D[T]] = fmultia_reshape(ma, Seq(d0, d1, d2)).as3D
    def reshape(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int], d3: Rep[Int]): Rep[Array4D[T]] = fmultia_reshape(ma, Seq(d0, d1, d2, d3)).as4D
    def reshape(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int], d3: Rep[Int], d4: Rep[Int]): Rep[Array5D[T]] = fmultia_reshape(ma, Seq(d0, d1, d2, d3, d4)).as5D
    def reshape(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int], d3: Rep[Int], d4: Rep[Int], d5: Rep[Int]*): Rep[ArrayMD[T]] = fmultia_reshape(ma, Seq(d0, d1, d2, d3, d4) ++ d5.toSeq)

    def reshapeView(d0: Rep[Int]): Rep[Array1D[T]] = fmultia_reshape_view(ma, Seq(d0)).as1D
    def reshapeView(d0: Rep[Int], d1: Rep[Int]): Rep[Array2D[T]] = fmultia_reshape_view(ma, Seq(d0, d1)).as2D
    def reshapeView(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int]): Rep[Array3D[T]] = fmultia_reshape_view(ma, Seq(d0, d1, d2)).as3D
    def reshapeView(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int], d3: Rep[Int]): Rep[Array4D[T]] = fmultia_reshape_view(ma, Seq(d0, d1, d2, d3)).as4D
    def reshapeView(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int], d3: Rep[Int], d4: Rep[Int]): Rep[Array5D[T]] = fmultia_reshape_view(ma, Seq(d0, d1, d2, d3, d4)).as5D
    def reshapeView(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int], d3: Rep[Int], d4: Rep[Int], d5: Rep[Int]*): Rep[ArrayMD[T]] = fmultia_reshape_view(ma, Seq(d0, d1, d2, d3, d4) ++ d5.toSeq)

    // --- Permuting
    // TODO: Move to respective classes when they're created
    def permute(d0: Int, d1: Int, d2: Int) = fmultia_permute(ma, Seq(d0, d1, d2)).as3D
    def permute(d0: Int, d1: Int, d2: Int, d3: Int) = fmultia_permute(ma, Seq(d0, d1, d2, d3)).as4D
    def permute(d0: Int, d1: Int, d2: Int, d3: Int, d4: Int) = fmultia_permute(ma, Seq(d0, d1, d2, d3, d4)).as5D
    def permute(d0: Int, d1: Int, d2: Int, d3: Int, d4: Int, d5: Int*) = fmultia_permute(ma, Seq(d0, d1, d2, d3, d4) ++ d5.toSeq)

    def permuteView(d0: Int, d1: Int, d2: Int) = fmultia_permute_view(ma, Seq(d0, d1, d2)).as3D
    def permuteView(d0: Int, d1: Int, d2: Int, d3: Int) = fmultia_permute_view(ma, Seq(d0, d1, d2, d3)).as4D
    def permuteView(d0: Int, d1: Int, d2: Int, d3: Int, d4: Int) = fmultia_permute_view(ma, Seq(d0, d1, d2, d3, d4)).as5D
    def permuteView(d0: Int, d1: Int, d2: Int, d3: Int, d4: Int, d5: Int*) = fmultia_permute_view(ma, Seq(d0, d1, d2, d3, d4) ++ d5.toSeq)

    // --- single element
    def apply(i: Rep[Int]*): Rep[T] = fmultia_apply(ma,findices_new(i.toList))
    def update(i: Seq[Rep[Int]], x: Rep[T]): Rep[Unit] = fmultia_update(ma,findices_new(i),x)

    def apply(i: Rep[ForgeAbstractIndices]): Rep[T] = fmultia_apply(ma,i)
    def update(i: Rep[ForgeAbstractIndices], x: Rep[T]): Rep[Unit] = fmultia_update(ma,i,x)
    
    // --- parallel ops
    def map[B:Manifest](f: Rep[T] => Rep[B]): Rep[ArrayMD[B]] = fmultia_map(ma,f)
    def zip[B:Manifest,R:Manifest](y: Rep[ArrayMD[B]])(f: (Rep[T],Rep[B]) => Rep[R]): Rep[ArrayMD[R]] = fmultia_zipwith(ma,y,f)
    def reduce(zero: Rep[T])(f: (Rep[T],Rep[T]) => Rep[T]): Rep[T] = fmultia_reduce(ma,f,zero)
    def foreach(f: Rep[T] => Rep[Unit]): Rep[Unit] = fmultia_foreach(ma,{x: Rep[T] => f(x) })
    def forIndices(f: Rep[ForgeLoopIndices] => Rep[Unit]): Rep[Unit] = fmultia_forindices(ma,{i: Rep[ForgeLoopIndices] => f(i) })
    def groupBy[K:Manifest](key: Rep[T] => Rep[K]) = fmultia_groupBy(ma,key)
    def groupByReduce[K:Manifest, V:Manifest](key: Rep[T] => Rep[K], value: Rep[T] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V]) = fmultia_groupByReduce(ma,key,value,reduce)

    def mmap(f: Rep[T] => Rep[T]): Rep[Unit] = this.forIndices{i => ma(i) = f(ma(i))}
    def mzip[B:Manifest](y: Rep[ArrayMD[B]])(f: (Rep[T],Rep[B]) => Rep[T]): Rep[Unit] = this.forIndices{i => ma(i) = f(ma(i),y(i))}
  
    def mkString(dels: Rep[String]*) = fmultia_mkstring(ma,dels,None)
    def mkStringFunc(dels: Rep[String]*)(func: Rep[T] => Rep[String]) = fmultia_mkstring(ma,dels,Some(func))
  }

  object Array1D {
    def apply[T:Manifest](len: Rep[Int])(implicit ctx: SourceContext) = fmultia_new[T](List(len)).as1D
    def imm[T:Manifest](len: Rep[Int])(implicit ctx: SourceContext) = fmultia_new_immutable[T](List(len)).as1D
    def fromFunction[T:Manifest](len: Rep[Int])(func: Rep[Int] => Rep[T])(implicit ctx: SourceContext) = fmultia_fromfunction(List(len), {x: Rep[ForgeLoopIndices] => func(x.flat)}).as1D
  
    def sortIndices(length: Rep[Int])(comparator: (Rep[Int], Rep[Int]) => Rep[Int])(implicit ctx: SourceContext) = fmultia_sortIndices(length, comparator)
    def splitString(str: Rep[String],pat: Rep[String],ofs: Rep[Int] = unit(0))(implicit ctx: SourceContext) = fmultia_string_split(str,pat,ofs)

    def fromFile[T:Manifest](path: Rep[String])(func: Rep[String] => Rep[T])(implicit ctx: SourceContext) = fmultia_readfile(path, Seq(unit("\n")), func)
  
    def flatPinHACK[T:Manifest](arr: Rep[ForgeArray[T]])(implicit ctx: SourceContext) = fmultia_pin_1D_hack(arr)
  }

  implicit def repArray1DtoArray1DOps[T:Manifest](ma: Rep[Array1D[T]])(implicit ctx: SourceContext) = new Array1DOpsCls(ma)
  class Array1DOpsCls[T:Manifest](ma: Rep[Array1D[T]])(implicit ctx: SourceContext) {
    // --- properties
    def length: Rep[Int] = fmultia_size(ma)

    // --- copies
    def Clone: Rep[ArrayMD[T]] = this.map{x => x}.as1D
    def mutable: Rep[Array1D[T]] = fmultia_mutable(ma).as1D
    def immutable: Rep[Array1D[T]] = fmultia_immutable(ma).as1D

    // --- slicing
    // NOTE: len here is the length of the array view (including stride). should be calculated as floor((prev_len - start)/stride)
    def slice(start: Rep[Int], stride: Rep[Int], len: Rep[Int]) = fmultia_view(ma, Seq(start), Seq(stride), Seq(len)).as1D
    def slice(start: Rep[Int], len: Rep[Int]) = fmultia_view(ma, Seq(start), Seq(unit(1)), Seq(len)).as1D

    // --- mutability / buffering
    def update(i: Rep[Int], x: Rep[T]): Rep[Unit] = fmultia_update(ma,findices_new(Seq(i)),x)
    def insert(i: Rep[Int], x: Rep[T]): Rep[Unit] = fmultia_insert(ma,x,i)
    def append(x: Rep[T]): Rep[Unit] = fmultia_append(ma,x)
    def remove(start: Rep[Int], len: Rep[Int]): Rep[Unit] = fmultia_remove(ma, 0, start, len)
    def remove(i: Rep[Int]): Rep[Unit] = this.remove(i, unit(1))
  
    def insertAll(i: Rep[Int], rhs: Rep[Array1D[T]]): Rep[Unit] = fmultia_insertAll(ma, rhs, 0, i)
    def appendAll(rhs: Rep[Array1D[T]]): Rep[Unit] = fmultia_appendAll(ma, rhs, 0)
    
    def :=(rhs: Rep[T]): Rep[Unit] = fmultia_append(ma, rhs)
    def ::=(rhs: Rep[Array1D[T]]): Rep[Unit] = fmultia_appendAll(ma, rhs, 0)

    def sortWith(func: (Rep[T], Rep[T]) => Rep[Int]): Rep[Array1D[T]] = fmultia_sortWith(ma, func)

    // --- 1D parallel ops
    def map[B:Manifest](f: Rep[T] => Rep[B]): Rep[Array1D[B]] = fmultia_map(ma,f).as1D
    def zip[B:Manifest,R:Manifest](y: Rep[Array1D[B]])(f: (Rep[T],Rep[B]) => Rep[R]): Rep[Array1D[R]] = fmultia_zipwith(ma,y,f).as1D

    def forIndices(f: Rep[Int] => Rep[Unit]): Rep[Unit] = fmultia_forindices(ma, {i: Rep[ForgeLoopIndices] => f(i.flat) })
    def filter(f: Rep[T] => Rep[Boolean]): Rep[Array1D[T]] = fmultia_filter(ma,f)
    def flatMap[B:Manifest](f: Rep[T] => Rep[Array1D[B]])(implicit ctx: SourceContext) = fmultia_flatmap(ma,f)  
  
    def mkString(del: Rep[String] = unit(",")) = fmultia_mkstring(ma,Seq(del),None)
    def writeFile(path: Rep[String])(func: Rep[T] => Rep[String])(implicit ctx: SourceContext) = fmultia_writefile(ma, Seq(unit("\n")), path, func)
  
    // --- Pinning
    def flatPinHACK = fmultia_flatpin_1d_hack(ma)
  }

  implicit def repArray1DToOrderedArray1DOpsCls[T:Manifest:Ordering](ma: Rep[Array1D[T]])(implicit ctx: SourceContext) = new Array1DOrderedOpsCls(ma)
  class Array1DOrderedOpsCls[T:Manifest:Ordering](ma: Rep[Array1D[T]])(implicit ctx: SourceContext) {
    def sort = fmultia_sort(ma)
  }

  object Array2D {
    def apply[T:Manifest](rows: Rep[Int], cols: Rep[Int])(implicit ctx: SourceContext) = fmultia_new[T](List(rows,cols)).as2D
    def imm[T:Manifest](rows: Rep[Int], cols: Rep[Int])(implicit ctx: SourceContext) = fmultia_new_immutable[T](List(rows,cols)).as2D
    def fromFunction[T:Manifest](rows: Rep[Int], cols: Rep[Int])(func: (Rep[Int], Rep[Int]) => Rep[T])(implicit ctx: SourceContext) = fmultia_fromfunction(List(rows,cols), {x: Rep[ForgeLoopIndices] => func(x(0),x(1))}).as2D
    def fromFile[T:Manifest](path: Rep[String], cdel: Rep[String] = unit("\\s+"))(func: Rep[String] => Rep[T])(implicit ctx: SourceContext) = fmultia_readfile(path, Seq(unit("\n"), cdel), func)
  }

  implicit def repArray2DToArray2DOps[T:Manifest](ma: Rep[Array2D[T]])(implicit ctx: SourceContext) = new Array2DOpsCls(ma)
  class Array2DOpsCls[T:Manifest](ma: Rep[Array2D[T]])(implicit ctx: SourceContext) {
    // --- properties
    def rows = fmultia_dim(ma, 0)
    def cols = fmultia_dim(ma, 1)

    // --- copies
    def mutable: Rep[Array2D[T]] = fmultia_mutable(ma).as2D
    def immutable: Rep[Array2D[T]] = fmultia_immutable(ma).as2D

    // --- Permuting
    def t: Rep[Array2D[T]] = fmultia_permute(ma, Seq(1,0)).as2D
    def vt: Rep[Array2D[T]] = fmultia_permute_view(ma, Seq(1,0)).as2D

    // --- Slicing
    def slice(startRow: Rep[Int], rowStride: Rep[Int], rows: Rep[Int], startCol: Rep[Int], colStride: Rep[Int], cols: Rep[Int]) = fmultia_view(ma, Seq(startRow, startCol), Seq(rowStride, colStride), Seq(rows, cols)).as2D
    def slice(startRow: Rep[Int], rows: Rep[Int], startCol: Rep[Int], cols: Rep[Int]) = fmultia_view(ma, Seq(startRow, startCol), Seq(unit(1), unit(1)), Seq(rows, cols)).as2D
    def sliceRow(start: Rep[Int]) = fmultia_view(ma, Seq(start, unit(0)), Seq(unit(1)), Seq(cols), Seq(0)).as1D
    def sliceCol(start: Rep[Int]) = fmultia_view(ma, Seq(unit(0), start), Seq(unit(1)), Seq(rows), Seq(1)).as1D

    def sliceRows(start: Rep[Int], num: Rep[Int]) = this.slice(start, unit(1), num, unit(0), unit(1), cols).as2D
    def sliceCols(start: Rep[Int], num: Rep[Int]) = this.slice(unit(0), unit(1), rows, start, unit(1), num).as2D

    // --- mutability/buffering
    def update(i: Rep[Int], j: Rep[Int], x: Rep[T]): Rep[Unit] = fmultia_update(ma,findices_new(Seq(i,j)),x)

    def insertRow(i: Rep[Int], rhs: Rep[Array1D[T]]): Rep[Unit] = fmultia_insertAll(ma, rhs, 0, i)
    def insertCol(j: Rep[Int], rhs: Rep[Array1D[T]]): Rep[Unit] = fmultia_insertAll(ma, rhs, 1, j)
    def appendRow(rhs: Rep[Array1D[T]]): Rep[Unit] = fmultia_appendAll(ma, rhs, 0)
    def appendCol(rhs: Rep[Array1D[T]]): Rep[Unit] = fmultia_appendAll(ma, rhs, 1)

    def insertRows(i: Rep[Int], rhs: Rep[Array2D[T]]): Rep[Unit] = fmultia_insertAll(ma, rhs, 0, i)
    def insertCols(j: Rep[Int], rhs: Rep[Array2D[T]]): Rep[Unit] = fmultia_insertAll(ma, rhs, 1, j)
    def appendRows(rhs: Rep[Array2D[T]]): Rep[Unit] = fmultia_appendAll(ma, rhs, 0)
    def appendCols(rhs: Rep[Array2D[T]]): Rep[Unit] = fmultia_appendAll(ma, rhs, 1)

    def removeRows(start: Rep[Int], len: Rep[Int]) = fmultia_remove(ma, 0, start, len)
    def removeCols(start: Rep[Int], len: Rep[Int]) = fmultia_remove(ma, 1, start, len)
    def removeRow(i: Rep[Int]) = this.removeRows(i, unit(1))
    def removeCol(j: Rep[Int]) = this.removeCols(j, unit(1))

    // --- 2D parallel ops
    def map[B:Manifest](f: Rep[T] => Rep[B]): Rep[Array2D[B]] = fmultia_map(ma,f).as2D
    def zip[B:Manifest,R:Manifest](y: Rep[Array2D[B]])(f: (Rep[T],Rep[B]) => Rep[R]): Rep[Array2D[R]] = fmultia_zipwith(ma,y,f).as2D

    def forIndices(f: (Rep[Int], Rep[Int]) => Rep[Unit]): Rep[Unit] = fmultia_forindices(ma, {i: Rep[ForgeLoopIndices] => f(i(0), i(1)) })
    def mapRows[R:Manifest](f: Rep[Array1D[T]] => Rep[Array1D[R]]) = fmultia_NDmap(ma,List(0),{r: Rep[ArrayMD[T]] => f(r.as1D) }).as2D
    def mapCols[R:Manifest](f: Rep[Array1D[T]] => Rep[Array1D[R]]) = fmultia_NDmap(ma,List(1),{c: Rep[ArrayMD[T]] => f(c.as1D) }).as2D
  
    def mkString(rowDel: Rep[String] = unit("\n"), colDel: Rep[String] = unit(",")) = fmultia_mkstring(ma,Seq(rowDel,colDel),None)
    def writeFile(path: Rep[String], cdel: Rep[String] = unit("    "))(func: Rep[T] => Rep[String])(implicit ctx: SourceContext) = fmultia_writefile(ma, Seq(unit("\n"),cdel), path, func)
  }
}

package optila.library

import scala.
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

import optila.shared._
import optila.shared.ops._
import optila.shared.typeclass._
import optila.library._
import optila.library.classes._

trait ForgeIndicesWrapper { 
  type ForgeAbstractIndices = AbstractIndices
  type ForgeLoopIndices = LoopIndices
  type ForgeIndices = Indices

  abstract class AbstractIndices(inds: Seq[Rep[Int]]) { 
    def flatten(ofs: Rep[Int], stride: Seq[Rep[Int]]): Rep[Int] = {
      (Seq.tabulate(inds.length){d => inds(d) * stride(d) } :+ ofs).reduce{_+_}
    }
    def firstOrFlat: Rep[Int] = inds(0)
    def apply(i: Int): Rep[Int] = inds(i)
  }
  case class LoopIndices(flat: Rep[Int], inds: Seq[Rep[Int]]) extends AbstractIndices(inds) { 
    override def firstOrFlat: Rep[Int] = flat
  }
  case class Indices(inds: Seq[Rep[Int]]) extends AbstractIndices(inds)

  def unflattenIndex(i: Rep[Int], dims: Seq[Rep[Int]]): Seq[Rep[Int]] = {
    if (dims.length == 1) { LoopIndices(i, Seq(i)) }
    else {
      Seq.tabulate(dims.length) { d => 
        if (d == dims.length - 1) { i % dims(d) }
        else { (i / dims.drop(d+1).reduce{_*_}) % dims(d) }
      }
    }
  }

  object LoopIndices {
    def unflatten(i: Rep[Int], dims: Seq[Rep[Int]]) = LoopIndices(i, unflattenIndex(i, dims))
  }

  def findices_new(xs: Seq[Rep[Int]]): Rep[ForgeIndices] = Indices(xs) 

  implicit def manifestAbstInds: Manifest[ForgeAbstractIndices] = manifest[AbstractIndices]
  implicit def manifestLoopInds: Manifest[ForgeLoopIndices] = manifest[LoopIndices]
  implicit def manifestInds: Manifest[ForgeIndices] = manifest[Indices]

  implicit def loopInds_to_abstInds(inds: Rep[ForgeLoopIndices]) = inds.asInstanceOf[Rep[AbstractIndices]]
  implicit def inds_to_abstInds(inds: Rep[ForgeIndices]) = inds.asInstanceOf[Rep[AbstractIndices]]
}

trait MultiArrayLib extends OptiLABase with ForgeIndicesWrapper with ForgeArrayWrapper { this: MultiArraySupportWrapper =>
  
  def dimsToStride(dims: Seq[Rep[Int]]): Seq[Rep[Int]] = {
    Seq.tabulate(dims.length){d => 
      if (d == dims.length - 1) { 1 }
      else dims.drop(d + 1).reduce{_*_}
    }
  }

  // Library implementation of the Forge Multidimensional array
  class LibArrayMD[T: Manifest](_data: Array[T], _dims: Seq[Rep[Int]], _isMutable: Boolean, _ofs: Rep[Int], _stride: Seq[Rep[Int]]) {
    
    def this(data: Array[T], dims: Seq[Rep[Int]], isMutable: Boolean) = this(data, dims, isMutable, 0, dimsToStride(dims))
    // Class fields
    var __data = _data
    var __dims = _dims
    var __rank = _dims.length
    var __isMutable = _isMutable
    var __ofs = _ofs
    var __stride = _stride

    def __asMutable = {__isMutable = true; this}
    def __asImmutable = {__isMutable = false; this}

    override def toString() = fmultia_mkstring(this, Seq.tabulate(__rank - 1){i => "\n" ** i}.reverse :+ ", ", None)
  }

  class LibArray1D[T: Manifest](_data: Array[T], _dims: Seq[Rep[Int]], _isMutable: Boolean, _ofs: Rep[Int], _stride: Seq[Rep[Int]]) 
    extends LibArrayMD[T](_data, _dims, _isMutable, _ofs, _stride)

  class LibArray2D[T: Manifest](_data: Array[T], _dims: Seq[Rep[Int]], _isMutable: Boolean, _ofs: Rep[Int], _stride: Seq[Rep[Int]]) 
    extends LibArrayMD[T](_data, _dims, _isMutable, _ofs, _stride)

  class LibArray3D[T: Manifest](_data: Array[T], _dims: Seq[Rep[Int]], _isMutable: Boolean, _ofs: Rep[Int], _stride: Seq[Rep[Int]]) 
    extends LibArrayMD[T](_data, _dims, _isMutable, _ofs, _stride)

  class LibArray4D[T: Manifest](_data: Array[T], _dims: Seq[Rep[Int]], _isMutable: Boolean, _ofs: Rep[Int], _stride: Seq[Rep[Int]]) 
    extends LibArrayMD[T](_data, _dims, _isMutable, _ofs, _stride)

  class LibArray5D[T: Manifest](_data: Array[T], _dims: Seq[Rep[Int]], _isMutable: Boolean, _ofs: Rep[Int], _stride: Seq[Rep[Int]]) 
    extends LibArrayMD[T](_data, _dims, _isMutable, _ofs, _stride)

  // Other constructors
  object LibArrayMD {
    def apply[T:Manifest](data: Array[T], dims: Seq[Rep[Int]], mutable: Boolean = false) = new LibArrayMD(data, dims, mutable)
    def apply[T:Manifest](dims: Seq[Rep[Int]], mutable: Boolean = false): LibArrayMD[T] = new LibArrayMD(new Array[T](dims reduce {(a,b) => a * b}), dims, mutable)
  }

  object LibArray1D {
    def apply[T:Manifest](data: Array[T], mutable: Boolean = false) = new LibArray1D(data, Seq(data.length), mutable, Seq(0), Seq(1))
  }
}

/* For use in interpreted code */
trait MultiArraySupportWrapper extends MultiArrayLib with ForgeIndicesWrapper with InputOutputWrapper {
  this : ForgeArrayWrapper with ForgeArrayBufferWrapper => 

  type ArrayMD[T] = LibArrayMD[T]
  type Array1D[T] = LibArray1D[T]
  type Array2D[T] = LibArray2D[T]
  type Array3D[T] = LibArray3D[T]
  type Array4D[T] = LibArray4D[T]
  type Array5D[T] = LibArray5D[T]
  type ForgeMap[K,V] = scala.collection.mutable.HashMap[K,V]
  
  // Implicit manifests
  implicit def manifestND[T:Manifest] = manifest[ArrayMD[T]]
  implicit def manifest1D[T:Manifest] = manifest[Array1D[T]]
  implicit def manifest2D[T:Manifest] = manifest[Array2D[T]]
  implicit def manifest3D[T:Manifest] = manifest[Array3D[T]]
  implicit def manifest4D[T:Manifest] = manifest[Array4D[T]]
  implicit def manifest5D[T:Manifest] = manifest[Array5D[T]]
  implicit def manifestForgeMap[K:Manifest,V:Manifest] = manifest[ForgeMap[K,V]]

  // Implicit casts
  // (We don't know here that Array1D etc. are subtypes of ArrayMD)
  implicit def f1D_to_fND[T:Manifest](ma: Rep[Array1D[T]]) = ma.asInstanceOf[Rep[LibArrayMD[T]]]
  implicit def f2D_to_fND[T:Manifest](ma: Rep[Array2D[T]]) = ma.asInstanceOf[Rep[LibArrayMD[T]]]
  implicit def f3D_to_fND[T:Manifest](ma: Rep[Array3D[T]]) = ma.asInstanceOf[Rep[LibArrayMD[T]]]
  implicit def f4D_to_fND[T:Manifest](ma: Rep[Array4D[T]]) = ma.asInstanceOf[Rep[LibArrayMD[T]]]
  implicit def f5D_to_fND[T:Manifest](ma: Rep[Array5D[T]]) = ma.asInstanceOf[Rep[LibArrayMD[T]]]

  // --- MultiMap
  def fmulmap_size[K:Manifest,V:Manifest](fm: Rep[ForgeMap[K,V]])(implicit ctx: SourceContext): Rep[Int]
    = fm.size
  def fmulmap_get[K:Manifest,V:Manifest](fm: Rep[ForgeMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[V]
    = fm(key)
  def fmulmap_contains[K:Manifest,V:Manifest](fm: Rep[ForgeMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[Boolean]
    = fm.contains(key)
  def fmulmap_keys[K:Manifest,V:Manifest](fm: Rep[ForgeMap[K,V]])(implicit ctx: SourceContext): Rep[Array1D[K]]
    = LibArray1D(fm.keys.toArray, false)
  def fmulmap_values[K:Manifest,V:Manifest](fm: Rep[ForgeMap[K,V]])(implicit ctx: SourceContext): Rep[Array1D[V]]
    = LibArray1D(fm.values.toArray, false)
  def fmulmap_from_1d_arrays[K:Manifest,V:Manifest](keys: Rep[Array1D[K]], vals: Rep[Array1D[V]])(implicit ctx: SourceContext): Rep[ForgeMap[K,V]]
    = scala.collection.mutable.HashMap(keys.zip(vals): _*)

  // --- Rank type casts
  // Can't use .asInstanceOf[...] because the ArrayMD may not actually be an instance of that child class
  def fmultia_as_1D[T:Manifest](ma: Rep[ArrayMD[T]]) = {
    fassert(ma.__rank == 1, "Rank of array is not 1 - cannot cast to Array1D")
    new LibArray1D(ma.__data, ma.__dims, ma.__isMutable, ma.__ofs, ma.__stride)
  }
  def fmultia_as_2D[T:Manifest](ma: Rep[ArrayMD[T]]) = {
    fassert(ma.__rank == 2, "Rank of array is not 2 - cannot cast to Array2D")
    new LibArray2D(ma.__data, ma.__dims, ma.__isMutable, ma.__ofs, ma.__stride)
  }
  def fmultia_as_3D[T:Manifest](ma: Rep[ArrayMD[T]]) = {
    fassert(ma.__rank == 3, "Rank of array is not 3 - cannot cast to Array3D")
    new LibArray3D(ma.__data, ma.__dims, ma.__isMutable, ma.__ofs, ma.__stride)
  }
  def fmultia_as_4D[T:Manifest](ma: Rep[ArrayMD[T]]) = {
    fassert(ma.__rank == 4, "Rank of array is not 4 - cannot cast to Array4D")
    new LibArray4D(ma.__data, ma.__dims, ma.__isMutable, ma.__ofs, ma.__stride)
  }
  def fmultia_as_5D[T:Manifest](ma: Rep[ArrayMD[T]]) = {
    fassert(ma.__rank == 5, "Rank of array is not 5 - cannot cast to Array5D")
    new LibArray5D(ma.__data, ma.__dims, ma.__isMutable, ma.__ofs, ma.__stride)
  }

  // --- File reading/writing
  def fmultia_readfile[A:Manifest](path: Rep[String], dels: Seq[Rep[String]], func: Rep[String] => Rep[A])(implicit ctx: SourceContext): Rep[ArrayMD[A]] = {
    var dims: Seq[Int] = Nil
    
    var partial = ForgeFileReader.readLines(path){line => line}
    var prevSize = partial.length
    dims = dims :+ orig.length
    
    for (d <- 1 until dels.length) {
      partial = partial.flatMap{_.split(dels(d))}
      prevSize = curr.length
      dims = dims :+ ( curr.length / sizes(d - 1) )
    }
    val data = partial.map(func)
    LibArrayMD(data, dims)
  }
  def fmultia_writefile[A:Manifest](ma: Rep[ArrayMD[A]], dels: Seq[Rep[String]], path: Rep[String], func: Rep[A] => Rep[String])(implicit ctx: SourceContext): Rep[Unit] {
    if (ma.__rank > 1) {
      ForgeFileWriter.writeLines(path, ma.__dims(0)){i => 
        val start = i +: Seq.fill(ma.__rank - 1)(0)
        val stride = Seq.fill(ma.__rank)(1)
        val view = fmultia_view(ma, start, stride, ma.__dims.drop(1), Seq(0))
        fmultia_mkstring(view, dels.drop(1), Some(func))
      }
    }
    else {
      val arr = fmultia_as_1D(ma)
      ForgeFileWriter.writeLines(path, ma.__dims(0)){i => fmultia_apply(ma, LoopIndices(i, Seq(i)) ).toString }
    }
  }

  // --- Array constructors
  def fmultia_new[A:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ArrayMD[A]] = {
    fassert(dims.length > 0, "Cannot create array of rank 0")
    LibArrayMD(dims, true) 
  }
  def fmultia_new_immutable[A:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ArrayMD[A]] = {
    fassert(dims.length > 0, "Cannot create array of rank 0")
    LibArrayMD(dims, false)
  }

  private def createViewParams(m: Int, prevOfs: Rep[Int], prevStride: Seq[Rep[Int]], n: Int, ofs: Seq[Rep[Int]], stride: Seq[Rep[Int]], unitDims: Seq[Int])(implicit ctx: SourceContext): (Exp[Int], Seq[Exp[Int]]) = {
    val flatOfs = sumTree( ofs.zip(prevStride).map{o => delite_int_times(o._1, o._2)} )
    val newOfs = delite_int_plus(flatOfs, prevOfs)
    val newStride = if (m == n)     stride.zip(prevStride).map{s => delite_int_times(s._1, s._2)}
                    else if (m < n) stride.take(n - m) ++ stride.drop(n - m).zip(prevStride).map{s => delite_int_times(s._1, s._2)}                           // reshape view?
                    else            prevStride.zipWithIndex.filterNot{unitDims contains _._2}.map{_._1}.zip(stride).map{s => delite_int_times(s._1, s._2)}    // sub-dim slice
    (newOfs, newStride)
  }

  def fmultia_view[A:Manifest](ma: Rep[ArrayMD[A]], start: Seq[Rep[Int]], stride: Seq[Rep[Int]], dims: Seq[Rep[Int]], unitDims: Seq[Int] = Nil)(implicit ctx: SourceContext): Rep[ArrayMD[A]] = {
    fassert(dims.length == stride.length, "In view creation, view dimensions and view stride must have same cardinality")
    fassert(start.length == ma.__rank, "In view creation, start dimensions must have same cardinality as rank of input ArrayMD")
    fassert(unitDims < ma.__rank, "In view creation, number of unit dimensions must be less than rank of input ArrayMD")
    val (newOfs, newStride) = createViewParams(ma.__rank, ma.__ofs, ma.__stride, stride.length, start, stride, unitDims)
    LibArrayMD(ma.__data, dims, false, newOfs, newStride)
  }

  // --- Array properties
  def fmultia_rank[A:Manifest](ma: Rep[ArrayMD[A]])(implicit ctx: SourceContext): Rep[Int] = ma.__rank
  def fmultia_dim[A:Manifest](ma: Rep[ArrayMD[A]], n: Int)(implicit ctx: SourceContext): Rep[Int] = ma.__dims(n)
  def fmultia_size[A:Manifest](ma: Rep[ArrayMD[A]])(implicit ctx: SourceContext): Rep[Int] = ma.__dims.reduce{_*_}

  // --- Array single element
  def fmultia_apply[A:Manifest](ma: Rep[ArrayMD[A]], i: Rep[ForgeAbstractIndices])(implicit ctx: SourceContext): Rep[A]
    = ma.__data(i.flatten(ma.__ofs, ma.__stride))
  def fmultia_update[A:Manifest](ma: Rep[ArrayMD[A]], i: Rep[ForgeAbstractIndices], x: Rep[A])(implicit ctx: SourceContext): Rep[Unit] = {
    fassert(ma.__isMutable, "Cannot update immutable array")
    ma.__data(i.flatten(ma.__ofs, ma.__stride)) = x
  }

  // --- Array copies / reshaping
  def fmultia_mutable[A:Manifest](ma: Rep[ArrayMD[A]])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
    = fmultia_map(ma, {x: Rep[A] => x}).__asMutable
  def fmultia_immutable[A:Manifest](ma: Rep[ArrayMD[A]])(implicit ctx: SourceContext): Rep[ArrayMD[A]]
    = fmultia_map(ma, {x: Rep[A] => x}).__asImmutable
  def fmultia_permute[A:Manifest](ma: Rep[ArrayMD[A]], config: Seq[Int])(implicit ctx: SourceContext): Rep[ArrayMD[A]] = {
    val size = fmultia_size(ma)
    val dims = ma.__dims
    val rank = ma.__rank
    val data = Array.tabulate(size){v => 
      val inds = unflattenIndex(v, dims)
      val pinds = Seq.tabulate(rank){d => inds(config(d))}
      fmultia_apply(ma, LoopIndices(v, pinds))
    }
    val permuteDims = Seq.tabulate(rank){d => dims(config(d)) }
    LibArrayMD(data, permuteDims, false)
  }

  def fmultia_reshape[A:Manifest](ma: Rep[ArrayMD[A]], shape: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[ArrayMD[A]] = {
    fassert(shape.reduce{_*_} == fmultia_size(ma), "Total size after reshaping must not change (" + shape.reduce{_*_} + " != " + fmultia_size(ma) + ")")
    val copy = fmultia_map(ma, {x: Rep[A] => x})
    LibArrayMD(copy.__data, shape, false)
  }

  def fmultia_permute_view[A:Manifest](ma: Rep[ArrayMD[A]], config: Seq[Int])(implicit ctx: SourceContext): Rep[ArrayMD[A]] = {
    val dims = ma.__dims
    val start = ma.__ofs
    val strides = ma.__stride
    val permuteStrides = Seq.tabulate(n){d => strides(config(d)) }
    val permuteDims = Seq.tabulate(n){d => dims(config(d)) }
    LibArrayMD(ma.__data, permuteDims, false, start, permuteStrides)
  }
  
  def fmultia_reshape_view[A:Manifest](ma: Rep[ArrayMD[A]], shape: Seq[Rep[Int]], UNSAFE: Boolean)(implicit ctx: SourceContext): Rep[ArrayMD[A]] = {
    fassert(shape.reduce{_*_} = fmultia_size(ma), "Total size after reshaping must not change (" + shape.reduce{_*_} + " != " + fmultia_size(ma) + ")")
    
    // Check if MultiArray is a partial view (non-zero offset, strides not equal to dimension strides)
    val dimStrides = dimsToStride(ma.__dims)
    val fullView = ma.__ofs == 0 && ma.__stride.zip(dimStrides).map{d => d._1 == d._2}.fold{true}{_&&_}
    fassert(fullView || UNSAFE, "Creation of a reshaped view of a view is generally not possible. If you know your use case is sound, use the option UNSAFE = true. This will create a reshape view of the underlying data.")

    LibArrayMD(ma.__data, shape, false, 0, dimsToStride(shape))
  }

  // --- Parallel Ops
  def fmultia_fromfunction[A:Manifest](dims: Seq[Rep[Int]], f: Rep[ForgeLoopIndices] => Rep[A])(implicit ctx: SourceContext): Rep[ArrayMD[A]] = {
    val size = dims.reduce{_*_}
    val data = Array.tabulate[A](size){v => 
      val i = LoopIndices.unflatten(v, dims) 
      f(i)
    }
    LibArrayMD(data, dims, false)
  }
  def fmultia_map[A:Manifest,B:Manifest](ma: Rep[ArrayMD[A]], f: Rep[A] => Rep[B])(implicit ctx: SourceContext): Rep[ArrayMD[B]] = {
    val size = fmultia_size(ma)
    val data = new Array[A](size)
    for (v <- 0 until size) {
      val i = LoopIndices.unflatten(v, ma.__dims)
      val x = fmultia_apply(ma, i)
      data(v) = f(x)
    }
    LibArrayMD(data, ma.__dims, false)
  }

  def fmultia_zipwith[A:Manifest,B:Manifest,R:Manifest](ma: Rep[ArrayMD[A]], mb: Rep[ArrayMD[B]], f: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext): Rep[ArrayMD[R]] = {
    val sizeA = fmultia_size(ma)
    val sizeB = fmultia_size(mb)
    val dimsA = ma.__dims
    val dimsB = mb.__dims
    fwarn(sizeA == sizeB, "Size of ArrayMDs in zipWith do not match (" sizeA + " != " + sizeB + ")")
    fwarn(ma.__rank == mb.__rank, "Ranks of ArrayMDs in zipWith do not match (" + ma.__rank + " != " + mb.__rank + ")")
    dimsA.zip(dimsB).zipWithIndex foreach {d => fwarn(d._1._1 == d._1._2, "Dimension " + d._2 + " of ArrayMDs do not match in zipWith (" + d._1._1 + " != " + d._1._2 + ")")}
  
    val data = new Array[A](sizeA)
    for (v <- 0 until sizeA) {
      val iA = LoopIndices.unflatten(v, dimsA)
      val iB = LoopIndices.unflatten(v, dimsB)
      val a = fmultia_apply(ma, iA)
      val b = fmultia_apply(mb, iB)
      data(v) = f(a,b)
    }
    LibArrayMD(data, dimsA, false)
  }
  
  def fmultia_reduce[A:Manifest](ma: Rep[ArrayMD[A]], f: (Rep[A],Rep[A]) => Rep[A])(implicit ctx: SourceContext): Rep[A] = {
    val copy = fmultia_map(ma, {x: Rep[A] => x})  // change view to dense if necessary 
    copy.__data.reduce{(a,b) => f(a,b)}
  }
  def fmultia_fold[A:Manifest](ma: Rep[ArrayMD[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A])(implicit ctx: SourceContext): Rep[A] = {
    val copy = fmultia_map(ma, {x: Rep[A] => x})  // change view to dense if necessary 
    copy.__data.fold(zero){(a,b) => f(a,b)}
  }

  def fmultia_forindices[A:Manifest](ma: Rep[ArrayMD[A]], f: Rep[ForgeLoopIndices] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit] = {
    val size = fmultia_size(ma)
    for (v <- 0 until size) {
      val i = LoopIndices.unflatten(v, ma.__dims)
      f(i)
    }
  }
  def fmultia_foreach[A:Manifest](ma: Rep[ArrayMD[A]], f: Rep[A] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit] = {
    val size = fmultia_size(ma)
    for (v <- 0 until size) {
      val i = LoopIndices.unflatten(v, ma.__dims)
      val x = fmultia_apply(ma, i)
      f(x)
    }
  }
  // Super slow version of ND map with debug checks!
  def fmultia_NDmap[A:Manifest,B:Manifest](ma: Rep[ArrayMD[A]], mdims: Seq[Int], func: Rep[ArrayMD[A]] => Rep[ArrayMD[B]])(implicit ctx: SourceContext): Rep[ArrayMD[B]] = {
    val dims = ma.__dims
    val origRank = ma.__rank
    val innerRank = origRank - mdims.length
    val size = fmultia_size(ma)

    val outerDims = dims.zipWithIndex.filter{ mdims.contains _._2 }.map{_._1}
    val innerDims = dims.zipWithIndex.filterNot{ mdims.contains _._2 }.map{_._1}

    val outerLoopSize = outerDims.fold(1){_*_}
    val innerLoopSize = innerDims.fold(1){_*_}

    var encounteredWarnings = false

    val data = new Array[B](size)
    for (v <- 0 until outerLoopSize) {
      // Create ND view of ma
      val sinds = unflattenIndex(v, outerDims)

      // TODO: Should be a more concise way of writing this
      var ofs = Seq[Int]()
      var k = 0
      for (d <- 0 until origRank) {
        if (mdims contains d) { ofs = ofs :+ sinds(k); k++ }
        else { ofs = ofs :+ 0 }
      }      
      var stride = Seq.fill(origRank)(1)
      val view = fmultia_view(ma, ofs, stride, innerDims, mdims)
      
      // Apply map function on view - may return a view itself
      val mout = func(view)

      // Rank, size, and individual dimensions should match in output. Individual dimensions is optional for now
      fassert(mout.__rank == innerRank, "Output of ND map must have the same rank as input slice")
      fassert(fmultia_size(mout) == size, "Output of ND map must have the same size as input slice")
      if (!encounteredWarnings) {
        mout.__dims.zip(innerDims).zipWithIndex foreach {d => 
          fwarn(d._1._1 == d._1._2, "Dimension " + d._2 + " of output of ND map does not match that of input slice (" + d._1._1 + " != " + d._1._2 + ")")
          encounteredWarnings = encounteredWarnings || (d._1._1 != d._1._2)
        }
      }

      // Copy output of map function to data
      for (v2 <- 0 until innerLoopSize) {
        val i = LoopIndices.unflatten(v2, innerDims)
        val x = fmultia_apply(mout, i)
        data(v*innerLoopSize + v2) = x
      }
    }
    LibArrayMD(data, dims, false)
  }

  def fmultia_groupBy[A:Manifest,K:Manifest,V:Manifest](ma: Rep[ArrayMD[A]], keyFunc: Rep[A] => Rep[K], valFunc: Rep[A] => Rep[V])(implicit ctx: SourceContext): Rep[DeliteHashMap[K,Array1D[A]]] = {
    val copy = fmultia_map(ma, {x: Rep[A] => x})
    val hm = copy.__data.groupBy(keyFunc).mapValues{v => LibArray1D(v.map(valFunc)) }
    (scala.collection.mutable.HashMap[K,V]() ++ hm)
  }
  def fmultia_groupByReduce[A:Manifest,K:Manifest,V:Manifest](ma: Rep[ArrayMD[A]], keyFunc: Rep[A] => Rep[K], valFunc: Rep[A] => Rep[V], redFunc: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext): Rep[DeliteHashMap[K,V]] = {
    val copy = fmultia_map(ma, {x: Rep[A] => x})
    val hm = copy.__data.groupBy[K](keyFunc).mapValues{v => v.map(valFunc).reduce(redFunc) }
    (scala.collection.mutable.HashMap[K,V]() ++ hm)    
  }

  // --- 1D Parallel Ops
  def fmultia_fromseq[A:Manifest](seq: Seq[Rep[A]])(implicit ctx: SourceContext): Rep[Array1D[A]] = {
    val data = fmultia_new[A](Seq(seq.length))
    for (i <- 0 until seq.length) {
      fmultia_update(data, Indices(i), seq(i))
    }
    data.__asImmutable
  }
  def fmultia_flatmap[A:Manifest,B:Manifest](ma: Rep[Array1D[A]], f: Rep[A] => Rep[Array1D[B]])(implicit ctx: SourceContext): Rep[Array1D[B]] = {
    val copy = fmultia_map(ma, {x: Rep[A] => x})
    val data = copy.__data.flatMap{x => 
      val e = f(x)
      fmultia_map(e, {x: Rep[A] => x}).__data
    }
    LibArray1D(data)
  }
  def fmultia_filter[A:Manifest](ma: Rep[Array1D[A]], f: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext): Rep[Array1D[A]] = {
    val copy = fmultia_map(ma, {x: Rep[A] => x})
    val data = copy.__data.filter(f)
    LibArray1D(data)
  }
  
  // Adapted from ArrayBuffer code
  private def fdata_insertspace[A:Manifest](data: Rep[Array[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit] = {
    val newData = fdata_ensureextra(d,len)
    System.arraycopy(data, pos, newData, pos + len, data.length - pos)
    (newData)
  }

  private def fdata_ensureextra[A:Manifest](data: Rep[Array[A]], curLen: Rep[Int], extra: Rep[Int]): Rep[Unit] = {
    if (data.length - curLen < extra) fdata_realloc(data, curLen - extra)
    else data
  }

  private def fdata_realloc[A:Manifest](oldData: Rep[Array[A]], curLen: Rep[Int], minLen: Rep[Int]): Rep[Unit] = {  
    val doubleLength = oldData.length * 2
    var n = if (4 > doubleLength) 4 else doubleLength
    while (n < minLen) { n = n * 2 } // could also be done with log + pow/shifting

    val newData = new Array[A](n)
    System.arraycopy(oldData, 0, newData, 0, curLen)
    (newData)
  }

  // --- Buffer ops
  def fmultia_insert[A:Manifest](ma: Rep[Array1D[A]], x: Rep[A], index: Rep[Int])(implicit ctx: SourceContext): Rep[Unit] = {
    fassert(ma.__isMutable, "Cannot insert element into an immutable array")
    fassert(ma.__rank == 1, "Element insert is undefined for arrays of rank greater than one")
  
    val newData = fdata_insertspace(ma.__data, index, 1)
    newData(index) = x
    ma.__data = newData
    ma.__dims = Seq(ma.__dims(0) + 1)
  }
  def fmultia_append[A:Manifest](ma: Rep[Array1D[A]], x: Rep[A])(implicit ctx: SourceContext): Rep[Unit] = {
    fassert(ma.__isMutable, "Cannot append element to an immutable array")
    fassert(ma.__rank == 1, "Element append is undefined for arrays of rank greater than one")
  
    val newData = fdata_ensureextra(ma.__data, ma.__dims(0), 1)
    newData(ma.__dims(0)) = x
    ma.__data = newData
    ma.__dims = Seq(ma.__dims(0) + 1) 
  }
  def fmultia_insertAll[A:Manifest](ma: Rep[ArrayMD[A]], rhs: Rep[ArrayMD[A]], axis: Int, index: Rep[Int])(implicit ctx: SourceContext): Rep[Unit] = {
    fassert(ma.__isMutable, "Cannot insert elements to an immutable array")
    fassert(ma.__rank == rhs.__rank || ma.__rank == rhs.__rank + 1, "Rank of rhs of insertAll must be equal to lhs's rank or lhs's rank minus one")
    fassert(axis <= ma.__rank, "Insert axis must be between 0 (inclusive) and lhs rank (exclusive)")
    if (ma.__rank == 1) {
      val size = fmultia_size(rhs)
      val newData = fdata_insertspace(ma.__data, ma.__dims(0), size)
      for (v <- 0 until size) {
        val i = LoopIndices.unflatten(v, rhs.__dims)
        newData(v + index) = fmultia_apply(ma, i)
      }
      ma.__data = newData
      ma.__dims = Seq(ma.__dims(0) + size)
    }
  }
  def fmultia_appendAll[A:Manifest](ma: Rep[ArrayMD[A]], rhs: Rep[ArrayMD[A]], axis: Int)(implicit ctx: SourceContext): Rep[Unit]
    = fmultia_insertAll(ma, rhs, axis, ma.__dims(axis))

  def fmultia_remove[A:Manifest](ma: Rep[ArrayMD[A]], axis: Int, start: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit] = {
    fassert(ma.__isMutable, "Cannot remove elements from an immutable array")
    fassert(axis <= ma.__rank, "Removal axis must be between 0 (inclusive) and lhs rank (exclusive)")

    if (ma.__rank == 1) {
      System.arraycopy(ma.__data, start + len, ma.__data, start, ma.__dims(0) - (start + len))
      ma.__dims = Seq(ma.__dims(0) - len)
    }
  }

  // --- Misc.
  def fmultia_mkstring[A:Manifest](ma: Rep[ArrayMD[A]], dels: Seq[Rep[String]], func: Option[Rep[A] => Rep[String]])(implicit ctx: SourceContext): Rep[String] = {
    val size = fmultia_size(ma)
    val dims = ma.__dims
    val rank = ma.__rank

    def stringify(x: Rep[A]) = func match {
      case Some(f) => f(x)
      case None => x.toString
    }

    def array1d_mkstring(ma: Rep[ArrayMD[A]], del: Rep[String]) = {
      var s = ""
      var v = 0
      while (v < fmultia_size(ma) - 1) {
        val x = fmultia_apply(ma, LoopIndices(i, Seq(i)))
        s = s + stringify(x) 
        s = s + del
        v = v + 1
      }
      val x = fmultia_apply(ma, LoopIndices(i, Seq(i)))
      (s + stringify(x))
    }
    def array2d_mkstring(ma: Rep[ArrayMD[A]], rdel: Rep[String], cdel: Rep[String]) = {
      val numRows = ma.__dims(0)
      val numCols = ma.__dims(1)
      var s = ""
      var v = 0
      while (v < numRows - 1) {
        val row = fmultia_view(ma, Seq(v, 0), Seq(1), Seq(numCols), Seq(0))
        s = s + array1d_mkstring(row, cdel)
        s = s + rdel
        v = v + 1
      }
      val row = fmultia_view(ma, Seq(v, 0), Seq(1), Seq(numCols), Seq(0))
      (s + array1d_mkstring(row, cdel))
    }
    def arraynd_mkstring(ma: Rep[ArrayMD[A]]) = {
      val hdims = dims.take(rank - 2)
      val idims = dims.drop(rank - 2)
      val hsize = hdims.reduce{_*_}
      var s = ""
      var v = 0
      var prevInds = Seq.fill(rank - 2)(0)
      while (v < size) {
        val inds = unflattenIndex(v, hdims)
        val view = fmultia_view(ma, inds ++ Seq(0,0), Seq.fill(rank-2)(1), idims, Seq.tabulate(rank-2){i => i})

        s = s + array2d_mkstring(ma, dels(rank - 2), dels(rank - 1))

        for (r <- 0 until rank-2) {
          if (inds(r) != prevInds(r) && inds(r) < hdims(r) - 1)
            s = s + dels(r)
        }
        prevInds = inds
        v = v + 1
      }
      (s)
    }

    if (size == 0) { "[ ]" }
    else if (rank == 1) { array1d_mkstring(ma, dels(0)) } 
    else if (rank == 2) { array2d_mkstring(ma, dels(0), dels(1)) }
    else                { arraynd_mkstring(ma) }
  }

  // --- 1D Ops
  def fmultia_sortIndices(len: Rep[Int], comp: (Rep[Int],Rep[Int]) => Rep[Int])(implicit ctx: SourceContext): Rep[Array1D[Int]] = {
    Array.tabulate(len){i => i}.sortWith{(i,j) => comp(i,j) == -1 }
  } 
  def fmultia_sort[A:Manifest:Ordering](ma: Rep[Array1D[A]])(implicit ctx: SourceContext): Rep[Array1D[A]] = {
    val copy = fmultia_map(ma, {x: Rep[A] => x})
    val data = copy.__data.sortBy{x => x}
    LibArray1D(data)
  }

  def fmultia_sortWith[A:Manifest](ma: Rep[Array1D[A]], func: (Rep[A], Rep[A]) => Rep[Int])(implicit ctx: SourceContext): Rep[Array1D[A]] = {
    val copy = fmultia_map(ma, {x: Rep[A] => x})
    val data = copy.__data.sortWith{(a,b) => func(a,b) < 0}
    LibArray1D(data)
  }
  def fmultia_string_split(str: Rep[String],pat: Rep[String],lim: Rep[Int] = unit(0))(implicit ctx: SourceContext): Rep[Array1D[String]] = {
    LibArray1D(str.split(pat,lim))
  }

  // --- 2D Ops
  def fmultia_matmult[A:Manifest](lhs: Rep[Array2D[A]], rhs: Rep[Array2D[A]], mult: (Rep[A],Rep[A]) => Rep[A], add: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A], one: Rep[A])(implicit ctx: SourceContext): Rep[Array2D[A]] = {
    fassert(lhs.__dims(1) == rhs.__dims(0), "Dimension mismatch in matrix multiply: " + lhs.__dims(1) + " != " + rhs.__dims(0))
    val numRows = lhs.__dims.apply(0)
    val numCols = rhs.__dims.apply(1)
    val common = lhs.__dims.apply(1)
    val data = new Array[T](numRows*numCols)
    val i = 0
    val j = 0
    for (i <- 0 until numRows) {
      for (j <- 0 until numCols) {
        data(i*numCols + j) = Array.tabulate(common){k => 
          // Have to use implicitly[Numeric] since Arith has precedence on Rep * Rep
          implicitly[Numeric[T]].times(fmultia_apply(lhs, Indices(Seq(i, k))), fmultia_apply(rhs, Indices(Seq(k, j))))
        }.sum
      }
    }
    LibArrayMD(data, Seq(numRows, numCols), false)
  }
  def fmultia_matvecmult[A:Manifest](mat: Rep[Array2D[A]], vec: Rep[Array1D[A]], mult: (Rep[A],Rep[A]) => Rep[A], add: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A], one: Rep[A])(implicit ctx: SourceContext): Rep[Array1D[A]] = {
    val rhs = fmultia_view(vec, Seq(0), Seq(1,1), vec.__dims :+ 1, Nil).as2D
    fmultia_matmult(mat, rhs, mult, add, zero, one)
  }

  // --- Pinning
  def fmultia_pin_1D_hack[A:Manifest](arr: Rep[ForgeArray[A]])(implicit ctx: SourceContext): Rep[Array1D[A]] = {
    LibArray1D(arr)
  }
  def fmultia_flatpin_1d_hack[A:Manifest](ma: Rep[Array1D[A]])(implicit ctx: SourceContext): Rep[ForgeArray[A]] = {
    ma.__data
  }
}

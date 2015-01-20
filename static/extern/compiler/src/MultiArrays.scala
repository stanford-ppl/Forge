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
  def multia_empty[T:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext) = dmultia_new(dims)
  def multia_empty_imm[T:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext) = dmultia_new_immutable(dims)
  def multia_fromfunction[T:Manifest](dims: Seq[Rep[Int]], func: Rep[Seq[Rep[Int]]] => Rep[T])(implicit ctx: SourceContext) = dmultia_fromfunction(dims,func)
  
  def multia_view[T:Manifest](ma: Rep[ForgeMultiArray[T]], start: Seq[Rep[Int]], stride: Seq[Rep[Int]], dims: Seq[Rep[Int]]) = dmultia_view(ma,start,stride,dims)

  // --- Properties
  def multia_rank[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext) = dmultia_rank(ma)
  def multia_shape[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext) = dmultia_shape(ma)
  def multia_size[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext) = dmultia_size(ma)

  // --- Single element operators
  def multia_apply[T:Manifest](ma: Rep[ForgeMultiArray[T]],i: Seq[Rep[Int]])(implicit ctx: SourceContext) = dmultia_apply(ma,i)
  def multia_update[T:Manifest](ma: Rep[ForgeMultiArray[T]],i: Seq[Rep[Int]],x: Rep[T])(implicit ctx: SourceContext) = dmultia_update(ma,i,x)
  
  // --- Copies / Mutating / Permuting
  def multia_clone[T:Manifest](ma: Rep[ForgeMultiArray[T]])(implicit ctx: SourceContext) = dmultia_clone(ma)
  def multia_permute[T:Manifest](ma: Rep[ForgeMultiArray[T]], config: Seq[Int])(implicit ctx: SourceContext) = dmultia_permute(ma,config)
  def multia_reshape[T:Manifest](ma: Rep[ForgeMultiArray[T]], shape: Seq[Rep[Int]])(implicit ctx: SourceContext) = dmultia_reshape(ma,shape)

  // --- Parallel ops
  def multia_map[T:Manifest,R:Manifest](ma: Rep[ForgeMultiArray[T]], func: Rep[T] => Rep[R])(implicit ctx: SourceContext) = dmultia_map(ma,func)
  def multia_zipwith[T:Manifest,B:Manifest,R:Manifest](ma: Rep[ForgeMultiArray[T]], mb: Rep[ForgeMultiArray[B]], func: (Rep[T],Rep[B]) => Rep[R])(implicit ctx: SourceContext) = dmultia_zipwith(ma,mb,func)
  def multia_reduce[T:Manifest](ma: Rep[ForgeMultiArray[T]], func: (Rep[T],Rep[T]) => Rep[T], zero: Rep[T])(implicit ctx: SourceContext) = dmultia_reduce(ma,func,zero)

  def multia_mmap[T:Manifest](ma: Rep[ForgeMultiArray[T]], func: Rep[T] => Rep[T])(implicit ctx: SourceContext) = dmultia_mmap(ma,func)
  def multia_mzipwith[T:Manifest,B:Manifest](ma: Rep[ForgeMultiArray[T]], mb: Rep[ForgeMultiArray[B]], func: (Rep[T],Rep[B]) => Rep[T])(implicit ctx: SourceContext) = dmultia_mzipwith(ma,mb,func)

  def multia_NDmap[A:Manifest,B:Manifest](ma: Rep[ForgeMultiArray[A]], mdims: Seq[Int], func: Rep[ForgeMultiArray[A]] => Rep[ForgeMultiArray[B]])(implicit ctx: SourceContext) = dmultia_NDmap(ma,mdims,func)

  // --- Buffer ops
  def multia_NDinsert[A:Manifest](ma: Rep[ForgeMultiArray[A]], rhs: Rep[ForgeMultiArray[A]], axis: Int, index: Rep[Int])(implicit ctx: SourceContext) = dmultia_NDinsert(ma,rhs,axis,index)
  def multia_NDappend[A:Manifest](ma: Rep[ForgeMultiArray[A]], rhs: Rep[ForgeMultiArray[A]], axis: Int)(implicit ctx: SourceContext) = dmultia_NDappend(ma,rhs,axis)

  def multia_insertAll[A:Manifest](ma: Rep[ForgeMultiArray[A]], rhs: Rep[ForgeMultiArray[A]], axis: Int, index: Rep[Int])(implicit ctx: SourceContext) = dmultia_insertAll(ma,rhs,axis,index)
  def multia_appendAll[A:Manifest](ma: Rep[ForgeMultiArray[A]], rhs: Rep[ForgeMultiArray[A]], axis: Int)(implicit ctx: SourceContext) = dmultia_appendAll(ma,rhs,axis,index)

  // --- Misc.
  def multia_mkstring[T:Manifest](ma: Rep[ForgeMultiArray[T]], dels: Seq[Rep[String]])(implicit ctx: SourceContext) = dmultia_mkstring(ma,dels)  
  def multia_string_split(str: Rep[String], pat: Rep[String], ofs: Rep[Int] = unit(0))(implicit ctx: SourceContext) = dmultia_string_split(str,pat,ofs)

  // --- 1D Operations
  def multia_sortwith[T:Manifest](ma: Rep[ForgeArray1D[T]], comp: (Rep[T],Rep[T]) => Rep[Int])(implicit ctx: SourceContext)
    = dmultia_sortIndices(ma.length, {(a,b) => 
        val aV = ma(a)
        val bV = ma(b)
        comp(aV,bV)
      })

  def multia_sortIndices[R:Manifest:Ordering](len: Rep[Int], func: (Rep[Int] => Rep[R]))(implicit ctx: SourceContext)
    = dmultia_sortIndices(len, {(a,b) =>
        val aV = func(a)
        val bV = func(b)

        // Java comparator requires three conditions + default
        if (delite_less_than(aV, bV)) unit(-1)
        else if (delite_equals(aV, bV)) unit(0)
        else if (delite_greater_than(aV, bV)) unit(1)
        else unit(0)
      })

  def multia_flatmap[T:Manifest,R:Manifest](ma: Rep[ForgeArray1D[T]], func: Rep[T] => Rep[ForgeArray1D[R]])(implicit ctx: SourceContext) = dmultia_flatmap(ma,func)
  def multia_filter[T:Manifest](ma: Rep[ForgeArray1D[T]], cond: Rep[T] => Rep[Boolean])(implicit ctx: SourceContext) = dmultia_filter(ma,cond)
  def multia_groupBy[T:Manifest,K:Manifest](ma: Rep[ForgeArray1D[T]], key: Rep[T] => Rep[K])(implicit ctx: SourceContext) = dmultia_groupBy(ma,key)
  def multia_groupByReduce[T:Manifest,K:Manifest,V:Manifest](ma: Rep[ForgeArray1D[T]],key: Rep[T] => Rep[K], value: Rep[T] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext)
    = dmultia_groupByReduce(ma,key,value,reduce)

  // May need to change this implementation later..
  def multia_fromseq[T:Manifest](sq: Seq[Rep[T]])(implicit ctx: SourceContext) = {
    val out = dmultia_new[T](unit(sq.length))
    for (i <- 0 until sq.length) {
      out(unit(i)) = sq(i)
    }
    delite_unsafe_immutable(out)
  }

  def multia_insert[A:Manifest](ma: Rep[ForgeArray1D[A]], x: Rep[A], index: Rep[Int])(implicit ctx: SourceContext) = dmultia_insert(ma,x,index)
  def multia_append[A:Manifest](ma: Rep[ForgeArray1D[A]], x: Rep[A])(implicit ctx: SourceContext) = dmultia_append(ma,x)

  // --- 2D Operations
  def multia_matmult[T:Manifest:Numeric](lhs: Rep[ForgeArray2D[T]], rhs: Rep[ForgeArray2D[T]])(implicit ctx: SourceContext) = dmultia_matmult(lhs,rhs)
  def multia_matvecmult[T:Manifest:Numeric](mat: Rep[ForgeArray2D[T]], vec: Rep[ForgeArray1D[T]])(implicit ctx: SourceContext) = dmultia_matvecmult(lhs,rhs)
}

package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

// We use metadata to track the rank of each MultiArray in the IR. Since the Scala library doesn't have
// an IR, we stick the metadata directly in a hashmap on array creation. Since we always have control of
// when and where these arrays are created, we can always ensure their metadata is defined.
trait MultiArrayImpls extends FlatMultiArrays { this: OptiMADSL =>

  def importImplUtils() {
    val Utils = grp("Utils")
    val T = tpePar("T")

    internal (Utils) ("reductionTree", T, (SList(T), ((T,T) ==> T)) :: SList(T)) implements composite ${
      if ($0.length == 1) $0
      else if ($0.length % 2 == 0) reductionTree( List.tabulate($0.length/2){i => $1( $0(2*i), $0(2*i+1)) }, $1)
      else reductionTree( List.tabulate($0.length/2){i => $1( $0(2*i), $0(2*i+1)) } :+ $0.last, $1)
    }
    internal (Utils) ("productTree", Nil, SList(MInt) :: MInt) implements composite ${ reductionTree[Int]($0, {(a,b) => a * b}).head }
    internal (Utils) ("sumTree", Nil, SList(MInt) :: MInt) implements composite ${ reductionTree[Int]($0, {(a,b) => a + b}).head }

    internal (Utils) ("dimsToStrides", Nil, SList(MInt) :: SList(MInt)) implements composite ${
      List.tabulate($0.length){d =>
        if (d == $0.length - 1) unit(1)
        else productTree($0.drop(d + 1))
      }
    }
    internal (Utils) ("flattenIndices", Nil, (("indices", SList(MInt)), ("ofs", MInt), ("stride", SList(MInt))) :: MInt) implements composite ${
      List.tabulate($indices.length){i => $indices(i)*$stride(i) }.reduce{_+_} + $ofs
    }

    // Flat offset
    internal (Utils) ("createFlatViewOfs", Nil, (("targOfs", MInt), ("targStride", SList(MInt)), ("ofs", SList(MInt))) :: MInt) implements composite ${
      $targOfs + sumTree( $ofs.zip($targStride).map{case (a,b) => a*b} )
    }
    internal (Utils) ("createFlatViewStrides", Nil, (("targRank", SInt), ("curRank", SInt), ("targStride", SList(MInt)), ("stride", SList(MInt)), ("unitDims", SList(SInt))) :: SList(MInt)) implements composite ${
      if ($targRank == $curRank)  $stride.zip($targStride).map{case (a,b) => a*b}
      else if ($targRank < $curRank) $stride.take($curRank - $targRank) ++ $stride.drop($curRank - $targRank).zip($targStride).map{case (a,b) => a*b}   // Reshape-view
      else $targStride.zipWithIndex.filterNot{$unitDims contains _._2}.map{_._1}.zip($stride).map{case (a,b) => a*b}                                    // Sub-arity slice
    }

  }

  def importMultiArrayImpls() {
    val T = tpePar("T")

    val ArrayND = lookupTpe("ArrayND")
    val ImplND  = lookupTpe("ImplND")

    val Impls = withTpe(ArrayND)
    Impls {
      internal.infix ("ofs") (Nil :: MInt) implements composite ${ maimpl_ofs($self) }
      internal.infix ("dims") (Nil :: SList(MInt)) implements composite ${ maimpl_dims($self) }
      internal.infix ("strides") (Nil :: SList(MInt)) implements composite ${ maimpl_strides($self) }

      internal ("maimpl_ofs") (Nil :: MInt) implements composite ${ layout($self) match {
        case lt if lt.isView => field[Int]($self, "ofs")
        case _ => unit(0)
      }}
      internal ("maimpl_dim") (SInt :: MInt) implements composite ${ layout($self) match {
        case MLayout(1,Flat,Plain) => array_length($self.asInstanceOf[Rep[ForgeArray[T]]])
        case _ => field[Int]($self, "dim_" + $1)
      }}
      internal ("maimpl_dims") (Nil :: SList(MInt)) implements composite ${ List.tabulate(rank($self)){i => maimpl_dim($self, i) } }
      internal ("maimpl_size") (Nil :: MInt) implements composite ${ productTree(maimpl_dims($self)) }

      internal ("maimpl_stride") (SInt :: MInt) implements composite ${ layout($self) match {
        case lt if lt.isView => field[Int]($self, "stride_" + $1)
        case _ => throw new Exception("Can't get single stride for non-view")
      }}
      internal ("maimpl_strides") (Nil :: SList(MInt)) implements composite ${ layout($self) match {
        case lt if lt.isView => List.tabulate(rank($self)){i => maimpl_stride($self, i) }
        case _ => dimsToStrides(maimpl_dims($self))
      }}
    }

    // --- Import concrete implementations
    importImplUtils()
    importFlatMultiArrayImpls()
  }
}

trait FlatMultiArrays { this: OptiMADSL =>

  def importFlatMultiArrayImpls() {
    val T = tpePar("T")
    val Indices = lookupTpe("Indices")
    val Range   = lookupTpe("Range")
    val ArrayND = lookupTpe("ArrayND")
    val ImplND  = lookupTpe("ImplND")
    val FlatND  = lookupTpe("FlatND")
    val MLayout  = lookupMeta("MLayout" )

    val ArrayNDFlatOps = withTpe(ArrayND)
    ArrayNDFlatOps {
      internal.infix ("asFlat1D") (Nil :: MArray(T)) implements composite ${
        assert(layout($self) == MLayout(1, Flat, Plain), "Cannot cast to DeliteArray")
        $self.asInstanceOf[Rep[ForgeArray[T]]]
      }
      internal.infix ("asFlatND") (Nil :: FlatND(T)) implements composite ${
        assert(layout($self).tpe == Flat, "Cannot cast to flat array implementation")
        $self.asInstanceOf[Rep[FlatND[T]]]
      }
    }
    val FlatNDOps = withTpe(FlatND)
    FlatNDOps {
      internal.infix ("data") (Nil :: MArray(T)) implements composite ${ field[ForgeArray[T]]($self, "data") }
    }

    // TODO: Add metadata propagation to this if required
    internal (FlatND) ("flat1d_new", T, MInt :: MArray(T)) implements composite ${ array_empty_imm[T]($0) }
    internal (FlatND) ("flat1d_apply", T, (MArray(T), MInt) :: T) implements composite ${ array_apply($0, $1) }

    internal (FlatND) ("flatnd_new", T, (MArray(T), SList(MInt)) :: FlatND(T)) implements
      record(FlatND(T), ("data", MArray(T), ${$0}), ("dim", SList(MInt), ${$1}))

    /**
     * Creates an ND view of a flat array
     * @param data    - Flat array to be viewed
     * @param ofs     - Flat offset for view of underlying data (always zero for non-views)
     * @param strides - Strides used to calculate actual flat indices
     * @param dims    - Number of elements contained along each dimension
     **/
    internal (FlatND) ("flatview_new", T, (MArray(T), MInt, SList(MInt), SList(MInt)) :: FlatND(T)) implements
      record(FlatND(T), ("data", MArray(T), ${$0}), ("ofs", MInt, ${$1}), ("stride", SList(MInt), ${$2}), ("dim", SList(MInt), ${$3}))

    internal (FlatND) ("flatview_fake", T, (MArray(T), SList(MInt)) :: FlatND(T)) implements composite ${
      flatview_new($0, unit(0), dimsToStrides($1), $1)
    }

    internal (FlatND) ("maflat_new", T, (SList(MInt), MLayout)  :: FlatND(T)) implements composite ${
      val array = $1 match {
        case MLayout(1,Flat,Plain) => flat1d_new[T]($0.head).asInstanceOf[Rep[FlatND[T]]]
        case MLayout(n,Flat,Plain) => flatnd_new[T](flat1d_new[T](productTree($0)), $0)
        case MLayout(n,Flat,View)  => flatview_fake[T](flat1d_new[T](productTree($0)), $0)  // TODO: Does this ever occur?
        case _ => throw new Exception("Don't know how to implement layout " + $1)
      }
      layout(array) = $1
      (array)
    }
    internal (FlatND) ("maflat_new_immutable", T, (SList(MInt), MLayout)  :: FlatND(T)) implements composite ${ maflat_new($0, $1) }

    internal (FlatND) ("maflat_view", T, (ArrayND(T), SList(Range), SList(SInt), MLayout)  :: FlatND(T)) implements composite ${
      if (layout($0).tpe == Flat && $3.tpe == Flat) {
        val offsets = $1.map(_.start)
        val strides = $1.map(_.stride)
        val dims    = $1.map(_.len)

        val newOfs = createFlatViewOfs($0.ofs, $0.strides, offsets)
        val newStrides = createFlatViewStrides(rank($0), $3.rank, $0.strides, strides, $2)

        val view = layout($0) match {
          case MLayout(1,Flat,Plain) => flatview_new($0.asFlat1D, newOfs, newStrides, dims)
          case MLayout(n,Flat,Plain) => flatview_new($0.asFlatND.data, newOfs, newStrides, dims)
          case MLayout(n,Flat,View)  => flatview_new($0.asFlatND.data, newOfs, newStrides, dims)
          case _ => throw new Exception("Don't know how to implement layout " + $3 + " from layout " + layout($0))
        }
        layout(view) = $3
        (view)
      }
      else throw new Exception("Target and view must both be flat.")
    }

    internal (FlatND) ("maflat_apply", T, (ArrayND(T), Indices) :: T) implements composite ${ layout($0) match {
      case MLayout(1,Flat,Plain) => flat1d_apply($0.asFlat1D, $1(0))
      case MLayout(n,Flat,Plain) => flat1d_apply($0.asFlatND.data, flattenIndices($1.toList(n), $0.ofs, $0.strides))
      case MLayout(n,Flat,View)  => flat1d_apply($0.asFlatND.data, flattenIndices($1.toList(n), $0.ofs, $0.strides))
      case lt => throw new Exception("Don't know how to implement layout " + lt)
    }}

    internal (FlatND) ("maflat_update", T, (ArrayND(T), Indices, T) :: MUnit, effect = write(0)) implements composite ${ layout($0) match {
      case MLayout(1,Flat,Plain) => array_update($0.asFlat1D, $1(0), $2)
      case MLayout(n,Flat,Plain) => array_update($0.asFlatND.data, flattenIndices($1.toList(n), $0.ofs, $0.strides), $2)
      // Technically shouldn't normally happen, but may be ok for accumulators in mutable reduce
      case MLayout(n,Flat,View)  => array_update($0.asFlatND.data, flattenIndices($1.toList(n), $0.ofs, $0.strides), $2)
    }}

  }
}
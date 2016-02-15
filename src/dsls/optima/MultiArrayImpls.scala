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
    internal (Utils) ("productTree", Nil, SList(MInt) :: MInt) implements composite ${
      reductionTree($0, {(a: Rep[Int],b: Rep[Int]) => a * b}).head
    }
    internal (Utils) ("dimsToStrides", Nil, SList(MInt) :: SList(MInt)) implements composite ${
      List.tabulate($0.length){d =>
        if (d == $0.length - 1) unit(1)
        else productTree($0.drop(d + 1))
      }
    }
    internal (Utils) ("flattenIndices", Nil, (("indices", SList(MInt)), ("ofs", MInt), ("stride", SList(MInt))) :: MInt) implements composite ${
      List.tabulate($indices.length){i => $indices(i)*$stride(i) }.reduce{_+_} + $ofs
    }
  }

  def importMultiArrayImpls() {
    val T = tpePar("T")

    val ArrayND = lookupTpe("ArrayND")
    val ImplND  = lookupTpe("ImplND")

    val ImplInfixOps = withTpe(ImplND)
    ImplInfixOps {
      // All MultiArray implementation versions are created using Records
      internal.infix ("dim") (SInt :: MInt) implements composite ${ maimpl_dim($self, $1) }
      internal.infix ("stride") (SInt :: MInt) implements composite ${ maimpl_stride($self, $1) }
      internal.infix ("ofs") (Nil :: MInt) implements composite ${ maimpl_ofs($self) }

      internal.infix ("dims") (Nil :: SList(MInt)) implements composite ${ maimpl_dims($self) }
      internal.infix ("strides") (Nil :: SList(MInt)) implements composite ${ maimpl_strides($self) }

      internal.infix ("size") (Nil :: MInt) implements composite ${ maimpl_size($self) }
    }

    val Impls = withTpe(ArrayND)
    Impls {
      internal ("maimpl_ofs") (Nil :: MInt) implements composite ${ field[Int]($self, "ofs") }
      internal ("maimpl_dim") (SInt :: MInt) implements composite ${ field[Int]($self, "dim_" + $1) }
      internal ("maimpl_stride") (SInt :: MInt) implements composite ${ field[Int]($self, "stride_" + $1) }
      internal ("maimpl_size") (Nil :: MInt) implements composite ${ productTree(maimpl_dims($self)) }

      internal ("maimpl_dims") (Nil :: SList(MInt)) implements composite ${ List.tabulate(rank($self)){i => maimpl_dim($self, i) } }
      internal ("maimpl_strides") (Nil :: SList(MInt)) implements composite ${ List.tabulate(rank($self)){i => maimpl_stride($self, i) } }
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
    val ArrayND = lookupTpe("ArrayND")
    val ImplND  = lookupTpe("ImplND")
    val FlatND  = lookupTpe("FlatND")
    val MLayout = lookupMeta("MLayout")

    val ArrayNDFlatOps = withTpe(ArrayND)
    ArrayNDFlatOps {
      internal.infix ("asFlat1D") (Nil :: MArray(T)) implements composite ${
        assert(layout($self) == FlatLayout(1, Plain), "Cannot cast to DeliteArray")
        $self.asInstanceOf[Rep[ForgeArray[T]]]
      }
      internal.infix ("asFlatND") (Nil :: FlatND(T)) implements composite ${
        assert(layout($self).layout == 0, "Cannot cast to flat array implementation")
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
     * @param data    - Full flat array to be viewed
     * @param ofs     - Flat offset for view of underlying data (always zero for non-views)
     * @param strides - Strides used to calculate actual flat indices
     * @param dims    - Number of elements contained along each dimension
     **/
    internal (FlatND) ("flatview_new", T, (MArray(T), MInt, SList(MInt), SList(MInt)) :: FlatND(T)) implements
      record(FlatND(T), ("data", MArray(T), ${$0}), ("ofs", MInt, ${$1}), ("stride", SList(MInt), ${$2}), ("dim", SList(MInt), ${$3}))

    internal (FlatND) ("flatview_fake", T, (MArray(T), SList(MInt)) :: FlatND(T)) implements composite ${
      flatview_new($0, unit(0), dimsToStrides($1), $1)
    }

    internal (FlatND) ("maflat_new", T, (SList(MInt), MLayout) :: FlatND(T)) implements composite ${
      val array = $1 match {
        case FlatLayout(1, 0) => flat1d_new[T]($0.head).asInstanceOf[Rep[FlatND[T]]]
        case FlatLayout(n, 0) => flatnd_new[T](flat1d_new[T](productTree($0)), $0)
        case FlatLayout(n, 1)  => flatview_fake[T](flat1d_new[T](productTree($0)), $0)  // TODO: Does this ever occur?
        case _ => throw new Exception("Don't know how to implement layout " + $1)
      }
      layout(array) = $1
      (array)
    }

    internal (FlatND) ("maflat_new_immutable", T, (SList(MInt), MLayout) :: FlatND(T)) implements composite ${ maflat_new($0, $1) }

    internal (FlatND) ("maflat_apply", T, (ArrayND(T), Indices) :: T) implements composite ${ layout($0) match {
      case FlatLayout(1, 0) => flat1d_apply($0.asFlat1D, $1(0))
      case FlatLayout(n, 0) =>
        val impl = $0.asFlatND
        flat1d_apply(impl.data, flattenIndices($1.toList(n), unit(0), dimsToStrides(impl.dims)))
      case FlatLayout(n, 1) =>
        val impl = $0.asFlatND
        flat1d_apply(impl.data, flattenIndices($1.toList(n), impl.ofs, impl.strides))
      case lt =>
        throw new Exception("Don't know how to implement layout " + lt)
    }}

  }
}
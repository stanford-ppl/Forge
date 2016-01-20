package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

trait MultiArrayOps { this: OptiMADSL =>

  def importMultiArrayOps() {
    val T = tpePar("T")
    val R = tpePar("R")

    val ArrayND = lookupTpe("ArrayND")
    val Array1D = lookupTpe("Array1D")
    val Array2D = lookupTpe("Array2D")
    val Array3D = lookupTpe("Array3D")

    // --- Utils
    // TODO: Move to a different file
    val Utils = grp("Utils")
    //compiler (Utils) ("list_zeros", T, IInt :: IList(MInt)) implements composite ${ List.fill($0)(unit(0)) }
    compiler (Utils) ("reductionTree", T, (IList(T), ((T,T) ==> T)) :: IList(T)) implements composite ${
      if ($0.length == 1) $0
      else if ($0.length % 2 == 0) reductionTree( List.tabulate($0.length/2){i => $1( $0(2*i), $0(2*i+1)) }, $1)
      else reductionTree( List.tabulate($0.length/2){i => $1( $0(2*i), $0(2*i+1)) } :+ $0.last, $1)
    }
    compiler (Utils) ("productTree", Nil, IList(MInt) :: MInt) implements composite ${
      reductionTree($0, {(a: Rep[Int],b: Rep[Int]) => a * b}).head
    }
    compiler (Utils) ("dimsToStrides", Nil, IList(MInt) :: IList(MInt)) implements composite ${
      List.tabulate($0.length){d =>
        if (d == $0.length - 1) unit(1)
        else productTree($0.drop(d + 1))
      }
    }
    compiler (Utils) ("flattenIndices", Nil, (("indices", IList(MInt)), ("ofs", MInt), ("stride", IList(MInt))) :: MInt) implements composite ${
      List.tabulate($indices.length){i => $indices(i)*$stride(i) }.reduce{_+_} + $ofs
    }
    library (Utils) ("flattenIndicesInto", T, (("indices", IList(MInt)), ("ma", ArrayND(T))) :: MInt) implements composite ${
      flattenIndices($indices, multia_ofs($ma), multia_stride($ma))
    }

    // --- Library implementation
    // Dimensions - Number of elements contained along each dimension
    // Offset     - Flat offset for view of underlying data (always zero for non-views)
    // Strides    - Strides used to calculate actual flat indices
    data(ArrayND, ("_data", MArray(T)), ("_dims", IList(MInt)), ("_ofs", MInt), ("_stride", IList(MInt)))
    data(Array3D, ("_data", MArray(T)), ("_dims", IList(MInt)), ("_ofs", MInt), ("_stride", IList(MInt)))
    data(Array2D, ("_data", MArray(T)), ("_dims", IList(MInt)), ("_ofs", MInt), ("_stride", IList(MInt)))
    data(Array1D, ("_data", MArray(T)), ("_dims", IList(MInt)), ("_ofs", MInt), ("_stride", IList(MInt)))

    // --- Library implementation accessors
    library (ArrayND) ("multia_data", T, ArrayND(T) :: MArray(T)) implements getter(0, "_data")
    library (ArrayND) ("multia_dims", T, ArrayND(T) :: IList(MInt)) implements getter(0, "_dims")
    library (ArrayND) ("multia_ofs", T, ArrayND(T) :: MInt) implements getter(0, "_ofs")
    library (ArrayND) ("multia_stride", T, ArrayND(T) :: IList(MInt)) implements getter(0, "_stride")

    // --- Array contructors
    library (ArrayND) ("multiaview_from_array", T, (MArray(T), IList(MInt), MInt, IList(MInt)) :: ArrayND(T)) implements allocates(ArrayND, ${$0}, ${$1}, ${$2}, ${$3})
    library (Array3D) ("array3dview_from_array", T, (MArray(T), IList(MInt), MInt, IList(MInt)) :: Array3D(T)) implements allocates(Array3D, ${$0}, ${$1}, ${$2}, ${$3})
    library (Array2D) ("array2dview_from_array", T, (MArray(T), IList(MInt), MInt, IList(MInt)) :: Array2D(T)) implements allocates(Array2D, ${$0}, ${$1}, ${$2}, ${$3})
    library (Array1D) ("array1dview_from_array", T, (MArray(T), IList(MInt), MInt, IList(MInt)) :: Array1D(T)) implements allocates(Array1D, ${$0}, ${$1}, ${$2}, ${$3})

    library (ArrayND) ("multia_from_array", T, (MArray(T), IList(MInt)) :: ArrayND(T)) implements figment ${
      multiaview_from_array($0, $1, unit(0), dimsToStrides($1))
    }
    // node constructors
    compiler (ArrayND) ("multia_new", T, IList(MInt) :: ArrayND(T)) implements figment ${
      multia_from_array(array_empty_imm[T]($0.reduce{_*_}), $0)
    }
    /*compiler (ArrayND) ("multia_view", T, (("target", (ArrayND(T))), ("lengths", IList(MInt)), ("ofs", IList(MInt)), ("stride", IList(MInt))) :: ArrayND(T)) implements figment ${

    }*/

    // --- Properties
    // UNUSED: compiler (ArrayND) ("multia_rank", T, ArrayND(T) :: MInt) implements figment ${ multia_dims($0).length }
    compiler (ArrayND) ("multia_size", T, ArrayND(T) :: MInt) implements figment ${ multia_dims($0).reduce{_*_} }
    compiler (ArrayND) ("multia_dim", T, (ArrayND(T), IInt) :: MInt) implements figment ${ multia_dims($0).apply($1) }

    // --- Rank casts
    // FIXME: In the library this is a shallow copy, while in the compiler this is just a cast
    // These should be made consistent somehow
    compiler (ArrayND) ("multia_as_1d", T, ArrayND(T) :: Array1D(T), aliasHint = aliases(0)) implements figment ${
      array1dview_from_array(multia_data($0), multia_dims($0), multia_ofs($0), multia_stride($0))
    }
    compiler (ArrayND) ("multia_as_2d", T, ArrayND(T) :: Array2D(T), aliasHint = aliases(0)) implements figment ${
      array2dview_from_array(multia_data($0), multia_dims($0), multia_ofs($0), multia_stride($0))
    }
    compiler (ArrayND) ("multia_as_3d", T, ArrayND(T) :: Array3D(T), aliasHint = aliases(0)) implements figment ${
      array3dview_from_array(multia_data($0), multia_dims($0), multia_ofs($0), multia_stride($0))
    }

    // --- Single element operators
    compiler (ArrayND) ("multia_apply", T, (ArrayND(T), IList(MInt)) :: T) implements figment ${
      val index = flattenIndicesInto($1, $0)
      multia_data($0).apply(index)
    }

    /*
    compiler (ArrayND) ("flatten_indices", Nil, (IList(MInt), IList(MInt)) :: MInt) implements composite ${
      Seq.tabulate($0.length){i =>
        if (i == $0.length - 1) $0(i)
        else $0(i) * $1.drop(i + 1).reduce{_*_}
      }.sum
    }
    compiler (ArrayND) ("flatten_indices", T, (IList(MInt), ArrayND(T)) :: MInt) implements composite ${
      val dims = multiarray_dims($1)
      flatten_indices($0, dims)
    }




    //compiler (ArrayND) ("array1d_new", T, IList(MInt) :: ArrayND(T)) implements
    // --- Single element ops
    compiler (ArrayND) ("multiarray_apply", T, (ArrayND(T), IList(MInt)) :: T) implements figment ${
      val flatIndex = flatten_indices($1, $0)
      multiarray_data($0).apply(flatIndex)
    }

    compiler (ArrayND) ("multiarray_update", T, (ArrayND(T), IList(MInt), T) :: MUnit, effect = write(0)) implements figment ${
      val flatIndex = flatten_indices($1, $0)
      val data = multiarray_data($0)
      data(flatIndex) = $2
    }

    compiler (ArrayND) ("multiarray_permute", T, (ArrayND(T), IList(IInt)) :: ArrayND(T)) implements figment ${

    }
    compiler (ArrayND) ("multiarray_reshape", T, (ArrayND(T), IList(MInt)) :: ArrayND(T)) implements figment ${

    }

    //static (Array1D) ("apply", T, MInt :: Array1D(T)) implements composite ${  }

    */
    // -----------------
    // Syntax sugar
    // -----------------
    static (ArrayND) ("apply", T, varArgs(MInt) :: ArrayND(T)) implements composite ${ multia_new[T]($0.toList) }
    static (Array3D) ("apply", T, (MInt, MInt, MInt) :: Array3D(T)) implements composite ${ multia_new[T](List($0, $1, $2)).as3D }
    static (Array2D) ("apply", T, (MInt, MInt) :: Array2D(T)) implements composite ${ multia_new[T](List($0, $1)).as2D }
    static (Array1D) ("apply", T, MInt :: Array1D(T)) implements composite ${ multia_new[T](List($0)).as1D }

    val ArrayNDOps = withTpe(ArrayND)
    ArrayNDOps {
      // --- Compiler shortcuts
      // Users shouldn't have access to these (how to restrict subset of infixes?)
      infix ("as3D") (Nil :: Array3D(T)) implements composite ${ multia_as_3d($self) }
      infix ("as2D") (Nil :: Array2D(T)) implements composite ${ multia_as_2d($self) }
      infix ("as1D") (Nil :: Array1D(T)) implements composite ${ multia_as_1d($self) }

      // --- Properties
      infix ("size") (Nil :: MInt) implements composite ${ multia_size($self) }
      infix ("nRows") (Nil :: MInt) implements composite ${ multia_dim($self, 0) }
      infix ("nCols") (Nil :: MInt) implements composite ${ multia_dim($self, 1) }
      infix ("dim") (IInt :: MInt) implements composite ${ multia_dim($self, $1) }
    }

    val Array1DOps = withTpe(Array1D)
    Array1DOps {
      infix ("length") (Nil :: MInt) implements composite ${ multia_size($self) }
    }

  }
}
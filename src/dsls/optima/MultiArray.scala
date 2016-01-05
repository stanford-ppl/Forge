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

    // --- Library implementation
    data(ArrayND, ("_data", MArray(T)), ("_dims", CSeq(MInt)), ("_ofs", CSeq(MInt)), ("_stride", CSeq(MInt)))
    data(Array3D, ("_data", MArray(T)), ("_dims", CSeq(MInt)), ("_ofs", CSeq(MInt)), ("_stride", CSeq(MInt)))
    data(Array2D, ("_data", MArray(T)), ("_dims", CSeq(MInt)), ("_ofs", CSeq(MInt)), ("_stride", CSeq(MInt)))
    data(Array1D, ("_data", MArray(T)), ("_dims", CSeq(MInt)), ("_ofs", CSeq(MInt)), ("_stride", CSeq(MInt)))

    compiler (ArrayND) ("multiarray_data", T, ArrayND(T) :: MArray(T)) implements getter(0, "_data")
    compiler (ArrayND) ("multiarray_dims", T, ArrayND(T) :: CSeq(MInt)) implements getter(0, "_dims")
    compiler (ArrayND) ("multiarray_ofs", T, ArrayND(T) :: CSeq(MInt)) implements getter(0, "_ofs")
    compiler (ArrayND) ("multiarray_stride", T, ArrayND(T) :: CSeq(MInt)) implements getter(0, "_stride")

    compiler (ArrayND) ("flatten_indices", Nil, (CSeq(MInt), CSeq(MInt)) :: MInt) implements composite ${
      Seq.tabulate($0.length){i =>
        if (i == $0.length - 1) $0(i)
        else $0(i) * $1.drop(i + 1).reduce{_*_}
      }.sum
    }
    compiler (ArrayND) ("flatten_indices", T, (CSeq(MInt), ArrayND(T)) :: MInt) implements composite ${
      val dims = multiarray_dims($1)
      flatten_indices($0, dims)
    }
    compiler (ArrayND) ("seq_zeros", T, IInt :: CSeq(MInt)) implements composite ${ Seq.fill($0)(unit(0)) }


    // --- Rank casts
    // FIXME: These aren't correct yet (for lib or for compiler versions)
    compiler (ArrayND) ("multiarray_as_1D", T, ArrayND(T) :: Array1D(T), aliasHint = aliases(0)) implements figment ${ $0.asInstanceOf[Rep[Array1D[T]]] }
    compiler (ArrayND) ("multiarray_as_2D", T, ArrayND(T) :: Array2D(T), aliasHint = aliases(0)) implements figment ${ $0.asInstanceOf[Rep[Array2D[T]]] }
    compiler (ArrayND) ("multiarray_as_3D", T, ArrayND(T) :: Array3D(T), aliasHint = aliases(0)) implements figment ${ $0.asInstanceOf[Rep[Array3D[T]]] }

    // --- Array contructors
    // library-only helpers
    compiler (ArrayND) ("multiarrayview_fromarray", T, (MArray(T), CSeq(MInt), CSeq(MInt), CSeq(MInt)) :: ArrayND(T)) implements allocates(ArrayND, ${$0}, ${$1}, ${$2}, ${$3})
    compiler (ArrayND) ("multiarray_fromarray", T, (MArray(T), CSeq(MInt)) :: ArrayND(T)) implements figment ${
      multiarrayview_fromarray($0, $1, seq_zeros($0.length), seq_zeros($0.length))
    }

    // node constructors
    compiler (ArrayND) ("multiarray_new", T, CSeq(MInt) :: ArrayND(T)) implements figment ${
      multiarray_fromarray(array_empty_imm[T]($0.reduce{_*_}), $0)
    }
    compiler (ArrayND) ("multiarray_view", T, (("target", (ArrayND(T))), ("lengths", CSeq(MInt)), ("ofs", CSeq(MInt)), ("stride", CSeq(MInt))) :: ArrayND(T)) implements figment ${

    }
    //compiler (ArrayND) ("array1d_new", T, CSeq(MInt) :: ArrayND(T)) implements

    // --- Properties
    compiler (ArrayND) ("multiarray_rank", T, ArrayND(T) :: MInt) implements figment ${ multiarray_dims($0).length }
    compiler (ArrayND) ("multiarray_size", T, ArrayND(T) :: MInt) implements figment ${ multiarray_dims($0).reduce{_*_} }
    compiler (ArrayND) ("multiarray_dim", T, (ArrayND(T), CInt) :: MInt) implements figment ${ multiarray_dims($0).apply($1) }

    // --- Single element ops
    compiler (ArrayND) ("multiarray_apply", T, (ArrayND(T), CSeq(MInt)) :: T) implements figment ${
      val flatIndex = flatten_indices($1, $0)
      multiarray_data($0).apply(flatIndex)
    }

    compiler (ArrayND) ("multiarray_update", T, (ArrayND(T), CSeq(MInt), T) :: MUnit, effect = write(0)) implements figment ${
      val flatIndex = flatten_indices($1, $0)
      val data = multiarray_data($0)
      data(flatIndex) = $2
    }

    compiler (ArrayND) ("multiarray_permute", T, (ArrayND(T), CSeq(IInt)) :: ArrayND(T)) implements figment ${

    }
    compiler (ArrayND) ("multiarray_reshape", T, (ArrayND(T), CSeq(MInt)) :: ArrayND(T)) implements figment ${

    }

    //static (Array1D) ("apply", T, MInt :: Array1D(T)) implements composite ${  }


    // -----------------
    // Syntax sugar
    // -----------------
    static (ArrayND) ("apply", T, varArgs(MInt) :: ArrayND(T)) implements composite ${ multiarray_new[T]($0.toSeq) }



    val ArrayNDOps = withTpe(ArrayND)
    val Array3DOps = withTpe(Array3D)
    val Array2DOps = withTpe(Array2D)
    val Array1DOps = withTpe(Array1D)




  }
}
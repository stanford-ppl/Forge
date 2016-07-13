package ppl.dsl.forge
package dsls
package dhdl

@dsl
trait DHDLBRAMs {
  this: DHDLDSL =>

  // TODO: Should we support a BRAM reset API? How to time this properly? Should this be a composite which creates a Pipe?
  def importBRAM() {
    val T = tpePar("T")
    val BRAM         = lookupTpe("BRAM")
    val Tile         = lookupTpe("Tile")
    val SparseTile   = lookupTpe("SparseTile")
    val Indices      = lookupTpe("Indices")
    val Range        = lookupTpe("Range")
    val MVector      = lookupTpe("Vector")
    val CounterChain = lookupTpe("CounterChain")
    val Idx          = lookupAlias("Index")

    // --- Nodes
    val bram_new = internal (BRAM) ("bram_new", T, (("size", Idx), ("zero", T)) :: BRAM(T), effect = mutable)
    val bram_load = internal (BRAM) ("bram_load", T, (("bram", BRAM(T)), ("addr", Idx)) :: T, aliasHint = aliases(Nil))
    val bram_store = internal (BRAM) ("bram_store", T, (("bram", BRAM(T)), ("addr", Idx), ("value", T)) :: MUnit, effect = write(0), aliasHint = aliases(Nil))
    //val bram_reset = internal (BRAM) ("bram_reset", T, (("bram", BRAM(T)), ("zero", T)) :: MUnit, effect = write(0))

    // --- Internals
    /** @nodoc -- only used for Mem typeclass instance **/
    direct (BRAM) ("bram_load_nd", T, (BRAM(T), SList(Idx)) :: T) implements composite ${
      if ($1.length < 1) stageError("Cannot load from zero indices")
      val addr = calcAddress($1, dimsOf($0))
      val ld = bram_load($0, addr)

      if ($1.length == 1 && dimsOf($0).length > 1 && accessIndicesOf($1.head).nonEmpty)
        accessIndicesOf(ld) = accessIndicesOf($1.head)
      else
        accessIndicesOf(ld) = $1
      ld
    }
    /** @nodoc -- only used for Mem typeclass instance **/
    direct (BRAM) ("bram_store_nd", T, (BRAM(T), SList(Idx), T) :: MUnit, effect = write(0)) implements composite ${
      if ($1.length < 1) stageError("Cannot store to zero indices")
      val addr = calcAddress($1, dimsOf($0))
      val st = bram_store($0, addr, $2)

      if ($1.length == 1 && dimsOf($0).length > 1 && accessIndicesOf($1.head).nonEmpty)
        accessIndicesOf(st) = accessIndicesOf($1.head)
      else
        accessIndicesOf(st) = $1
      st
    }

    /** @nodoc -- only used for Mem typeclass instance **/
    direct (BRAM) ("bram_zero_idx", T, BRAM(T) :: Indices) implements composite ${ indices_create(List.fill(dimsOf($0).length){0.as[Index]}) }

    /** @nodoc -- only used for Mem typeclass instance **/
    direct (BRAM) ("bram_calc_addr", T, (BRAM(T), Indices) :: Idx) implements composite ${ calcAddress($1.toList, dimsOf($0)) }

    /** @nodoc -- only used for Mem typeclass instance **/
    direct (BRAM) ("bram_iterator", T, (BRAM(T), SList(MInt)) :: CounterChain) implements composite ${
      val dims = dimsOf($0)
      val pars = List.fill(dims.length - $1.length)(param(1)) ++ $1
      val ctrs = dims.zip(pars).map{case (d,p) => Counter(min = 0, max = d, step = 1, par = p) }
      CounterChain(ctrs:_*)
    }

    /** @nodoc -- only used for Mem typeclass instance **/
    direct (BRAM) ("bram_empty", T, BRAM(T) :: BRAM(T), TNum(T)) implements composite ${ BRAM[T](dimsOf($0):_*) }

    val Mem = lookupTpeClass("Mem").get
    val BramMem = tpeClassInst("BramMem", T, TMem(T, BRAM(T)))
    infix (BramMem) ("ld", T, (BRAM(T), Idx) :: T) implements composite ${ bram_load_nd($0, List($1)) }
    infix (BramMem) ("st", T, (BRAM(T), Idx, T) :: MUnit, effect = write(0)) implements composite ${ bram_store_nd($0, List($1), $2) }
    infix (BramMem) ("zeroIdx", T, (BRAM(T)) :: Indices) implements composite ${ bram_zero_idx($0) }
    infix (BramMem) ("flatIdx", T, (BRAM(T), Indices) :: Idx) implements composite ${ bram_calc_addr($0, $1) }
    infix (BramMem) ("iterator", T, (BRAM(T), SList(MInt)) :: CounterChain) implements composite ${ bram_iterator($0, $1) }
    infix (BramMem) ("empty", T, BRAM(T) :: BRAM(T), TNum(T)) implements composite ${ bram_empty($0) }


    // --- API
    /** Creates a BRAM with given dimensions. Dimensions must be statically known signed integers (constants or parameters).
     * @param dims
     **/
    static (BRAM) ("apply", T, (varArgs(Idx)) :: BRAM(T), TNum(T)) implements composite ${
      if ($0.length < 1) stageError("Cannot create a BRAM with no dimensions")
      $0.foreach{dim => if (!isStaticSize(dim)) stageError("Only constants and DSE parameters are allowed as dimensions of BRAM") }

      val bram = bram_new[T](productTree($0.toList), zero[T])
      dimsOf(bram) = $0.toList
      bram
    }

    val BRAM_API = withTpe(BRAM)
    BRAM_API {
      /** Creates a read from this BRAM at the given multi-dimensional address. Number of indices given can either be 1 or the
       * same as the number of dimensions that the BRAM was declared with.
       * @param ii: multi-dimensional address
       **/
      infix ("apply") (varArgs(Idx) :: T) implements composite ${ bram_load_nd($self, $1.toList) }

      /*infix ("load") (Range :: MVector(T)) implements composite ${
        bramLoadVector($self, List($1.start), $1.len, param(1))
      }
      infix ("load") ((Idx, Range) :: MVector(T)) implements composite ${
        bramLoadVector($self, List($1,$2.start), $2.len, param(1))
      }
      infix ("load") ((Idx, Idx, Range) :: MVector(T)) implements composite ${
        bramLoadVector($self, List($1,$3.start), $3.len, param(1))
      }*/

      /* Store */
      /** Creates a write to this BRAM at the given 1D address.
       * @param i: 1D address
       * @param x: element to be stored to BRAM
       **/
      infix ("update") ((Idx, T) :: MUnit, effect = write(0)) implements composite ${ bram_store_nd($self, List($1), $2) }
      /** Creates a write to this BRAM at the given 2D address. The BRAM must have initially been declared as 2D.
       * @param i: row index
       * @param j: column index
       * @param x: element to be stored to BRAM
       **/
      infix ("update") ((Idx, Idx, T) :: MUnit, effect = write(0)) implements composite ${ bram_store_nd($self, List($1, $2), $3) }
      /** Creates a write to this BRAM at the given 3D address. The BRAM must have initially been declared as 3D.
       * @param i: row index
       * @param j: column index
       * @param k: page index
       * @param x: element to be stored to BRAM
       **/
      infix ("update") ((Idx, Idx, Idx, T) :: MUnit, effect = write(0)) implements composite ${ bram_store_nd($self, List($1, $2, $3), $4) }

      /** Creates a write to this BRAM at the given multi-dimensional address. The number of indices given can either be 1 or the
       * same as the number of dimensions that the BRAM was declared with.
       * @param ii: multi-dimensional index
       * @param x: element to be stored to BRAM
       **/
      infix ("update") ((SList(Idx), T) :: MUnit, effect = write(0)) implements composite ${ bram_store_nd($self, $1.toList, $2) }

      /** @nodoc - BRAM reset is not yet well defined **/
      //infix ("rst") (Nil :: MUnit, TNum(T), effect = write(0)) implements composite ${ bram_reset($self, zero[T]) }

      /** Stores a Tile of an OffChipMem to this BRAM.
       * @param tile
       **/
      infix (":=") (Tile(T) :: MUnit, TNum(T), effect = write(0)) implements redirect ${ copyTile($1, $self, false) }

      /*infix (":=") (MVector(T) :: MUnit, effect = write(0)) implements composite ${
        bramStoreVector($self, dimsOf($self).map(dim => 0.as[Index]), $1, param(1))
      }*/

      /** Gathers the values from the supplied SparseTile into this BRAM
       * @param tile
       **/
      infix (":=") (SparseTile(T) :: MUnit, effect = write(0)) implements redirect ${ copySparse($1, $self, false) }
    }

    // --- Scala Backend
    impl (bram_new)   (codegen($cala, ${ Array.fill($size.toInt)($zero) })) // $t[T] refers to concrete type in IR
    impl (bram_load)  (codegen($cala, ${ $bram.apply($addr.toInt) }))
    impl (bram_store) (codegen($cala, ${ $bram.update($addr.toInt, $value) }))
    //impl (bram_reset) (codegen($cala, ${ (0 until $bram.length).foreach{i => $bram.update(i, $zero) }}))

    // --- C++ Backend
    impl (bram_new) (codegen(cpp, ${new $t[T]($size) }))

    // --- MaxJ Backend
    // bram_new (extern)
    // bram_load (extern)
    // bram_store (extern)
    // impl (bram_reset) (codegen(maxj, ${ })) TODO: removed from dhdl?


    // --- Unrolled nodes
    val load = internal (BRAM) ("par_bram_load", T, (("bram", BRAM(T)), ("addr", MVector(Idx))) :: MVector(T), aliasHint = aliases(Nil))
    val store = internal (BRAM) ("par_bram_store", T, (("bram", BRAM(T)), ("addr", MVector(Idx)), ("value", MVector(T))) :: MUnit, effect = write(0), aliasHint = aliases(Nil))

    impl (load) (codegen($cala, ${
      $addr.map{addr => if (addr.toInt < $bram.length) $bram(addr.toInt) else $bram(0) }
    }))

    impl (store) (codegen($cala, ${
      $value.zip($addr).foreach{ case (v,a) => if (a.toInt < $bram.length) $bram(a.toInt) = v }
    }))

  }
}

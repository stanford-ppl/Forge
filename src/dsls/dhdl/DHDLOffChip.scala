package ppl.dsl.forge
package dsls
package dhdl

@dsl
trait DHDLOffChip {
  this: DHDLDSL =>

  // TODO: Size of offchip memory can be a staged value, but it can't be a value which is calculated in hardware
  //       Any way to make this distinction?
  // TODO: Change interface of tile load / store to words rather than BRAMs?
  def importOffChip() {
    val T = tpePar("T")

    val OffChip = lookupTpe("OffChipMem")
    val Tile    = lookupTpe("Tile")
    val BRAM    = lookupTpe("BRAM")
    val Range   = lookupTpe("Range")
    val MVector = lookupTpe("Vector")
    val Idx     = lookupAlias("Index")

    // --- Nodes
    val offchip_new   = internal (OffChip) ("offchip_new", T, ("size", Idx) :: OffChip(T), effect = mutable)
    val offchip_load  = internal (OffChip) ("offchip_load_vector", T, (("mem",OffChip(T)), ("ofs",Idx), ("len",Idx)) :: MVector(T))
    val offchip_store = internal (OffChip) ("offchip_store_vector", T, (("mem",OffChip(T)), ("ofs",Idx), ("vec",MVector(T))) :: MUnit, effect = write(0))

    // --- Internals
    internal (OffChip) ("offchipCreate", T, (SOption(SString), SList(Idx)) :: OffChip(T)) implements composite ${
      if ($1.length < 1) stageError("Cannot create an OffChipMem with zero dimensions")
      val offchip = offchip_new[T](productTree($1))
      $0.foreach{name => nameOf(offchip) = name }
      dimsOf(offchip) = $1
      offchip
    }

    internal (OffChip) ("offchipLoadVector", T, (("mem",OffChip(T)), ("ofs",Idx), ("len",Idx)) :: MVector(T)) implements composite ${
      val vec = offchip_load_vector($mem,$ofs,$len)
      dimsOf(vec) = List($len)
      vec
    }
    internal (OffChip) ("offchipStoreVector", T, (("mem",OffChip(T)), ("ofs",Idx), ("vec",MVector(T))) :: MUnit, effect = write(0)) implements composite ${
      offchip_store_vector($mem,$ofs,$vec)
    }


    // --- API
    /** Creates a reference to a multi-dimensional array in main memory with given name and dimensions
     * @param name
     * @param dims
     **/
    static (OffChip) ("apply", T, (SString, varArgs(Idx)) :: OffChip(T), TNum(T)) implements composite ${ offchipCreate(Some($0), $1.toList) }
    /** Creates a reference to an unnamed multi-dimensional array in main memory with given dimensions
     * @param dims
     **/
    static (OffChip) ("apply", T, (varArgs(Idx)) :: OffChip(T), TNum(T)) implements composite ${ offchipCreate(None, $0.toList) }

    // Offer multiple versions of tile select since implicit cast from signed int to range isn't working
    val OffChip_API = withTpe(OffChip)
    OffChip_API {
      /** Loads a fixed size tile of this 1D OffChipMem onto an on-chip MVector.
       * @param cols
       **/
      infix ("load") (Range :: MVector(T)) implements composite ${ offchipLoadVector($self, $1.start, $1.len) }

      /** Loads a fixed size 1D column tile of this 2D OffChipMem onto an on-chip MVector.
       * @param row
       * @param cols
       **/
      infix ("load") ((Idx, Range) :: MVector(T)) implements composite ${
        offchipLoadVector($self, calcAddress(List($1, $2.start), dimsOf($self)), $2.len)
      }

      /** Loads a fixed size 1D page tile of this 3D OffChipMem onto an on-chip MVector.
       * @param row
       * @param col
       * @param pages
       **/
      infix ("load") ((Idx,Idx,Range) :: MVector(T)) implements composite ${
        offchipLoadVector($self, calcAddress(List($1, $2, $3.start), dimsOf($self)), $3.len)
      }


      /** Creates a reference to a 1D Tile of this 1D OffChipMem which can be loaded into on-chip BRAM.
       * @param cols
       **/
      infix ("apply") (Range :: Tile(T)) implements composite ${ tile_create($self, List($1)) }
      /** Creates a reference to a 2D Tile of this 2D OffChipMem which can be loaded into on-chip BRAM.
       * @param rows
       * @param cols
       **/
      infix ("apply") ((Range,Range) :: Tile(T)) implements composite ${ tile_create($self, List($1,$2)) }
      /** Creates a reference to a 3D Tile of this 3D OffChipMem which can be loaded into on-chip BRAM.
       * @param rows
       * @param cols
       * @param pages
       **/
      infix ("apply") ((Range,Range,Range) :: Tile(T)) implements composite ${ tile_create($self, List($1,$2,$3)) }

      // Hack version for adding explicit parallelization factors to a tile load / store
      /** @nodoc - syntax needs some tweaking here **/
      infix ("apply") ((Range,MInt) :: Tile(T)) implements composite ${
        val tile = tile_create($self, List($1))
        tilePar(tile) = $2
        tile
      }
      /** @nodoc - syntax needs some tweaking here **/
      infix ("apply") ((Range,Range,MInt) :: Tile(T)) implements composite ${
        val tile = tile_create($self, List($1,$2))
        tilePar(tile) = $3
        tile
      }
      /** @nodoc - syntax needs some tweaking here **/
      infix ("apply") ((Range,Range,Range,MInt) :: Tile(T)) implements composite ${
        val tile = tile_create($self, List($1,$2,$3))
        tilePar(tile) = $4
        tile
      }

      // 2D -> 1D
      /** Creates a reference to a 1D row Tile of this 2D OffChipMem
       * @param row
       * @param cols
       **/
      infix ("apply") ((Idx, Range) :: Tile(T)) implements composite ${ tile_create($self, List(unitRange($1), $2)) }
      /** Creates a reference to a 1D column Tile of this 2D OffChipMem
       * @param rows
       * @param col
       **/
      infix ("apply") ((Range, Idx) :: Tile(T)) implements composite ${ tile_create($self, List($1, unitRange($2))) }

      // 3D -> 2D
      /** Creates a reference to a 2D column/page Tile of this 3D OffChipMem
       * @param row
       * @param cols
       * @param pages
       **/
      infix ("apply") ((Idx, Range, Range) :: Tile(T)) implements composite ${ tile_create($self, List(unitRange($1), $2, $3)) }
      /** Creates a reference to a 2D row/page Tile of this 3D OffChipMem
       * @param rows
       * @param col
       * @param pages
       **/
      infix ("apply") ((Range, Idx, Range) :: Tile(T)) implements composite ${ tile_create($self, List($1, unitRange($2), $3)) }
      /** Creates a reference to a 2D row/column Tile of this 3D OffChipMem
       * @param rows
       * @param cols
       * @param page
       **/
      infix ("apply") ((Range, Range, Idx) :: Tile(T)) implements composite ${ tile_create($self, List($1, $2, unitRange($3))) }

      // 3D -> 1D
      /** Creates a reference to a 1D page Tile of this 3D OffChipMem
       * @param row
       * @param col
       * @param pages
       **/
      infix ("apply") ((Idx, Idx, Range) :: Tile(T)) implements composite ${ tile_create($self, List(unitRange($1), unitRange($2), $3)) }
      /** Creates a reference to a 1D column Tile of this 3D OffChipMem
       * @param row
       * @param cols
       * @param page
       **/
      infix ("apply") ((Idx, Range, Idx) :: Tile(T)) implements composite ${ tile_create($self, List(unitRange($1), $2, unitRange($3))) }
      /** Creates a reference to a 1D row Tile of this 3D OffChipMem
       * @param rows
       * @param col
       * @param page
       **/
      infix ("apply") ((Range, Idx, Idx) :: Tile(T)) implements composite ${ tile_create($self, List($1, unitRange($2), unitRange($3))) }
    }

    // --- Scala Backend
    impl (offchip_new) (codegen($cala, ${ new Array[$t[T]]($size.toInt) }))
    impl (offchip_load) (codegen($cala, ${ $mem.slice($ofs.toInt, ($ofs+$len).toInt) }))
    impl (offchip_store) (codegen($cala, ${
      for (i <- 0 until $vec.length) { $mem(i + $ofs.toInt) = $vec(i) }
    }))

    val lbl = "\"\\\"\" + quote(sym)"
    val endlbl = "\"\\\"\""

    // --- Dot Backend
    impl (offchip_new) (codegen(dot, ${
      @ alwaysGen {
        @ var label = \$lbl
        @ if (quote(size).forall(_.isDigit)) {
          @   label += ", size=" + quote(size)
        @ } else {
          $size -> $sym [ headlabel="size" ]
        @ }
        @ label += \$endlbl
        $sym [ label=$label shape="square" fontcolor="white" color="white" style="filled"
        fillcolor=$dramFillColor color=black]
      @ }
    }))

    // --- MaxJ Backend
    //offchip_new (extern)
  }


  def importTiles() {
    val T = tpePar("T")

    val Tile    = lookupTpe("Tile")
    val OffChip = lookupTpe("OffChipMem")
    val BRAM    = lookupTpe("BRAM")
    val Range   = lookupTpe("Range")

    // TODO: How to avoid CSE? Doesn't matter except that same symbol may be returned
    // and need different symbols to manage offset staging metadata properly
    data(Tile, ("_target", OffChip(T)))
    internal (Tile) ("tile_new", T, OffChip(T) :: Tile(T)) implements allocates(Tile, ${$0})
    internal (Tile) ("tile_create", T, (OffChip(T), SList(Range)) :: Tile(T)) implements composite ${
      if (dimsOf($0).length != $1.length) stageError("Attempting to access " + dimsOf($0).length + "D memory with " + $1.length + " indices")
      val tile = tile_new($0)
      rangesOf(tile) = $1
      tile
    }
    internal.infix (Tile) ("mem", T, Tile(T) :: OffChip(T)) implements getter(0, "_target")


    /** Creates a store from the given on-chip BRAM to this Tile of off-chip memory
     * @param bram
     **/
    infix (Tile) (":=", T, (Tile(T), BRAM(T)) :: MUnit, effect = write(0)) implements redirect ${ transferTile($0, $1, true) }

    /** @nodoc - not actually a user-facing method for now **/
    direct (Tile) ("transferTile", T, (("tile",Tile(T)), ("local",BRAM(T)), ("store", SBoolean)) :: MUnit, effect = simple) implements composite ${
      val mem      = $tile.mem
      val ranges   = rangesOf($tile)
      val offsets  = ranges.map(_.start)
      val unitDims = ranges.map(isUnit(_))
      val tileDims = ranges.map(_.len)

      val cmdCtrs = if (tileDims.length > 1) tileDims.take(tileDims.length - 1).map{d => Counter(max = d) } else List(Counter(max = 1.as[Index]))

      val par = tilePar($tile).getOrElse(param(1))

      Pipe(CounterChain(cmdCtrs:_*)).forIndices{indices =>
        val inds = indices.toList :+ 0.as[Index]
        val indsDropUnits = inds.zip(unitDims).flatMap{case (i,isUnitDim) => if (!isUnitDim) Some(i) else None}

        val memOfs = calcAddress(offsets.zip(inds).map{case (a,b) => a + b}, dimsOf(mem))
        val localOfs = calcAddress(indsDropUnits, dimsOf(local))

        if ($store) {
          val vec = bramLoadVector(local, localOfs, tileDims.last, par) // Load MVector from BRAM
          offchipStoreVector(mem, memOfs, vec)  // Store to offchip
        }
        else {
          val vec = offchipLoadVector(mem, memOfs, tileDims.last)  // Load MVector from offchip
          bramStoreVector(local, localOfs, vec, par) // Store to BRAM
        }
      }
    }
  }

}

package ppl.dsl.forge
package dsls
package dhdl

trait DHDLMemories {
	this: DHDLDSL =>

  object TMem extends TypeClassSignature {
    def name = "Mem"
    def prefix = "_m"
    def wrapper = None
  }

  def importDHDLMemories() {
    importMemOps()
    importRegs()
    importBRAM()
    importOffChip()
  }

  // Type class for local memories which can be used as accumulators in reductions
  def importMemOps() {
    val Indices = lookupTpe("Indices")
    val T = tpePar("T")       // data type
    val C = hkTpePar("C", T)  // memory type

    val Mem = tpeClass("Mem", TMem, (T, C))
    infix (Mem) ("ld", (T,C), (C, Indices) :: T)
    infix (Mem) ("st", (T,C), (C, Indices, T) :: MUnit, effect = write(0))
  }

  // TODO: ArgIn should be immutable (shouldn't be able to write to an input argument)
  // TODO: Register initial values should be restricted to a constant value
  // TODO: Should we allow ArgIn / ArgOut with no given name? Way of automatically numbering them instead?
  // TODO: Better / more correct way of exposing register reset?
  // TODO: Should we allow general access of initial value of register using reg.init? Only for debugging?
  // TODO: Add implicit reset in the scope in which a register is created?
  // TODO: Implicit from Reg to Reg.value?
  def importRegs() {
    val T = tpePar("T")
    val Reg = lookupTpe("Reg")
    val RegTpe = lookupTpe("RegTpe", stage=compile)
    val Indices = lookupTpe("Indices")

    // --- Nodes
    val reg_new   = direct (Reg) ("reg_new", T, ("init", T) :: Reg(T), effect = mutable)
    val reg_read  = direct (Reg) ("reg_read", T, ("reg", Reg(T)) :: T)
    val reg_write = direct (Reg) ("reg_write", T, (("reg", Reg(T)), ("value", T)) :: MUnit, effect = write(0))
    val reg_reset = direct (Reg) ("reg_reset", T, ("reg", Reg(T)) :: MUnit, effect = write(0))

    // --- Internals
    internal (Reg) ("reg_create", T, (SOption(SString), T, RegTpe) :: Reg(T), effect = mutable) implements composite ${
      val reg = reg_new[T](init = $1)
      $0.foreach{name => nameOf(reg) = name }
      dblbuf(reg) = false
      regtpe(reg) = $2
      resetValue(reg) = $1
      reg
    }

    val RegMem = tpeClassInst("RegMem", T, TMem(T, Reg(T)))
    infix (RegMem) ("ld", T, (Reg(T), Indices) :: T) implements composite ${ reg_read($0) } // Ignore address
    infix (RegMem) ("st", T, (Reg(T), Indices, T) :: MUnit, effect = write(0)) implements composite ${ reg_write($0, $2) }

    // --- API
    /* Reg */
    static (Reg) ("apply", T, (("name", SString), ("init", T)) :: Reg(T), TNum(T)) implements composite ${ reg_create[T](Some($0), $1, Regular) }
    static (Reg) ("apply", T, ("name", SString) :: Reg(T), TNum(T)) implements composite ${ reg_create[T](Some($0), zero[T], Regular) }
    static (Reg) ("apply", T, ("init", T) :: Reg(T), TNum(T)) implements composite ${ reg_create[T](None, $0, Regular) }
    static (Reg) ("apply", T, Nil :: Reg(T), TNum(T)) implements composite ${ reg_create[T](None, zero[T], Regular) }

    /* ArgIn */
    direct (Reg) ("ArgIn", T, (SString, T) :: T, TNum(T)) implements composite ${ reg_create[T](Some($0), $1, ArgIn).value }
    direct (Reg) ("ArgIn", T, SString :: T, TNum(T)) implements composite ${ reg_create[T](Some($0), zero[T], ArgIn).value }
    direct (Reg) ("ArgIn", T, T :: T, TNum(T)) implements composite ${ reg_create[T](None, $0, ArgIn).value }
    direct (Reg) ("ArgIn", T, Nil :: T, TNum(T)) implements composite ${ reg_create[T](None, zero[T], ArgIn).value }

    /* ArgOut */
    direct (Reg) ("ArgOut", T, SString :: Reg(T), TNum(T)) implements composite ${ reg_create[T](Some($0), zero[T], ArgOut) }
    direct (Reg) ("ArgOut", T, Nil :: Reg(T), TNum(T)) implements composite ${ reg_create[T](None, zero[T], ArgOut) }

    val Reg_API = withTpe(Reg)
    Reg_API {
      infix ("value") (Nil :: T) implements composite ${ reg_read($self) }
      //infix ("init") (Nil :: T) implements composite ${ resetValue($self) }
      infix (":=") (T :: MUnit, effect = write(0)) implements composite ${ reg_write($self, $1) }
      infix ("rst") (Nil :: MUnit, effect = write(0)) implements composite ${ reg_reset($self) }
    }

    // --- Scala backend
    impl (reg_new)   (codegen($cala, ${ Array($init) }))
    impl (reg_read)  (codegen($cala, ${ $reg.apply(0) }))
    impl (reg_write) (codegen($cala, ${ $reg.update(0, $value) }))
    impl (reg_reset) (codegen($cala, ${
      @ val init = resetValue($reg)
      $reg.update(0, $init)
    }))
  }

  // TODO: Generalize definition of BRAM store to be equivalent to St node in original DHDL?
  // TODO: Check precision to make sure load/store address is an integer
  // TODO: Should dimensionality mismatches be errors or warnings? Either way, need source context
  // TODO: Should we support a BRAM reset API? How to time this properly? Should this be a composite which creates a Pipe?
  def importBRAM() {
    val T = tpePar("T")
    val BRAM = lookupTpe("BRAM")
		val Fix  = lookupTpe("Fix")
    val Indices = lookupTpe("Indices")

    // --- Nodes
    val bram_new = internal (BRAM) ("bram_new", T, ("size", SInt) :: BRAM(T), effect = mutable)
    val bram_load = internal (BRAM) ("bram_load", T, (("bram", BRAM(T)), ("addr", Fix)) :: T)
    val bram_store = internal (BRAM) ("bram_store", T, (("bram", BRAM(T)), ("addr", Fix), ("value", T)) :: MUnit, effect = write(0))
    val bram_reset = internal (BRAM) ("bram_reset", T, (("bram", BRAM(T)), ("zero", T)) :: MUnit, effect = write(0))
    val bram_mkstring = internal (BRAM) ("bram_makestring", T, BRAM(T) :: MString)

    // --- Internals
    internal (BRAM) ("bram_create", T, (SOption(SString), SList(SInt)) :: BRAM(T)) implements composite ${
      val bram = bram_new[T]($1.reduce(_*_))
      sizeOf(bram) = $1
      $0.foreach{name => nameOf(bram) = name }
      dblbuf(bram) = false
      banks(bram)  = 1
      bram
    }

    internal (BRAM) ("bram_load_nd", T, (BRAM(T), SList(Fix)) :: T) implements composite ${
      val addr = calcAddress($1, sizeOf($0))
      bram_load($0, addr)
    }
    internal (BRAM) ("bram_store_nd", T, (BRAM(T), SList(Fix), T) :: MUnit, effect = write(0)) implements composite ${
      val addr = calcAddress($1, sizeOf($0))
      bram_store($0, addr, $2)
    }

    direct (BRAM) ("bram_load_inds", T, (BRAM(T), Indices) :: T) implements composite ${
      val inds = $1.toList(sizeOf($0).length)
      bram_load_nd($0, inds)
    }
    direct (BRAM) ("bram_store_inds", T, (BRAM(T), Indices, T) :: MUnit, effect = write(0)) implements composite ${
      val inds = $1.toList(sizeOf($0).length)
      bram_store_nd($0, inds, $2)
    }

    // TODO: Flat addressing currently won't work with this type class instance
    // Assumes # of indices = # of dimensions of BRAM
    val BramMem = tpeClassInst("BramMem", T, TMem(T, BRAM(T)))
    infix (BramMem) ("ld", T, (BRAM(T), Indices) :: T) implements composite ${ bram_load_inds($0, $1) }
    infix (BramMem) ("st", T, (BRAM(T), Indices, T) :: MUnit, effect = write(0)) implements composite ${ bram_store_inds($0, $1, $2) }


    // --- API
		static (BRAM) ("apply", T, (SString, SInt, varArgs(SInt)) :: BRAM(T), TNum(T)) implements composite ${ bram_create[T](Some($0), $1 +: $2.toList) }
		static (BRAM) ("apply", T, (SInt, varArgs(SInt)) :: BRAM(T), TNum(T)) implements composite ${ bram_create[T](None, $0 +: $1.toList) }

		val BRAM_API = withTpe(BRAM)
		BRAM_API {
      /* Load */
      infix ("apply") ((Fix, varArgs(Fix)) :: T) implements composite ${ bram_load_nd($self, $1 +: $2.toList) }

			/* Store */
      // varArgs in update doesn't work in Scala
			infix ("update") ((Fix, T) :: MUnit, effect = write(0)) implements composite ${ bram_store_nd($self, List($1), $2) }
      infix ("update") ((Fix, Fix, T) :: MUnit, effect = write(0)) implements composite ${ bram_store_nd($self, List($1, $2), $3) }
      infix ("update") ((Fix, Fix, Fix, T) :: MUnit, effect = write(0)) implements composite ${ bram_store_nd($self, List($1, $2, $3), $4) }
      infix ("update") ((SSeq(Fix), T) :: MUnit, effect = write(0)) implements composite ${ bram_store_nd($self, $1.toList, $2) }

			infix ("rst") (Nil :: MUnit, TNum(T), effect = write(0)) implements composite ${ bram_reset($self, zero[T]) }
      infix ("mkString") (Nil :: MString) implements composite ${ bram_mkstring($self) }
		}

    // --- Scala backend
    impl (bram_new)   (codegen($cala, ${ new Array[$t[T]]($size) })) // $t[T] refers to concrete type in IR
    impl (bram_load)  (codegen($cala, ${ $bram.apply($addr.toInt) }))
    impl (bram_store) (codegen($cala, ${ $bram.update($addr.toInt, $value) }))
    impl (bram_reset) (codegen($cala, ${ (0 until $bram.length).foreach{i => $bram.update(i, $zero) }}))
    impl (bram_mkstring) (codegen($cala, ${ "BRAM[" + $0.mkString(", ") + "]" }))
  }


  // TODO: Can probably change tile load/store "start" to a single flat offset
  // TODO: Change interface of tile load / store to words rather than BRAMs?
  // TODO: Improve syntax for tile load + store?
  // TODO: Check precision of addresses to make sure they're integers
  // TODO: Support more than 2D tile loading
  // TODO: Don't need "offDims" once metadata mirroring is added - works right now since we only use
  // the symbol metadata at staging time (similar to struct unwrapping during staging actually)
  def importOffChip() {
    val T = tpePar("T")
		val OffChip = lookupTpe("OffChipMem")
    val BRAM    = lookupTpe("BRAM")
    val Fix     = lookupTpe("Fix")

    // --- Nodes
    val offchip_new = internal (OffChip) ("offchip_new", T, ("size", Fix) :: OffChip(T), effect = mutable)

    val offchip_load = internal (OffChip) ("offchip_load", T, (("offchip", OffChip(T)),
                                                               ("offDims", SList(Fix)),
                                                               ("bram", BRAM(T)),
                                                               ("start", SList(Fix)),
                                                               ("tileDims", SList(SInt))
                                                              ) :: MUnit,
                                                              effect = write(2), aliasHint = aliases(Nil))

    val offchip_store = internal (OffChip) ("offchip_store", T, (("offchip", OffChip(T)),
                                                                 ("offDims", SList(Fix)),
                                                                 ("bram", BRAM(T)),
                                                                 ("start", SList(Fix)),
                                                                 ("tileDims", SList(SInt))
                                                                ) :: MUnit,
                                                                effect = write(0), aliasHint = aliases(Nil))

    // --- Scala Debugging Nodes
    val offchip_apply = internal (OffChip) ("offchip_apply", T, (OffChip(T), Fix) :: T)
    val offchip_mkstring = internal (OffChip) ("offchip_makestring", T, (OffChip(T)) :: MString)
    val offchip_from_array = internal (OffChip) ("offchip_from_array", T, ("arr", MArray(T)) :: OffChip(T), effect = mutable)

    // --- Internals
    internal (OffChip) ("offchip_create", T, (SOption(SString), SSeq(Fix)) :: OffChip(T)) implements composite ${
      val offchip = offchip_new[T](productTree($1.toList))
      $0.foreach{name => nameOf(offchip) = name }
      symSizeOf(offchip) = if ($1.length == 1) 1.toFixPt +: $1.toList else $1.toList // 1D should be a row, not a column
      offchip
    }

    internal (OffChip) ("offchip_fromseq", T, (SOption(SString), SSeq(T), SSeq(SInt)) :: OffChip(T)) implements composite ${
      val offchip = offchip_from_array(array_fromseq[T]($1))
      $0.foreach{name => nameOf(offchip) = name }
      symSizeOf(offchip) = if ($2.length == 1) 1.toFixPt +: $2.toList.map(_.toFixPt) else $2.toList.map(_.toFixPt)
      offchip
    }


    // --- Debugging API
    // Initialize OffChipMem to constant values. These apply functions are for Scala testing purpose only!
    // Changed name (ambiguous with arbitrary dimensions otherwise)
    static (OffChip) ("withInit1D", T, (SString, SSeq(T)) :: OffChip(T), TNum(T)) implements composite ${ offchip_fromseq(Some($0), $1, Seq($1.length)) }
    static (OffChip) ("withInit1D", T, SSeq(T) :: OffChip(T), TNum(T)) implements composite ${ offchip_fromseq(None, $0, Seq($0.length)) }

    static (OffChip) ("withInit2D", T, (SString, SSeq(SSeq(T))) :: OffChip(T), TNum(T)) implements composite ${ offchip_fromseq(Some($0), $1.flatten, Seq($1.length, $1.head.length)) }
    static (OffChip) ("withInit2D", T, SSeq(SSeq(T)) :: OffChip(T), TNum(T)) implements composite ${ offchip_fromseq(None, $0.flatten, Seq($0.length, $0.head.length)) }

    val OffChip_DebugAPI = withTpe(OffChip)
    OffChip_DebugAPI {
      // Load single element from OffChip. This load is for Scala testing purpose only!
      infix ("ld") (Fix :: T) implements composite ${ offchip_apply($self, $1) }
      infix ("mkString") (Nil :: MString) implements composite ${ offchip_makestring($self) }
    }

    // --- API
		static (OffChip) ("apply", T, (SString, Fix, varArgs(Fix)) :: OffChip(T), TNum(T)) implements composite ${ offchip_create(Some($0), $1 +: $2) }
		static (OffChip) ("apply", T, (Fix, varArgs(Fix)) :: OffChip(T), TNum(T)) implements composite ${ offchip_create(None, $0 +: $1) }

		val OffChip_API = withTpe(OffChip)
		OffChip_API {
      /* Load */
      infix ("ld") ((("bram", BRAM(T)), ("start", Fix), ("tileSize", SInt)) :: MUnit, effect = write(1)) implements composite ${
        offchip_load($self, symSizeOf($self), $bram, List($start), List($tileSize))
      }
      infix ("ld") ((("bram", BRAM(T)), ("startRow", Fix), ("startCol", Fix), ("tileRows", SInt), ("tileCols", SInt)) :: MUnit, effect = write(1)) implements composite ${
        offchip_load($self, symSizeOf($self), $bram, List($startRow, $startCol), List($tileRows, $tileCols))
      }

      /* Store */
      infix ("st") ((("bram", BRAM(T)), ("start", Fix), ("tileSize", SInt)) :: MUnit, effect = write(0)) implements composite ${
        offchip_store($self, symSizeOf($self), $bram, List($start), List($tileSize))
      }

      infix ("st") ((("bram", BRAM(T)), ("startRow", Fix), ("startCol", Fix), ("tileRows", SInt), ("tileCols", SInt)) :: MUnit, effect = write(0)) implements composite ${
        offchip_store($self, symSizeOf($self), $bram, List($startRow, $startCol), List($tileRows, $tileCols))
      }
    }
    // --- Scala Backend
    impl (offchip_new) (codegen($cala, ${ new Array[$t[T]]($size.toInt) }))

    impl (offchip_load) (codegen($cala, ${
      @ val offCols = $offDims.apply(1)
      @ val tileRows = if ($tileDims.length == 1) 1 else $tileDims.head
      @ val tileCols = if ($tileDims.length == 1) $tileDims.head else $tileDims(1)
      @ val startRow = if ($start.length == 1) 0L else $start.head
      @ val startCol = if ($start.length == 1) $start.head else $start(1)
      @
      val offchip_offset = ($offCols * $startRow) + $startCol
      for (i <- 0 until $tileRows) {
        for (j <- 0 until $tileCols) {
          val offaddr  = $offCols*i + j + offchip_offset
          val bramaddr = $tileRows*i + j
          $bram(bramaddr.toInt) = $offchip(offaddr.toInt)
        }
      }
    }))

    impl (offchip_store) (codegen($cala, ${
      @ val offCols = $offDims.apply(1)
      @ val tileRows = if ($tileDims.length == 1) 1 else $tileDims.head
      @ val tileCols = if ($tileDims.length == 1) $tileDims.head else $tileDims(1)
      @ val startRow = if ($start.length == 1) 0L else $start.head
      @ val startCol = if ($start.length == 1) $start.head else $start(1)
      @
      val offchip_offset = ($offCols * $startRow) + $startCol
      for (i <- 0 until $tileRows) {
        for (j <- 0 until $tileCols) {
          val offaddr  = $offCols*i + j + offchip_offset
          val bramaddr = $tileRows*i + j
          $offchip(offaddr.toInt) = $bram(bramaddr.toInt)
        }
      }
    }))

    impl (offchip_apply) (codegen($cala, ${ $0.apply($1.toInt) }))
    impl (offchip_mkstring) (codegen($cala, ${
      @ val name = nameOf($0)
      "offchip: " + $name + " data: " + "--------->\\n[" + $0.mkString(",") + "]\\n------------------>"
    }))

    impl (offchip_from_array) (codegen($cala, ${ Array.tabulate($arr.length){i => $arr.apply(i)} }))
	}
}

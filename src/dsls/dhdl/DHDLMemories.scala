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
    importTiles()
  }

  // Type class for local memories which can be used as accumulators in reductions
  def importMemOps() {
    val Indices = lookupTpe("Indices")
    val Idx = lookupAlias("Index")
    val T = tpePar("T")       // data type
    val C = hkTpePar("C", T)  // memory type

    val Mem = tpeClass("Mem", TMem, (T, C))
    infix (Mem) ("ld", (T,C), (C, Idx) :: T)
    infix (Mem) ("st", (T,C), (C, Idx, T) :: MUnit, effect = write(0))
    infix (Mem) ("flatIdx", (T,C), (C, Indices) :: Idx)
  }


  // TODO: Should we allow ArgIn / ArgOut with no given name? Way of automatically numbering them instead?
  // TODO: Better / more correct way of exposing register reset?
  // TODO: Add explicit reset in the IR in scope in which a register is created? Immediately after reg_create?
  def importRegs() {
    val T = tpePar("T")

    val FixPt = lookupTpe("FixPt")
    val FltPt = lookupTpe("FltPt")
    val Bit   = lookupTpe("Bit")
    val Reg   = lookupTpe("Reg")
    val RegTpe  = lookupTpe("RegTpe", stage=compile)
    val Indices = lookupTpe("Indices")
    val Idx     = lookupAlias("Index")

    // --- Nodes
    val reg_new   = internal (Reg) ("reg_new", T, ("init", T) :: Reg(T), effect = mutable)
    val reg_read  = internal (Reg) ("reg_read", T, ("reg", Reg(T)) :: T, aliasHint = aliases(Nil), effect = simple)
    val reg_write = internal (Reg) ("reg_write", T, (("reg", Reg(T)), ("value", T)) :: MUnit, effect = write(0))
    val reg_reset = internal (Reg) ("reg_reset", T, ("reg", Reg(T)) :: MUnit, effect = write(0))

    // --- Internals
    internal (Reg) ("reg_create", T, (SOption(SString), T, RegTpe) :: Reg(T), effect = mutable) implements composite ${
      val reg = reg_new[T](init = $1)
      $0.foreach{name => nameOf(reg) = name }
      isDblBuf(reg) = false
      regType(reg) = $2
      resetValue(reg) = $1
      reg
    }

    direct (Reg) ("readReg", T, ("reg", Reg(T)) :: T) implements composite ${ reg_read($0) }
    direct (Reg) ("writeReg", T, (("reg", Reg(T)), ("value", T)) :: MUnit, effect = write(0)) implements composite ${ reg_write($0, $1) }

    val Mem = lookupTpeClass("Mem").get
    val RegMem = tpeClassInst("RegMem", T, TMem(T, Reg(T)))
    infix (RegMem) ("ld", T, (Reg(T), Idx) :: T) implements composite ${ readReg($0) } // Ignore address
    infix (RegMem) ("st", T, (Reg(T), Idx, T) :: MUnit, effect = write(0)) implements composite ${ writeReg($0, $2) }
    infix (RegMem) ("flatIdx", T, (Reg(T), Indices) :: Idx) implements composite ${ 0.as[Index] }

    // --- API
    /* Reg */
    static (Reg) ("apply", T, ("name", SString) :: Reg(T), TNum(T)) implements composite ${ reg_create[T](Some($0), zero[T], Regular) }
    static (Reg) ("apply", T, Nil :: Reg(T), TNum(T)) implements composite ${ reg_create[T](None, zero[T], Regular) }

    UnstagedNumerics.foreach{ (ST,_) =>
      static (Reg) ("apply", T, (("name", SString), ("init", ST)) :: Reg(T), TNum(T)) implements composite ${ reg_create[T](Some($0), $1.as[T], Regular) }
      static (Reg) ("apply", T, ("init", ST) :: Reg(T), TNum(T)) implements composite ${ reg_create[T](None, $init.as[T], Regular) }
    }

    // Disallowing staged init values for now
    //static (Reg) ("apply", T, (("name", SString), ("init", T)) :: Reg(T), TNum(T)) implements composite ${ reg_create[T](Some($0), $1, Regular) }
    //static (Reg) ("apply", T, ("init", T) :: Reg(T), TNum(T)) implements composite ${ reg_create[T](None, $0, Regular) }


    /* ArgIn */
    direct (Reg) ("ArgIn", T, ("name", SString) :: Reg(T), TNum(T)) implements composite ${ reg_create[T](Some($name), zero[T], ArgumentIn) }
    direct (Reg) ("ArgIn", T, Nil :: Reg(T), TNum(T)) implements composite ${ reg_create[T](None, zero[T], ArgumentIn) }

    /* ArgOut */
    direct (Reg) ("ArgOut", T, ("name", SString) :: Reg(T), TNum(T)) implements composite ${ reg_create[T](Some($name), zero[T], ArgumentOut) }
    direct (Reg) ("ArgOut", T, Nil :: Reg(T), TNum(T)) implements composite ${ reg_create[T](None, zero[T], ArgumentOut) }

    val Reg_API = withTpe(Reg)
    Reg_API {
      infix ("value") (Nil :: T) implements redirect ${ readReg($self) }
      infix (":=") (T :: MUnit, effect = write(0)) implements composite ${
        if (regType($self) == ArgumentIn) stageError("Writing to an input argument is disallowed")
        reg_write($self, $1)
      }
      infix ("rst") (Nil :: MUnit, effect = write(0)) implements composite ${ reg_reset($self) }
    }

    // TODO: Should warn/error if not an ArgIn?
    fimplicit (Reg) ("regFix_to_fix", (S,I,F), Reg(FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ readReg($0) }
    fimplicit (Reg) ("regFlt_to_flt", (G,E), Reg(FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ readReg($0) }
    fimplicit (Reg) ("regBit_to_bit", Nil, Reg(Bit) :: Bit) implements redirect ${ readReg($0) }

    // --- Scala Backend
    impl (reg_new)   (codegen($cala, ${ Array($init) }))
    impl (reg_read)  (codegen($cala, ${ $reg.apply(0) }))
    impl (reg_write) (codegen($cala, ${ $reg.update(0, $value) }))
    impl (reg_reset) (codegen($cala, ${
      @ val init = resetValue($reg)
      $reg.update(0, $init)
    }))

    // --- Dot Backend
    impl (reg_new)   (codegen(dot, ${
			@ regType(sym) match {
				@ case Regular =>
					@ if (isDblBuf(sym)) {
							$sym [margin=0, rankdir="LR", label="{<st> \$sym | <ld>}" shape="record"
										color="$dblbufBorderColor " style="filled" fillcolor="$regColor "]
					@ } else {
							$sym [label= "\$sym" shape="square" color="$regColor " style="filled"
									 fillcolor="$regColor "]
					@ }
				@ case ArgumentIn =>
					@ val sn = "ArgIn" + quote(sym).substring(quote(sym).indexOf("_"))
        	@ alwaysGen {
            $sym [label=$sn shape="Msquare"]
				  @ }
        @ case ArgumentOut =>
					@ val sn = "ArgOut" + quote(sym).substring(quote(sym).indexOf("_"))
        	@ alwaysGen {
            $sym [label=$sn shape="Msquare"]
			    @ }
      @ }
		}))
    impl (reg_read)  (codegen(dot, ${
			@ emitAlias(sym, reg)
		}))
    impl (reg_write) (codegen(dot, ${
			$value -> $reg
		}))
    impl (reg_reset) (codegen(dot, ${
    }))

    // --- MaxJ Backend
    impl (reg_new)   (codegen(maxj, ${
			@ val ts = tpstr[T](par(sym))
			@ regType(sym) match {
				@ case Regular =>
					/*
          val parent = if (!n.hasParent()) "top" else s"${quote(n.getParent())}"
					*/
          //@ val wen = s"${quote(parent)}_en"
          //@ val rst = s"${quote(parent)}_rst_en"
          //@ val din = n.input
					@ if (isDblBuf(sym)) {
					@		val symlib = quote(sym) + "_lib"
					@		val p = par(sym)
          		DblRegFileLib $symlib = new DblRegFileLib(this, $ts, $sym, $p);
          @   if (p > 1) {
					@			val symlibreadv = symlib + ".readv();"
								DFEVector<DFEVar> $sym = $symlibreadv
          @    } else {
					@			val symlibread = symlib + ".read();"
              	DFEVar $sym = $symlibread
          @  }
          //@  emit(s"""${quote(sym)}_lib.write($din, ${quote(sym.getWriter())}_done);""")
          //@  emit(s"""${quote(sym)}_lib.connectWdone(${quote(n.getWriter())}_done);""")
          //@  n.getReaders().map { r =>
          //@    emit(s"""${quote(n)}_lib.connectRdone(${quote(r)}_done);""")
          //@  }
          @ } else {
					@		val pre = maxJPre(sym)
					@ 	val tsinst = ts + ".newInstance(this);"
          		$pre $sym = $tsinst
          //@  if (!n.input.hasParent) {
          //@    throw new Exception(s"""Reg ${quote(n)}'s input ${n.input} does not have a parent. How is that possible?""")
          //@  }

          //@  val enSignalStr = sym.producer match {
          //@    case p@Pipe(ctr,_,_) =>
          //@      s"${quote(ctr)}_en_from_pipesm"
          //@    case _ =>
          //@      s"${quote(sym.producer)}_en"
          //@  }
          //@  emit(s"""DFEVar ${quote(sym.input)}_real = $enSignalStr ? ${quote(sym.input)} : ${quote(sym)}; // enable""")
          //@  emit(s"""DFEVar ${quote(sym)}_hold = Reductions.streamHold(${quote(sym.input)}_real, ($rst | ${quote(sym.producer)}_redLoop_done));""")
          //@  emit(s"""${quote(sym)} <== $rst ? constant.var(${tpstr(init)},0) : stream.offset(${quote(sym)}_hold, -${quote(sym.producer)}_offset); // reset""")
          @ }
				@ case ArgumentIn =>  // alwaysGen
					@ val sn = "ArgIn" + quote(sym).substring(quote(sym).indexOf("_"))
          DFEVar $sn = io.scalarInput($sn, $ts);
				@ case ArgumentOut => // alwaysGen
					@ val sn = "ArgOut" + quote(sym).substring(quote(sym).indexOf("_"))
			@ }
		}))
    impl (reg_read)  (codegen(maxj, ${
		}))
    impl (reg_write) (codegen(maxj, ${
      //@ val valueStr = if (value.isInstanceOf[Reg]) {
      //@   s"${value}_hold"
      //@ } else {
      //@   s"$value"
      //@ }
			@ val valueStr = "value"
      //@ val controlStr = if (!n.hasParent()) s"top_done" else s"${quote(n.getParent())}_done"
			@ val controlStr = "top_done"
			@ val ts = tpstr[T](par(sym))
      io.scalarOutput($sym, $valueStr, $ts, $controlStr);
		}))
    impl (reg_reset) (codegen(maxj, ${
    }))

  }


  // TODO: Generalize definition of BRAM store to be equivalent to St node in original DHDL?
  // TODO: Should we support a BRAM reset API? How to time this properly? Should this be a composite which creates a Pipe?
  def importBRAM() {
    val T = tpePar("T")
    val BRAM    = lookupTpe("BRAM")
    val Tile    = lookupTpe("Tile")
    val Idx     = lookupAlias("Index")
    val Indices = lookupTpe("Indices")

    // --- Nodes
    val bram_new = internal (BRAM) ("bram_new", T, (("size", SInt), ("zero", T)) :: BRAM(T), effect = mutable)
    val bram_load = internal (BRAM) ("bram_load", T, (("bram", BRAM(T)), ("addr", Idx)) :: T)
    val bram_store = internal (BRAM) ("bram_store", T, (("bram", BRAM(T)), ("addr", Idx), ("value", T)) :: MUnit, effect = write(0), aliasHint = aliases(Nil))
    val bram_reset = internal (BRAM) ("bram_reset", T, (("bram", BRAM(T)), ("zero", T)) :: MUnit, effect = write(0))

    // --- Internals
    internal (BRAM) ("bram_create", T, (SOption(SString), SList(SInt)) :: BRAM(T), TNum(T)) implements composite ${
      val bram = bram_new[T]($1.reduce(_*_), zero[T])
      dimsOf(bram) = $1
      $0.foreach{name => nameOf(bram) = name }
      isDblBuf(bram) = false
      banks(bram) = 1
      bram
    }

    direct (BRAM) ("bram_load_nd", T, (BRAM(T), SList(Idx)) :: T) implements composite ${
      val addr = calcLocalAddress($1, dimsOf($0))
      bram_load($0, addr)
    }
    direct (BRAM) ("bram_store_nd", T, (BRAM(T), SList(Idx), T) :: MUnit, effect = write(0)) implements composite ${
      val addr = calcLocalAddress($1, dimsOf($0))
      bram_store($0, addr, $2)
    }

    direct (BRAM) ("bram_calc_addr", T, (BRAM(T), Indices) :: Idx) implements composite ${ calcLocalAddress($1.toList, dimsOf($0)) }

    // direct (BRAM) ("bram_load_inds", T, (BRAM(T), Indices) :: T) implements composite ${ bram_load_nd($0, $1.toList) }
    // direct (BRAM) ("bram_store_inds", T, (BRAM(T), Indices, T) :: MUnit, effect = write(0)) implements composite ${ bram_store_nd($0, $1.toList, $2) }

    val Mem = lookupTpeClass("Mem").get
    val BramMem = tpeClassInst("BramMem", T, TMem(T, BRAM(T)))
    infix (BramMem) ("ld", T, (BRAM(T), Idx) :: T) implements composite ${ bram_load_nd($0, List($1)) }
    infix (BramMem) ("st", T, (BRAM(T), Idx, T) :: MUnit, effect = write(0)) implements composite ${ bram_store_nd($0, List($1), $2) }
    infix (BramMem) ("flatIdx", T, (BRAM(T), Indices) :: Idx) implements composite ${ bram_calc_addr($0, $1) }


    // --- API
    static (BRAM) ("apply", T, (SString, SInt, varArgs(SInt)) :: BRAM(T), TNum(T)) implements composite ${ bram_create[T](Some($0), $1 +: $2.toList) }
    static (BRAM) ("apply", T, (SInt, varArgs(SInt)) :: BRAM(T), TNum(T)) implements composite ${ bram_create[T](None, $0 +: $1.toList) }

    val BRAM_API = withTpe(BRAM)
    BRAM_API {
      /* Load */
      infix ("apply") ((Idx, varArgs(Idx)) :: T) implements composite ${ bram_load_nd($self, $1 +: $2.toList) }

      /* Store */
      // varArgs in update doesn't work in Scala
      infix ("update") ((Idx, T) :: MUnit, effect = write(0)) implements composite ${ bram_store_nd($self, List($1), $2) }
      infix ("update") ((Idx, Idx, T) :: MUnit, effect = write(0)) implements composite ${ bram_store_nd($self, List($1, $2), $3) }
      infix ("update") ((Idx, Idx, Idx, T) :: MUnit, effect = write(0)) implements composite ${ bram_store_nd($self, List($1, $2, $3), $4) }
      infix ("update") ((SSeq(Idx), T) :: MUnit, effect = write(0)) implements composite ${ bram_store_nd($self, $1.toList, $2) }

      infix ("rst") (Nil :: MUnit, TNum(T), effect = write(0)) implements composite ${ bram_reset($self, zero[T]) }
      infix (":=") (Tile(T) :: MUnit, effect = write(0)) implements redirect ${ transferTile($1, $self, false) }
    }

    // --- Scala Backend
    impl (bram_new)   (codegen($cala, ${ Array.fill($size)($zero) })) // $t[T] refers to concrete type in IR
    impl (bram_load)  (codegen($cala, ${ $bram.apply($addr.toInt) }))
    impl (bram_store) (codegen($cala, ${ $bram.update($addr.toInt, $value) }))
    impl (bram_reset) (codegen($cala, ${ (0 until $bram.length).foreach{i => $bram.update(i, $zero) }}))

    // --- Dot Backend
    impl (bram_new)   (codegen(dot, ${
      @ if (isDblBuf(sym)) {
      	$sym [margin=0 rankdir="LR" label="{<st> \$sym | <ld> }" shape="record"
							color="$dblbufBorderColor " style="filled" fillcolor="$memColor "]
      @ } else {
        	$sym [label="\$sym" shape="square" style="filled" fillcolor="\$memColor"]
      @ }
		}))
		impl (bram_load)  (codegen(dot, ${
			$addr -> $bram [label="addr"]
			//$sym [style="invisible" height=0 size=0 margin=0 label=""]
			//$sym [label=$sym fillcolor="lightgray" style="filled"]
			@ emitAlias(sym, bram)
		}))
		impl (bram_store) (codegen(dot, ${
			$addr -> $bram [label="addr"]
			$value -> $bram [label="data"]
		}))
    impl (bram_reset) (codegen(dot, ${ }))

    // --- MaxJ Backend
    impl (bram_new)   (codegen(maxj, ${
      @ if (isDblBuf(sym)) {
      @ } else {
				//TODO: add stride once it's in language
        //BramLib $sym = new BramLib(this, $depth, $ts, $bks , $stride );
				@ val ts = tpstr[T]( par(sym) )
				@ val bks = banks(sym)
        BramLib $sym = new BramLib(this, $size, $ts, $bks , 1 );
      @ }
		}))
		impl (bram_load)  (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			@ val ts = tpstr[T](par(sym)) + ".newInstance(this);"
			$pre $sym = $ts
		}))
		impl (bram_store) (codegen(maxj, ${
      //val dataStr = if (data.isInstanceOf[Reg]) {
      //  val regData = data.asInstanceOf[Reg]
      //  s"${data}_hold"
      //} else {
      //  s"$data"
      //}
      //  if (mem.isAccum) {
      //    val offsetStr = s"${data.getParent()}_offset"
      //    val parentPipe = n.getParent().asInstanceOf[Pipe]
      //    val parentCtr = parentPipe.ctr
      //    if (mem.isDblBuf) {
      //      fp(s"""$mem.connectWport(stream.offset(${quote(addr)}, -$offsetStr), stream.offset($dataStr, -$offsetStr), ${quote(parentCtr)}_en_from_pipesm, $start, $stride);""")
      //    } else {
      //      fp(s"""$mem.connectWport(stream.offset($addr, -$offsetStr), stream.offset($dataStr, -$offsetStr), ${quote(parentCtr)}_en_from_pipesm, $start, $stride);""")
      //    }
      //  } else {
      //    if (mem.isDblBuf) {
      //      fp(s"""// mem writer: ${mem.getWriter()} """)
      //      fp(s"""$mem.connectWport(${quote(addr)}, $dataStr, ${quote(n.getParent())}_en, ${n.start}, ${n.stride});""")
      //    } else {
      //      fp(s"""$mem.connectWport($addr, $dataStr, ${quote(n.getParent())}_en, ${n.start}, ${n.stride});""")
      //    }
      //  }
		}))
    impl (bram_reset) (codegen(maxj, ${ }))

  }
  // TODO: Size of offchip memory can be a staged value, but it can't be a value which is calculated in hardware
  //       Any way to make this distinction?
  // TODO: Change interface of tile load / store to words rather than BRAMs?
  def importOffChip() {
    val T = tpePar("T")
    val OffChip = lookupTpe("OffChipMem")
    val Tile    = lookupTpe("Tile")
    val BRAM    = lookupTpe("BRAM")
    val Range   = lookupTpe("Range")
    val Idx     = lookupAlias("Index")

    // --- Nodes
    val offchip_new = internal (OffChip) ("offchip_new", T, ("size", Idx) :: OffChip(T), effect = mutable)
    // tile_transfer - see extern

    // --- Internals
    internal (OffChip) ("offchip_create", T, (SOption(SString), SSeq(Idx)) :: OffChip(T)) implements composite ${
      val offchip = offchip_new[T](productTree($1.toList))
      $0.foreach{name => nameOf(offchip) = name }
      symDimsOf(offchip) = $1.toList
      offchip
    }

    // --- API
    static (OffChip) ("apply", T, (SString, Idx, varArgs(Idx)) :: OffChip(T), TNum(T)) implements composite ${ offchip_create(Some($0), $1 +: $2) }
    static (OffChip) ("apply", T, (Idx, varArgs(Idx)) :: OffChip(T), TNum(T)) implements composite ${ offchip_create(None, $0 +: $1) }

    // Offer multiple versions of tile select since implicit cast from signed int to range isn't working
    val OffChip_API = withTpe(OffChip)
    OffChip_API {
      //infix ("apply") (varArgs(Range) :: Tile(T)) implements composite ${ tile_create($self, $1.toList) }
      infix ("apply") (Range :: Tile(T)) implements composite ${ tile_create($self, List($1)) }
      infix ("apply") ((Range,Range) :: Tile(T)) implements composite ${ tile_create($self, List($1,$2)) }
      infix ("apply") ((Range,Range,Range) :: Tile(T)) implements composite ${ tile_create($self, List($1,$2,$3)) }

      // 2D -> 1D
      infix ("apply") ((Idx, Range) :: Tile(T)) implements composite ${ tile_create($self, List(unitRange($1), $2)) }
      infix ("apply") ((Range, Idx) :: Tile(T)) implements composite ${ tile_create($self, List($1, unitRange($2))) }

      // 3D -> 2D
      infix ("apply") ((Idx, Range, Range) :: Tile(T)) implements composite ${ tile_create($self, List(unitRange($1), $2, $3)) }
      infix ("apply") ((Range, Idx, Range) :: Tile(T)) implements composite ${ tile_create($self, List($1, unitRange($2), $3)) }
      infix ("apply") ((Range, Range, Idx) :: Tile(T)) implements composite ${ tile_create($self, List($1, $2, unitRange($3))) }

      // 3D -> 1D
      infix ("apply") ((Idx, Idx, Range) :: Tile(T)) implements composite ${ tile_create($self, List(unitRange($1), unitRange($2), $3)) }
      infix ("apply") ((Idx, Range, Idx) :: Tile(T)) implements composite ${ tile_create($self, List(unitRange($1), $2, unitRange($3))) }
      infix ("apply") ((Range, Idx, Idx) :: Tile(T)) implements composite ${ tile_create($self, List($1, unitRange($2), unitRange($3))) }
    }

    // --- Scala Backend
    impl (offchip_new) (codegen($cala, ${ new Array[$t[T]]($size.toInt) }))

		// --- Dot Backend
		impl (offchip_new) (codegen(dot, ${
			@ alwaysGen {
        $sym [label="\$sym" shape="square" fontcolor="white" color="white" style="filled"
			  fillcolor="$offChipColor "]
			  $size -> $sym [label="size"]
			  //$size [style="invisible" height=0 size=0 margin=0 label=""]
      @ }
		}))


		// --- MaxJ Backend
		impl (offchip_new) (codegen(maxj, ${
		}))
  }


  def importTiles() {
    val T = tpePar("T")
    val OffChip = lookupTpe("OffChipMem")
    val BRAM    = lookupTpe("BRAM")
    val Tile    = lookupTpe("Tile")
    val Range   = lookupTpe("Range")

    // TODO: How to avoid CSE? Doesn't matter except that same symbol may be returned
    // and need different symbols to manage offset staging metadata properly
    data(Tile, ("_target", OffChip(T)))
    internal (Tile) ("tile_new", T, OffChip(T) :: Tile(T)) implements allocates(Tile, ${$0})
    internal (Tile) ("tile_create", T, (OffChip(T), SList(Range)) :: Tile(T)) implements composite ${
      if (symDimsOf($0).length != $1.length) stageError("Attempting to access " + symDimsOf($0).length + "D memory with " + $1.length + " indices")
      val tile = tile_new($0)
      rangesOf(tile) = $1
      tile
    }
    internal.infix (Tile) ("mem", T, Tile(T) :: OffChip(T)) implements getter(0, "_target")

    infix (Tile) (":=", T, (Tile(T), BRAM(T)) :: MUnit, effect = write(0)) implements redirect ${ transferTile($0, $1, true) }

    // Actual effect depends on store, but this isn't a node anyway
    direct (Tile) ("transferTile", T, (("tile",Tile(T)), ("local",BRAM(T)), ("store", SBoolean)) :: MUnit, effect = simple) implements composite ${
      val mem      = $tile.mem
      val ranges   = rangesOf($tile)
      val offsets  = ranges.map(_.start)
      val unitDims = ranges.map(isUnit(_))

      val memDims = symDimsOf(mem)
      val tileDims = dimsOf($local) // TODO: Allow this to be different than size of BRAM?
      val ofs = calcFarAddress(offsets, memDims)
      val strides = dimsToStrides(memDims)
      val nonUnitStrides = strides.zip(unitDims).filterNot(_._2).map(_._1)

      val ctrs = tileDims.map{d => Counter(max = d.as[Index]) }
      val chain = CounterChain(ctrs:_*)
      tile_transfer(mem, $local, nonUnitStrides, ofs, tileDims, chain, $store)
    }

	}
}

package ppl.dsl.forge
package dsls
package dhdl

trait MemsElements {
	this: DHDLDSL =>
	def importMems() = {

		val MemOps = grp("Mems")

		val T = tpePar("T")
		val FixPt = lookupTpe("Long")
		val SString = lookupTpe("java.lang.String", stage=compile)

		val Reg = tpe("Reg", tpePar("T"))
		//TODO: how to bound T to be one of fixpt, mfloat, or boolean?
		data(Reg, ("_value", T), ("_init", T))
		internal.direct (MemOps) ("newReg", T, ("init", T) :: Reg(T), effect=mutable) implements
			allocates(Reg, ${$init}, ${$init})
		static (Reg) ("apply", T, (SString, T) :: Reg(T)) implements composite ${
			val reg = newReg[T](init=$1)
			name(reg) = $0
			dblbuf(reg) = false
			regtpe(reg) = Regular 
			reg
		}
		static (Reg) ("apply", T, T :: Reg(T)) implements composite ${ Reg[T]("", $0) }
		direct (Reg) ("ArgIn", T, (SString, T) :: Reg(T)) implements composite ${
			val reg = newReg[T](init=$1)
			name(reg) = $0
			dblbuf(reg) = false
			regtpe(reg) = ArgIn 
			reg
		}
		direct (Reg) ("ArgIn", T, T :: Reg(T)) implements composite ${ ArgIn[T]("", $0) }
		direct (Reg) ("ArgOut", T, (SString, T) :: Reg(T)) implements composite ${
			val reg = newReg[T](init=$1)
			name(reg) = $0
			dblbuf(reg) = false
			regtpe(reg) = ArgOut 
			reg
		}
		direct (Reg) ("ArgOut", T, T :: Reg(T)) implements composite ${ ArgOut[T]("", $0) }

		val RegOps = withTpe(Reg)
		RegOps {
			infix ("name") (Nil :: MString) implements composite ${ getName($self) }
			infix ("value") (Nil :: T) implements getter(0, "_value")
			infix ("init") (Nil :: T) implements getter(0, "_init")
			infix ("write") (T :: MUnit, effect = write(0)) implements setter(0, "_value", ${$1})
			infix ("reset") (Nil :: MUnit, effect = write(0)) implements composite ${ $self.write($self.init) }
		}

		val BRAM = tpe("BRAM", tpePar("T"))
		data(BRAM, ("_data", MArray(T)))
		internal.direct (MemOps) ("newBRAM", T, ("size", MInt) :: BRAM(T), effect = mutable) implements
			allocates(BRAM, ${array_empty[T]($size)})
		/* 1-D BRAM */
		static (BRAM) ("apply", T, (SString, SInt) :: BRAM(T), effect = mutable) implements
		composite ${
			val bram = newBRAM[T](size=unit($1))
			size(bram) = $1::Nil
			name(bram) = $0
			dblbuf(bram) = false
			bram
		}
		static (BRAM) ("apply", T, SInt :: BRAM(T), effect = mutable) implements
		composite ${ BRAM[T]("",$0)}
		/* 2-D BRAM */
		static (BRAM) ("apply", T, (SString, SInt, SInt) :: BRAM(T), effect = mutable) implements
		composite ${
			val bram = newBRAM[T](size=unit($1*$2))
			size(bram) = $1::$2::Nil
			name(bram) = $0
			dblbuf(bram) = false
			bram
		}
		static (BRAM) ("apply", T, (SInt, SInt) :: BRAM(T), effect = mutable) implements
		composite ${BRAM[T]("", $0, $1)}
		/* 3-D BRAM */
		static (BRAM) ("apply", T, (SString, SInt, SInt, SInt) :: BRAM(T), effect = mutable) implements
		composite ${
			val bram = newBRAM[T](size=unit($1*$2*$3))
			size(bram) = $1::$2::$3::Nil
			name(bram) = $0
			dblbuf(bram) = false
			bram
		}
		static (BRAM) ("apply", T, (SInt, SInt, SInt) :: BRAM(T), effect = mutable) implements
		composite ${BRAM[T]("", $0, $1, $2)}

		val BRAMOps = withTpe(BRAM)
		BRAMOps {
			infix ("name") (Nil :: MString) implements composite ${ getName($self) }
			infix ("data") (Nil :: MArray(T)) implements getter(0, "_data")
			/* 1-D Store */
			infix ("st") ((FixPt,T) :: MUnit, effect = write(0)) implements composite ${
				array_update( $self.data, $1.toInt, $2 ) }
			/* 2-D Store */
			infix ("st") ((FixPt, FixPt, T) :: MUnit, effect = write(0)) implements composite ${
				if (size($self).size.length!=2) { 
					throw new Exception("Try to store to non-2D Bram using bram.st(idx1, idx2), size of bram:"
						+ size($self).size) 
				}
				val bramWidth = unit(getSize($self, 1)).toFixPt
				$self.st($1*bramWidth+$2, $3)
			}
			/* 3-D Store */
		 	//TODO: not a right-hand coordinate, Fix?  x: height, y width, z: depth, zero, top, left. front
			infix ("st") ((FixPt, FixPt, FixPt, T) :: MUnit, effect = write(0)) implements composite ${
				if (size($self).size.length!=3) { 
					throw new Exception("Try to store to non-2D Bram using bram.st(idx1, idx2), size of bram:"
						+ size($self).size) 
				}
				val bramWidth0 = unit(getSize($self, 0)).toFixPt
				val bramWidth1 = unit(getSize($self, 1)).toFixPt
				$self.st(bramWidth0*bramWidth1*$3 + $1*bramWidth1+$2, $4)
			}
			/* 1-D Load */
			infix ("ld") (FixPt :: T) implements composite ${ array_apply( $self.data, $1.toInt) }
			/* 2-D Load */
			infix ("ld") ((FixPt, FixPt) :: T) implements composite ${
				if (size($self).size.length!=2) { 
					throw new Exception("Try to load from non-2D Bram using bram.st(idx1, idx2), size of bram:" 
						+ size($self).size) 
				}
				val bramWidth = unit(getSize($self, 1)).toFixPt
				$self.ld($1*bramWidth+$2)
			}
			/* 3-D Load */
		 	//TODO: not a right-hand coordinate, Fix?  x: height, y width, z: depth, zero, top, left. front
			infix ("ld") ((FixPt, FixPt, FixPt) :: T) implements composite ${
				if (size($self).size.length!=3) { 
					throw new Exception("Try to load from non-2D Bram using bram.st(idx1, idx2), size of bram:" 
						+ size($self).size) 
				}
				val bramWidth0 = unit(getSize($self, 0)).toFixPt
				val bramWidth1 = unit(getSize($self, 1)).toFixPt
				$self.ld(bramWidth0*bramWidth1*$3 + $1*bramWidth1+$2)
			}
			infix ("mkString") (Nil :: MString) implements composite ${ unit("bram[") + array_mkstring[T]( $self.data,
				unit(", ")) + unit("]")}
		}

		val OffChipMem = tpe("OffChipMem", tpePar("T"))
		data(OffChipMem, ("_data", MArray(T)), ("_size", FixPt))
		internal.direct (MemOps) ("newOffChipMem", T, ("size", FixPt) :: OffChipMem(T), effect = mutable) implements
		allocates(OffChipMem, ${array_empty[T]( $size.toInt )}, ${$size})

		static (OffChipMem) ("apply", T, (SString, FixPt):: OffChipMem(T), effect = mutable) implements
		composite ${
			val offchip = newOffChipMem[T]($1)
			name(offchip) = $0
			dblbuf(offchip) = false
			offchip
		}
		static (OffChipMem) ("apply", T, FixPt:: OffChipMem(T), effect = mutable) implements
		composite ${ OffChipMem[T]($0) }

		/* Initialize OffChipMem to constant values. These apply functions are for testing purpose only! No codegen rule */
		static (OffChipMem) ("apply", T, varArgs(T) :: OffChipMem(T), effect = mutable) implements
			allocates(OffChipMem, ${ array_fromseq[T]( $0 ) }, ${ $0.length })
		static (OffChipMem) ("apply", T, (SString, varArgs(T)) :: OffChipMem(T), effect = mutable) implements
			composite ${
			val offchip = OffChipMem[T]($1:_*)
			name(offchip) = $0
			dblbuf(offchip) = false
			offchip
		}

		val OffChipMemOps = withTpe(OffChipMem)
		OffChipMemOps {
			infix ("name") (Nil :: MString) implements composite ${ getName($self) }
			infix ("data") (Nil :: MArray(T)) implements getter(0, "_data")
			infix ("mkString") (Nil :: MString) implements composite ${ offchip_to_string[T]( $self.name,
				$self.data )
			}
			/* Load signle element from OffChipArray. This load is for debugging purpose only! 
			 	No codegen rule! */
			infix ("ld") (FixPt :: T) implements composite ${ array_apply( $self.data, $1.toInt) }
			/* 1-D OffChipMem Load */
			val offld1 = infix ("ld") ((("bram", BRAM(T)), ("start", FixPt), ("offset", SInt)) :: MUnit, effect = write(1))
			impl (offld1) (composite ${
				var i = unit(0)
				while ( i <  unit($offset) ) {
					array_update[T]( $bram.data, i, $self.data.apply(i + $start.toInt) )
					i = i + unit(1)
				}
			})
			/* 2-D OffChipMem Load */
			val offld2 = infix ("ld") (MethodSignature(
				List(("bram", BRAM(T)), ("startx", FixPt), ("starty", FixPt), ("offsetx", SInt),
					("offsety", SInt), ("width", FixPt)), MUnit),
				effect = write(1))
			impl (offld2) (composite ${
				var i = unit(0)
				while ( i <  unit($offsetx) ) {
					var j = unit(0)
					while ( j <  unit($offsety) ) {
						val row:Rep[Int] = i + $startx.toInt
						val col:Rep[Int] = j + $starty.toInt
						val offaddr:Rep[Int] = row*$width.toInt + col
						val bramaddr:Rep[Int] = i*unit($offsety) + j
						array_update[T]( $bram.data, bramaddr, $self.data.apply(offaddr) )
						j = j + unit(1)
					}
					i = i + unit(1)
				}
			})
			/* 1-D OffChipMem Store */
		 val offst1 = infix ("st") ((("bram", BRAM), ("start", FixPt), ("offset",SInt)) :: MUnit, effect = write(0))
		 impl (offst1) (composite ${
			 var i = unit(0)
			 while ( i < unit($offset) ) {
				 array_update[T]( $self.data, i + $start.toInt, $bram.data.apply(i) )
				 i = i + unit(1)
			 }
		 })
			/* 2-D OffChipMem Store */
		 val offst2 = infix ("st") (MethodSignature(
			 List(("bram", BRAM), ("startx", FixPt), ("starty", FixPt), ("offsetx",SInt), ("offsety",
				 SInt), ("width", FixPt)), MUnit),
	 effect = write(0))
		 impl (offst2) (composite ${
			 var i = unit(0)
			 while ( i <  unit($offsetx) ) {
				 var j = unit(0)
				 while ( j <  unit($offsety) ) {
					 val row = i + $startx.toInt
					 val col = j + $starty.toInt
					 val offaddr = row*$width.toInt + col
					 val bramaddr = i*unit($offsetx) + j
					 array_update[T]( $self.data, offaddr, $bram.data.apply(bramaddr) )
					 j = j + unit(1)
				 }
				 i = i + unit(1)
			 }
		 })
		}

		internal (MemOps) ("offchip_to_string", T, (MString, MArray(T))::MString) implements
		codegen ($cala, ${"offchip: " + $0 + " data: "+ "--------->\\n[" + $1.mkString(",") +
		"]\\n------------------>"})
	}
}


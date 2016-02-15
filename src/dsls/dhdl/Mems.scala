package ppl.dsl.forge
package dsls
package dhdl

trait MemsElements {
	this: DHDLDSL =>
	def importMems() = {

		val MemOps = grp("Mems")

		val T = tpePar("T")
		val FixPt = lookupTpe("Long")

		//TODO: Add argIn/argOut as metadata of Reg
		val Reg = tpe("Reg", tpePar("T"))
		//TODO: how to constrain T to be one of fixpt, mfloat, or boolean?
		data(Reg, ("_name", MString), ("_value", T), ("_init", T))
		static (Reg) ("apply", T,
			MethodSignature(List(("name", MString, "unit(\"\")"),
													 ("value", T, "unit(0)"),
												 	 ("init", T, "unit(0)"),
													 ("tpe", MString, "unit(\"normal\")")), Reg(T)),
			effect=mutable) implements allocates(Reg,
			${$name}, ${$value}, ${$init})
		static (Reg) ("apply", T, T :: Reg(T)) implements redirect ${ Reg.apply[T](value=$0, init=$0) }
		direct (Reg) ("ArgIn", T, T :: Reg(T)) implements redirect ${ Reg.apply[T](value=$0, init=$0,
		tpe=unit("argin")) }
		direct (Reg) ("ArgOut", T, T :: Reg(T)) implements redirect ${ Reg.apply[T](value=$0, init=$0,
		tpe=unit("argout")) }
		/*
		static (Reg) ("apply", FixPt, Nil :: Reg(FixPt)) implements redirect ${ Reg[FixPt](unit(0)) }
		direct (Reg) ("ArgIn", FixPt, Nil :: Reg(FixPt)) implements redirect ${ ArgIn[FixPt](unit(0)) }
		direct (Reg) ("ArgOut", FixPt, Nil :: Reg(FixPt)) implements redirect ${ ArgOut[FixPt](unit(0)) }
		static (Reg) ("apply", MFloat, Nil :: Reg(MFloat)) implements redirect ${ Reg[Float](unit(0.0f)) }
		direct (Reg) ("ArgIn", MFloat, Nil :: Reg(MFloat)) implements redirect ${ ArgIn[Float](unit(0.0f)) }
		direct (Reg) ("ArgOut", MFloat, Nil :: Reg(MFloat)) implements redirect ${ ArgOut[Float](unit(0.0f)) }
		static (Reg) ("apply", MBoolean, Nil :: Reg(MBoolean)) implements redirect ${ Reg[Boolean](unit(false)) }
		direct (Reg) ("ArgIn", MBoolean, Nil :: Reg(MBoolean)) implements redirect ${ ArgIn[Boolean](unit(false)) }
		direct (Reg) ("ArgOut", MBoolean, Nil :: Reg(MBoolean)) implements redirect ${ ArgOut[Boolean](unit(false)) }
		*/

		val RegOps = withTpe(Reg)
		RegOps {
			infix ("name") (Nil :: MString) implements getter(0, "_name")
			infix ("value") (Nil :: T) implements getter(0, "_value")
			infix ("init") (Nil :: T) implements getter(0, "_init")
			infix ("write") (T :: MUnit, effect = write(0)) implements setter(0, "_value", ${$1})
			infix ("reset") (Nil :: MUnit, effect = write(0)) implements redirect ${ $self.write($self.init) }
		}

		val BRAM = tpe("BRAM", tpePar("T"))
		data(BRAM, ("_name", MString), ("_data", MArray(T)))
		/* apply(name:String, size:Int) */
		static (BRAM) ("apply", T,
			MethodSignature(List(("name", MString, "unit(\"\")"), ("size", MInt)), BRAM(T)),
			effect = mutable) implements
		allocates(BRAM, ${$name}, ${array_empty[T]($size)})
		/* apply(name:String, width:Int, length:Int) */
		static (BRAM) ("apply", T, MInt :: BRAM(T), effect = mutable) implements
		composite ${BRAM.apply[T](size=$0)}
		static (BRAM) ("apply", T, (MString, MInt, MInt) :: BRAM(T), effect = mutable) implements
		composite ${BRAM.apply[T]($0, $1 * $2)}
		static (BRAM) ("apply", T, (MInt, MInt) :: BRAM(T), effect = mutable) implements
		composite ${BRAM.apply[T](size=($0*$1))}

		val BRAMOps = withTpe(BRAM)
		BRAMOps {
			infix ("name") (Nil :: MString) implements getter(0, "_name")
			infix ("data") (Nil :: MArray(T)) implements getter(0, "_data")
			infix ("st") ((MInt,T) :: MUnit, effect = write(0)) implements composite ${
				array_update( $self.data, $1, $2 ) }
			infix ("st") ((MInt, MInt, T) :: MUnit, effect = write(0)) implements composite ${
				//TODO: This should be $1*width + $2
				$self.st($1*$2, $3)
			}
			infix ("ld") (MInt :: T) implements composite ${ array_apply( $self.data, $1) }
			//TODO this should be $1*width + $2
			infix ("ld") ((MInt, MInt) :: T) implements composite ${ $self.ld($1*$2) }
			infix ("mkString") (Nil :: MString) implements composite ${ unit("bram[") + array_mkstring[T]( $self.data,
				unit(", ")) + unit("]")}
		}

		val OffChipMem = tpe("OffChipMem", tpePar("T"))
		data(OffChipMem, ("_name", MString), ("_data", MArray(T)))
		static (OffChipMem) ("apply", T,
			MethodSignature(List(("name", MString, "unit(\"\")"), ("size", MInt)), OffChipMem(T)),
			effect = mutable) implements
		allocates(OffChipMem, ${$name}, ${array_empty[T]( $size )})

		static (OffChipMem) ("apply", T, MInt:: OffChipMem(T), effect = mutable) implements
		composite ${ OffChipMem[T](size=$0)}

		static (OffChipMem) ("apply", T, (MString, varArgs(T)) :: OffChipMem(T), effect = mutable) implements
		allocates(OffChipMem, ${$0}, ${ array_fromseq[T]( $1 ) })

		val OffChipMemOps = withTpe(OffChipMem)
		OffChipMemOps {
			infix ("name") (Nil :: MString) implements getter(0, "_name")
			infix ("data") (Nil :: MArray(T)) implements getter(0, "_data")
			infix ("mkString") (Nil :: MString) implements composite ${ offchip_to_string[T]( $self.name,
				$self.data )
			}
			infix ("ld") (MInt :: T) implements composite ${ array_apply( $self.data, $1) }
			/* load from offchip mem to bram. (BRAM, startIdx, offSet)*/
			val offld1 = infix ("ld") ((("bram", BRAM(T)), ("start", MInt), ("offset", MInt)) :: MUnit, effect = write(1))
			impl (offld1) (composite ${
				var i = unit(0)
				while ( i <  $offset ) {
					array_update[T]( $bram.data, i, $self.data.apply(i + $start) )
					i = i + unit(1)
				}
			})
			val offld2 = infix ("ld") (MethodSignature(
				List(("bram", BRAM(T)), ("startx", MInt), ("starty", MInt), ("offsetx", MInt), ("offsety", MInt), ("width", MInt)), MUnit),
				effect = write(1))
			impl (offld2) (composite ${
				var j = unit(0)
				while ( j <  $offsety ) {
					var i = unit(0)
					while ( i <  $offsetx ) {
						val row = i + $startx
						val col = j + $starty
						val offaddr = col*$width + row
						val bramaddr = j*$offsetx + i
						array_update[T]( $bram.data, bramaddr, $self.data.apply(offaddr) )
						i = i + unit(1)
					}
					j = j + unit(1)
				}
			})
			/* store from bram to offchip. (BRAM, stMUnitdx, offSet)*/
		 	val offst1 = infix ("st") ((("bram", BRAM), ("start", MInt), ("offset",MInt)) :: MUnit, effect = write(0))
			impl (offst1) (composite ${
				var i = unit(0)
				while ( i < $offset ) {
					array_update[T]( $self.data, i + $start, $bram.data.apply(i) )
					i = i + unit(1)
				}
			})
		 	val offst2 = infix ("st") (MethodSignature(
				List(("bram", BRAM), ("startx", MInt), ("starty", MInt), ("offsetx",MInt), ("offsety", MInt), ("width", MInt)), MUnit),
				effect = write(0))
			impl (offst2) (composite ${
				var j = unit(0)
				while ( j <  $offsety ) {
					var i = unit(0)
					while ( i <  $offsetx ) {
						val row = i + $startx
						val col = j + $starty
						val offaddr = col*$width + row
						val bramaddr = j*$offsetx + i
						array_update[T]( $self.data, offaddr, $bram.data.apply(bramaddr) )
						i = i + unit(1)
					}
					j = j + unit(1)
				}
			})
		}

		internal (MemOps) ("offchip_to_string", T, (MString, MArray(T))::MString) implements
		codegen ($cala, ${"offchip: " + $0 + " data: "+ "--------->\\n[" + $1.mkString(",") +
		"]\\n------------------>"})
	}
}


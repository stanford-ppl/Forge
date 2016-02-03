package ppl.dsl.forge
package dsls
package dhdl 

trait MemsElements {
  this: DHDLDS
	def importMems() = {

		val MemOps = grp("Mems")

		val T = tpePar("T")

		val Reg = tpe("Reg", tpePar("T"))
		//TODO: how to constrain T to be one of fixpt, mfloat, or boolean?
		data(Reg, ("_name", MString), ("_value", T), ("_init", T)) 
		static (Reg) ("apply", T, (MString,T) :: Reg(T), effect=mutable) implements allocates(Reg,
			${$0}, ${$1}, ${$1})  
		//TODO: how to create constant instance of MString?
		//static (Reg) ("apply", T, T :: Reg) implements redirect ${ Reg.apply(MString(""), $0, $1) }  

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
		static (BRAM) ("apply", T, (MString, MInt) :: BRAM(T), effect = mutable) implements
		allocates(BRAM, ${$0}, ${array_empty[T]($1)})    
		/* apply(name:String, width:Int, length:Int) */
		static (BRAM) ("apply", T, (MString, MInt, MInt) :: BRAM(T), effect = mutable) implements allocates(BRAM, ${$0}, ${array_empty[T]($1*$2)})    

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
			infix ("ld") ((MInt, MInt) :: T) implements composite ${ $self.ld($1*$2)}
			infix ("mkString") (Nil :: MString) implements composite ${ unit("bram[") + array_mkstring[T]( $self.data,
				unit(", ")) + unit("]")}
		}

		val OffChipMem = tpe("OffChipMem", tpePar("T")) 
		data(OffChipMem, ("_name", MString), ("_data", MArray(T)))
		static (OffChipMem) ("apply", T, (MString, MInt) :: OffChipMem(T), effect = mutable) implements
		allocates(OffChipMem, ${$0}, ${array_empty[T]($1)})    
		//TODO: Not working right
		static (OffChipMem) ("apply", T, (MString, MArray(T)) :: OffChipMem(T), effect = mutable) implements
		allocates(OffChipMem, ${$0}, ${$1})    

		val OffChipMemOps = withTpe(OffChipMem)
		OffChipMemOps {
			infix ("name") (Nil :: MString) implements getter(0, "_name")
			infix ("data") (Nil :: MArray(T)) implements getter(0, "_data")
			infix ("mkString") (Nil :: MString) implements composite ${ offchip_to_string[T]( $self.name,
				$self.data )
			}
			/* load from offchip mem to bram. (BRAM, startIdx, endIdx)*/
			infix ("ld") ((BRAM, MInt, MInt) :: MUnit, effect = write(1)) implements composite ${
				offchip_load( $self.data, $1.data, $2, $3 )
			}
			/* store from bram to offchip. (BRAM, startIdx, endIdx)*/
		 	infix ("st") ((BRAM, MInt, MInt) :: MUnit, effect = write(0)) implements composite ${
				offchip_store( $self.data, $1.data, $2, $3 )
			}
		}

		compiler (MemOps) ("offchip_load", T, (MArray(T), MArray(T), MInt, MInt) :: MUnit, 
			effect = write(1)) implements codegen ($cala, ${
			val offData = $0
			val bramData = $1
			val startIdx = $2
			val endIdx = $3
			(startIdx until endIdx).foreach {i => bramData(i-startIdx) = offData(i)}
		})
		compiler (MemOps) ("offchip_store", T, (MArray(T), MArray(T), MInt, MInt) :: MUnit,
			effect = write(0)) implements codegen ($cala, ${
			val offData = $0
			val bramData = $1
			val startIdx = $2
			val endIdx = $3
			(startIdx until endIdx).foreach {i => offData(i) = bramData(i-startIdx)}
		})

		compiler (MemOps) ("offchip_to_string", T, (MString, MArray(T))::MString) implements
		codegen ($cala, ${"name: " + $0 + "\\n data:\\n" + array_mkstring($1, ",")})
	}
}


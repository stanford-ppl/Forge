package ppl.dsl.forge
package dsls
package dhdl 

trait MemsElements {
	this: DHDLDSL =>
	def importMems() = {

		val MemOps = grp("Mems")

		val T = tpePar("T")
		val FixPt = lookupTpe("Long")

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
		allocates(OffChipMem, ${$0}, ${array_empty[T]( $1 )})    

		static (OffChipMem) ("apply", T, (MString, varArgs(T)) :: OffChipMem(T), effect = mutable) implements
		allocates(OffChipMem, ${$0}, ${ array_fromseq[T]( $1 ) })    

		val OffChipMemOps = withTpe(OffChipMem)
		OffChipMemOps {
			infix ("name") (Nil :: MString) implements getter(0, "_name")
			infix ("data") (Nil :: MArray(T)) implements getter(0, "_data")
			infix ("mkString") (Nil :: MString) implements composite ${ offchip_to_string[T]( $self.name,
				$self.data )
			}
			/* load from offchip mem to bram. (BRAM, startIdx, endIdx)*/
			infix ("ld") ((("bram", BRAM(T)), ("start", MInt), ("end", MInt)) :: MUnit, effect = write(1)) implements composite ${
				var i = $start
				while ( i < $end ) {
					array_update[T]( $bram.data, i, $self.data.apply(i) ) 
					i = i + unit(1)
				}
			}
			/* store from bram to offchip. (BRAM, startIdx, endIdx)*/
		 	infix ("st") ((("bram", BRAM), ("start", MInt), ("end",MInt)) :: MUnit, effect = write(0)) implements composite ${
				var i = $start
				while ( i < $end ) {
					array_update[T]( $self.data, i, $bram.data.apply(i) ) 
					i = i + unit(1)
				}
			}
		}

		compiler (MemOps) ("offchip_to_string", T, (MString, MArray(T))::MString) implements
		codegen ($cala, ${"offchip: " + $0 + " data: "+ "--------->\\n[" + $1.mkString(",") +
		"]\\n------------------>"})
	}
}


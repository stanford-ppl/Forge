package ppl.dsl.forge
package dsls
package dhdl

trait CtrlOps {
	this: DHDLDSL =>

	def importCtrls () = {

		val CtrlOps = grp("Ctrls")
		val Ctr = tpe("Ctr")
		//TODO: move step to metadata
		data (Ctr, ("_name", MString), ("_max", MInt), ("_step", MInt), ("_val", MInt))
		static (Ctr) ("apply", Nil, (MString,MInt,MInt) :: Ctr, effect=mutable) implements allocates(Ctr,
			${$0}, ${$1}, ${$2}, ${ unit(0) })

		val CtrOps = withTpe(Ctr)
		CtrOps {
			infix ("mkString") (Nil :: MString) implements composite ${
        unit("name: ") + $self.name +
			  unit(", max: ") + $self.max +
			  unit(", step: ") + $self.step
      }
			infix ("name") (Nil :: MString) implements getter(0, "_name")
			infix ("max") (Nil :: MInt) implements getter(0, "_max")
			infix ("step") (Nil :: MInt) implements getter(0, "_step")
		}

		val CtrChain = tpe("CtrChain")
		data (CtrChain, ("_chain", MArray(Ctr)))
		static (CtrChain) ("apply", Nil, Ctr :: CtrChain, effect=mutable) implements
		allocates(CtrChain, ${
			{val array = array_empty[Ctr]( unit(1) );
			array_update[Ctr](array, unit(0), $0);
			array}
		})

		//TODO: problem here
		val CtrChainOps = withTpe(CtrChain)
		CtrChainOps {
			infix ("mkString") (Nil :: MString) implements composite ${
				unit("ctrchain[") +
				array_reduce[String](
					array_map[Ctr,String]($self.chain, c => c.mkString),
          (s1, s2) => s1 + s2, unit("")
        ) + unit("]")
			}
			infix ("chain") (Nil :: MArray(Ctr)) implements getter(0, "_chain")
		}
	}
}

package ppl.dsl.forge
package dsls
package dhdl

trait CtrlOps {
	this: DHDLDSL =>

	def importCtrls () = {

		val Ctr = tpe("Ctr")
		data (Ctr, ("_name", MString), ("_min", MInt), ("_max", MInt), ("_step", MInt), ("_val", MInt))
		static (Ctr) ("apply", Nil, MethodSignature(List(("name", MString, "unit(\"\")"),
			                                               ("min", MInt, "unit(0)"), 
																										 MInt, MInt),
																								Ctr), effect=mutable) implements allocates(Ctr,
			${$name}, ${$min}, ${$2}, ${$3}, ${ unit(0) })
		val CtrOps = withTpe(Ctr)
		CtrOps {
			infix ("mkString") (Nil :: MString) implements composite ${
				unit("ctr(") +
        unit("name:") + $self.name +
			  unit(", min:") + $self.min +
			  unit(", max:") + $self.max +
			  unit(", step:") + $self.step + 
				unit(")")
      }
			infix ("name") (Nil :: MString) implements getter(0, "_name")
			infix ("min") (Nil :: MInt) implements getter(0, "_min")
			infix ("max") (Nil :: MInt) implements getter(0, "_max")
			infix ("step") (Nil :: MInt) implements getter(0, "_step")
		}

		val CtrChain = tpe("CtrChain")
		data (CtrChain, ("_chain", MArray(Ctr)))
    compiler (CtrChain) ("ctrchain_from_array", Nil, MArray(Ctr) :: CtrChain,effect=mutable) implements allocates(CtrChain, ${$0})
		static (CtrChain) ("apply", Nil, varArgs(Ctr) :: CtrChain) implements composite ${
      val array = array_empty[Ctr](unit($0.length))
      val ctrchain = ctrchain_from_array(array)
      for (i <- 0 until $0.length) { ctrchain(i) = $0.apply(i) }
			ctrchain.unsafeImmutable
    }
		val CtrChainOps = withTpe(CtrChain)
		CtrChainOps {
			infix ("mkString") (Nil :: MString) implements composite ${
				unit("ctrchain[") +
				array_mkstring[String](
					array_map[Ctr,String]($self.chain, c => c.mkString), ",") + 
				unit("]")
			}
			infix ("chain") (Nil :: MArray(Ctr)) implements getter(0, "_chain")
      infix ("update") ((MInt,Ctr) :: MUnit, effect = write(0)) implements composite ${ array_update($0.chain, $1, $2) }
			infix ("length") (Nil :: MInt) implements composite ${ $self.chain.length }
		}

		val Pipe = tpe("Pipe")
		data (Pipe, ("_ctrs", CtrChain)) //TODO: Modify pipe to keep track of nodes inside
		static (Pipe) ("apply", Nil, CtrChain :: Pipe) implements allocates(Pipe, ${$0})
		direct (Pipe) ("Map", Nil, (("ctrs", CtrChain), ("func", MInt==> MUnit)) :: Pipe) implements composite ${
			val pipe = Pipe(ctrs)
			val ctr1 = ctrs.chain.apply(unit(0))
			pipe_map(ctr1.min, ctr1.max, ctr1.step, $func)
			pipe
		}
		val pipe_map = compiler (Pipe) ("pipe_map", Nil, (("start", MInt), ("end", MInt), ("step", MInt), 
			("func", MInt==> MUnit)) :: MUnit)
		impl (pipe_map) (codegen($cala, ${
			var i = $start
			while (i < $end) {
				$b[func](i)
				i += $step
			}
		}))

		//val T = tpePar("T")
		//val Reg = lookupTpe("Reg")
		//direct (Pipe) ("Reduce", T, 
		//	(("ctrs", CtrChain), ("accum", Reg(T)), ("func", Tuple2(MInt, T)) ==> MUnit) :: Pipe) implements composite ${
		//	val pipe = Pipe(ctrs)
		//	val ctr1 = ctrs.chain.apply(unit(0))
		//	pipe_reduce(ctr1.min, ctr1.max, ctr1.step, $accum, $func)
		//	pipe
		//}
		//val pipe_reduce = compiler (Pipe) ("pipe_reduce", Nil, (("start", MInt), ("end", MInt),
		//	("step", MInt), ("accum", Reg(T)), ("func", Tuple2(MInt, T)==> MUnit)) :: MUnit)
		//impl (pipe_reduce) (codegen($cala, ${
		//	var i = $start
		//	while (i < $end) {
		//		$b[func](i, $accum)
		//		i += $step
		//	}
		//}))

	}
}

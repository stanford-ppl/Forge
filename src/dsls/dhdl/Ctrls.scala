package ppl.dsl.forge
package dsls
package dhdl

trait CtrlOps {
	this: DHDLDSL =>

	def importCtrls () = {

		val T = tpePar("T")
		val Reg = lookupTpe("Reg")
		val CtrlOps = grp("Ctrls")

		val Ctr = tpe("Ctr")
		data (Ctr, ("_name", MString), ("_min", MInt), ("_max", MInt), ("_step", MInt), ("_val", MInt))
		static (Ctr) ("apply", Nil, MethodSignature(List(("name", MString, "unit(\"\")"),
			                                               ("min", MInt, "unit(0)"),
																										 ("max", MInt), ("step", MInt, "unit(1)")),
																								Ctr), effect=mutable) implements allocates(Ctr,
			${$name}, ${$min}, ${$max}, ${$step}, ${ unit(0) })
		static (Ctr) ("apply", Nil, (("max", MInt), ("step", MInt)) :: Ctr, effect=mutable) implements
		redirect ${ Ctr.apply(name=unit(""), min=unit(0), max=$max, step=$step) }

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
		//val MCtrChain = metadata("MCtrChain", ("size",SInt))
		//meet (MCtrChain) ${ this }
		//compiler.static (MCtrChain) ("update", Nil, (MAny, SInt) :: MUnit, effect = simple) implements
		//composite ${ setMetadata($0, MCtrChain($1)) }
		//compiler.static (MCtrChain) ("apply", Nil, MAny :: SInt) implements composite ${
		//meta[MCtrChain]($0).get}

		data (CtrChain, ("_chain", MArray(Ctr)))
    internal (CtrChain) ("ctrchain_from_array", Nil, MArray(Ctr) :: CtrChain,effect=mutable) implements allocates(CtrChain, ${$0})
		static (CtrChain) ("apply", Nil, varArgs(Ctr) :: CtrChain) implements composite ${
      val array = array_empty[Ctr](unit($0.length))
      val ctrchain = ctrchain_from_array(array)
      for (i <- 0 until $0.length) { ctrchain(i) = $0.apply(i) }
			val ictrchain = ctrchain.unsafeImmutable
			//MCtrChain(ictrchain) = $0.length
			ictrchain
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

		val loop = internal (CtrlOps) ("loop", Nil, (("ctr", Ctr), ("lambda", MInt ==> MUnit)) :: MUnit)
		impl (loop) (composite ${
			var i = $ctr.min
			while (i < $ctr.max) {
				$lambda (i)
				i = i + $ctr.step
			}
		})

		val pipe_map1 = direct (Pipe) ("Pipe1", Nil, (("ctrs", CtrChain), ("mapFunc", varArgs(MInt) ==> MUnit)) :: Pipe)
		impl (pipe_map1) (composite ${
			val pipe = Pipe( $ctrs )
			def ctr(i:Int):Rep[Ctr] = $ctrs.chain.apply(unit(i))
			loop(ctr(0), (i:Rep[Int]) => $mapFunc(Seq(i)))
			pipe
		})

		val pipe_map2 = direct (Pipe) ("Pipe2", Nil, (("ctrs", CtrChain), ("mapFunc", varArgs(MInt) ==> MUnit)) :: Pipe)
		impl (pipe_map2) (composite ${
			val pipe = Pipe( $ctrs )
			def ctr(i:Int):Rep[Ctr] = $ctrs.chain.apply(unit(i))
			loop(ctr(0), (i:Rep[Int]) =>
				loop(ctr(1), (j:Rep[Int]) => $mapFunc(Seq(i, j)))
			)
			pipe
		})

		val pipe_map3 = direct (Pipe) ("Pipe3", Nil, (("ctrs", CtrChain), ("mapFunc", varArgs(MInt) ==> MUnit)) :: Pipe)
		impl (pipe_map3) (composite ${
			val pipe = Pipe( $ctrs )
			def ctr(i:Int):Rep[Ctr] = $ctrs.chain.apply(unit(i))
			loop(ctr(0), (i:Rep[Int]) =>
				loop(ctr(1), (j:Rep[Int]) =>
					loop(ctr(2), (k:Rep[Int]) => $mapFunc(Seq(i, j, k)))
				)
			)
			pipe
		})

		//TODO: Won't work here until have metadata
		val pipe_map = direct (Pipe) ("Pipe", Nil, (("ctrSize", SInt), ("ctrs", CtrChain), ("mapFunc", varArgs(MInt) ==> MUnit)) :: Pipe)
		impl (pipe_map) (composite ${

			def recPipe (idx:Int, idxs:Seq[Rep[Int]]): Rep[Unit] = {
				val ctr = $ctrs.chain.apply(unit(idx))
				if (idx == 1) {
					loop(ctr, ( (i:Rep[Int]) => $mapFunc( i+:idxs )) )
				} else {
					loop(ctr, ( (i:Rep[Int]) => recPipe(idx - 1, i+:idxs) ))
				}
			}
			val pipe = Pipe( $ctrs )

			recPipe( $ctrSize, Seq.empty[Rep[Int]] )

			pipe
		})

		val pipe_reduce1 = direct (Pipe) ("Pipe1", T, (("ctrs", CtrChain), ("accum", Reg(T)),
			("reduceFuc", (T, T) ==> T) , ("mapFunc", varArgs(MInt) ==> T)) :: Pipe)
		impl (pipe_reduce1) (composite ${
			val pipe = Pipe( $ctrs )
			def ctr(i:Int):Rep[Ctr] = $ctrs.chain.apply(unit(i))
			def func(idxs:Seq[Rep[Int]]) = $accum.write($reduceFuc($accum.value, $mapFunc(idxs)))
			$accum.reset
			loop(ctr(0), (i:Rep[Int]) => func(Seq(i)))
			pipe
		})
		val pipe_reduce2 = direct (Pipe) ("Pipe2", T, (("ctrs", CtrChain), ("accum", Reg(T)),
			("reduceFuc", (T, T) ==> T) , ("mapFunc", varArgs(MInt) ==> T)) :: Pipe)
		impl (pipe_reduce2) (composite ${
			val pipe = Pipe( $ctrs )
			def ctr(i:Int):Rep[Ctr] = $ctrs.chain.apply(unit(i))
			def func(idxs:Seq[Rep[Int]]) = $accum.write($reduceFuc($accum.value, $mapFunc(idxs)))
			$accum.reset
			loop(ctr(0), (i:Rep[Int]) =>
					loop(ctr(1), (j:Rep[Int]) => func(Seq(i, j)))
			)
			pipe
		})
		val pipe_reduce3 = direct (Pipe) ("Pipe3", T, (("ctrs", CtrChain), ("accum", Reg(T)),
			("reduceFuc", (T, T) ==> T) , ("mapFunc", varArgs(MInt) ==> T)) :: Pipe)
		impl (pipe_reduce3) (composite ${
			val pipe = Pipe( $ctrs )
			def ctr(i:Int):Rep[Ctr] = $ctrs.chain.apply(unit(i))
			def func(idxs:Seq[Rep[Int]]) = $accum.write($reduceFuc($accum.value, $mapFunc(idxs)))
			$accum.reset
			loop(ctr(0), (i:Rep[Int]) =>
				loop(ctr(1), (j:Rep[Int]) =>
					loop(ctr(2), (k:Rep[Int]) => func(Seq(i, j, k)))
				)
			)
			pipe
		})

		val MetaPipe = tpe("MetaPipe")
		data (MetaPipe, ("_ctrs", CtrChain)) //TODO: Modify pipe to keep track of nodes inside
		static (MetaPipe) ("apply", Nil, CtrChain :: MetaPipe) implements allocates(MetaPipe, ${$0})

		val meta_map1 = direct (MetaPipe) ("MetaPipe1", Nil, (("ctrs", CtrChain), ("mapFunc", varArgs(MInt) ==>
				MUnit)) :: MetaPipe)
		impl (meta_map1) (composite ${
			val metaPipe = MetaPipe( $ctrs )
			def ctr(i:Int):Rep[Ctr] = $ctrs.chain.apply(unit(i))
			loop(ctr(0), (i:Rep[Int]) => $mapFunc(Seq(i)))
			metaPipe
		})

		val meta_map2 = direct (MetaPipe) ("MetaPipe2", Nil, (("ctrs", CtrChain), ("mapFunc", varArgs(MInt) ==>
				MUnit)) :: MetaPipe)
		impl (meta_map2) (composite ${
			val metaPipe = MetaPipe( $ctrs )
			def ctr(i:Int):Rep[Ctr] = $ctrs.chain.apply(unit(i))
			loop(ctr(0), (i:Rep[Int]) =>
				loop(ctr(1), (j:Rep[Int]) => $mapFunc(Seq(i, j)))
			)
			metaPipe
		})

		val meta_map3 = direct (MetaPipe) ("MetaPipe3", Nil, (("ctrs", CtrChain), ("mapFunc", varArgs(MInt) ==>
				MUnit)) :: MetaPipe)
		impl (meta_map3) (composite ${
			val metaPipe = MetaPipe( $ctrs )
			def ctr(i:Int):Rep[Ctr] = $ctrs.chain.apply(unit(i))
			loop(ctr(0), (i:Rep[Int]) =>
				loop(ctr(1), (j:Rep[Int]) =>
					loop(ctr(2), (k:Rep[Int]) => $mapFunc(Seq(i, j, k)))
				)
			)
			metaPipe
		})

		val meta_reduce1 = direct (MetaPipe) ("MetaPipe1", T, (("ctrs", CtrChain), ("accum", Reg(T)),
			("reduceFuc", (T, T) ==> T) , ("mapFunc", varArgs(MInt) ==> T)) :: MetaPipe)
		impl (meta_reduce1) (composite ${
			val metaPipe = MetaPipe( $ctrs )
			def ctr(i:Int):Rep[Ctr] = $ctrs.chain.apply(unit(i))
			def func(idxs:Seq[Rep[Int]]) = $accum.write($reduceFuc($accum.value, $mapFunc(idxs)))
			$accum.reset
			loop(ctr(0), (i:Rep[Int]) => func(Seq(i)))
			metaPipe
		})

		val meta_parallel = direct (MetaPipe) ("Parallel", Nil, ("func", MThunk(MUnit)) :: MetaPipe) 
		impl (meta_parallel) (composite ${
			val metaPipe = MetaPipe( CtrChain(Ctr(max=unit(1))) )
			$func
			metaPipe
		})

		val meta_1iter = direct (MetaPipe) ("MetaPipe", Nil, ("func", MThunk(MUnit)) :: MetaPipe) 
		impl (meta_1iter) (composite ${
			val metaPipe = MetaPipe( CtrChain(Ctr(max=unit(1))) )
			$func
			metaPipe
		})

	}
}

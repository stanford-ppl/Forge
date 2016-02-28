package ppl.dsl.forge
package dsls
package dhdl

trait CtrlOps {
	this: DHDLDSL =>

  def importIndices() {
    val Indices = tpe("Indices")
		val FixPt = lookupTpe("Long")
    internal (Indices) ("indices_new", Nil, SList(MInt) :: Indices) implements
		record(Indices, ("i", SList(FixPt), quotedArg(0)))
    static (Indices) ("apply", Nil, varArgs(MInt) :: Indices) implements composite ${ indices_new($0.toList) }
    infix (Indices) ("apply", Nil, (Indices, SInt) :: MInt) implements composite ${ field[Int]($0, "i_" + $1) }
    internal.infix (Indices) ("toList", Nil, (Indices, SInt) :: SList(MInt)) implements composite ${ List.tabulate($1){i => $0(i)} }
  }

	def importCtrls () = {

		val T = tpePar("T")
		val Reg = lookupTpe("Reg")
		val FixPt = lookupTpe("Long")
		val CtrlOps = grp("Ctrls")
		val SString = lookupTpe("java.lang.String", stage=compile)

		val Counter = tpe("Counter")
		data (Counter, ("_min", FixPt), ("_max", FixPt), ("_step", FixPt), ("_val", FixPt))
		internal.direct (CtrlOps) ("newCounter", Nil, MethodSignature(List(
																										 ("min", FixPt),
																										 ("max", FixPt), 
																										 ("step", FixPt)),
																								Counter)) implements 
		allocates(Counter, ${$min}, ${$max}, ${$step}, ${ unit(0) })
		static (Counter) ("apply", Nil, MethodSignature(List(
																										 ("ctrPar", SInt, "0"),
																										 ("ctrName", SString, "\"\""), 
			                                               ("min", FixPt, "unit(0)"),
																										 ("max", FixPt), 
																										 ("step", FixPt, "unit(1)")),
																								Counter)) implements
		composite ${
			val ctr = newCounter($min, $max, $step)
			par(ctr) = $ctrPar
			name(ctr) = $ctrName
			ctr
		}
		//static (Counter) ("apply", Nil, (("max", FixPt), ("step", FixPt)) :: Counter) implements
		//composite ${ Counter.apply(max=$max, step=$step) }

		val CounterOps = withTpe(Counter)
		CounterOps {
			infix ("mkString") (Nil :: MString) implements codegen ($cala, ${
				"ctr(" +
        "name:" + getName($self) +
			  ", min:" + $self._min +
			  ", max:" + $self._max +
			  ", step:" + $self._step +
				")"
      })
			infix ("name") (Nil :: SString) implements composite ${ getName($self) }
			infix ("min") (Nil :: FixPt) implements getter(0, "_min")
			infix ("max") (Nil :: FixPt) implements getter(0, "_max")
			infix ("step") (Nil :: FixPt) implements getter(0, "_step")
		}

		val CounterChain = tpe("CounterChain")
		data (CounterChain, ("_chain", MArray(Counter)))
    internal (CounterChain) ("ctrchain_from_array", Nil, MArray(Counter) :: CounterChain, effect=mutable) implements allocates(CounterChain, ${$0})
		static (CounterChain) ("apply", Nil, varArgs(Counter) :: CounterChain) implements composite ${
      val array = array_empty[Counter](unit($0.length))
      val ctrchain = ctrchain_from_array(array)
      for (i <- 0 until $0.length) { ctrchain(i) = $0.apply(i) }
			val ictrchain = ctrchain.unsafeImmutable
			size(ictrchain) = $0.length::Nil
			ictrchain
    }
		val CounterChainOps = withTpe(CounterChain)
		CounterChainOps {
			infix ("mkString") (Nil :: MString) implements codegen ($cala, ${
				("ctrchain[" +
				$self._chain.map(ctr => counter_mkstring(ctr)).mkString(",") +
				"]")
			})
			infix ("chain") (Nil :: MArray(Counter)) implements getter(0, "_chain")
      infix ("update") ((MInt,Counter) :: MUnit, effect = write(0)) implements codegen ($cala, ${
				$self._chain($1) = $2
			})
			infix ("length") (Nil :: MInt) implements codegen ($cala, ${ $self._chain.length })
		}

		val loop = internal (CtrlOps) ("loop", Nil, (("ctr", Counter), ("lambda", FixPt ==> MUnit)) :: MUnit)
		impl (loop) (composite ${
			var i = $ctr.min
			while (i < $ctr.max) {
				$lambda(i)
				i = i + $ctr.step
			}
		})

		val Pipe = grp("Pipe")
		val pipe_map = static (Pipe) ("apply", Nil, 
			CurriedMethodSignature(List(
														 List(("ctrs", CounterChain)), 
														 List(("mapFunc", varArgs(FixPt) ==> MUnit))
														 ), MUnit))
		impl (pipe_map) (composite ${
			val ctrSize = getSize($ctrs, 0)
			def recPipe (idx:Int, idxs:Seq[Rep[FixPt]]): Rep[Unit] = {
				val ctr = $ctrs.chain.apply(unit(idx))
				if (idx == 0) {
					loop(ctr, { (i:Rep[FixPt]) => $mapFunc(i+:idxs)} )
				} else {
					loop(ctr, ( (i:Rep[FixPt]) => recPipe(idx - 1, i+:idxs) ))
				}
			}
			recPipe( ctrSize - 1, Seq.empty[Rep[FixPt]] )
		})

		val pipe_reduce = static (Pipe) ("apply", T, CurriedMethodSignature(List(
			List(("ctrs", CounterChain), ("accum", Reg(T)), ("reduceFunc", (T, T) ==> T)),
			List(("mapFunc", varArgs(FixPt) ==> T))), MUnit))
		impl (pipe_reduce) (composite ${
			def recPipe (idx:Int, idxs:Seq[Rep[FixPt]]): Rep[Unit] = {
				val ctr = $ctrs.chain.apply(unit(idx))
				if (idx == 0) {
					loop(ctr, ( (i:Rep[FixPt]) => $accum.write($reduceFunc($accum.value, $mapFunc(i+:idxs))) ))
				} else {
					loop(ctr, ( (i:Rep[FixPt]) => recPipe(idx - 1, i+:idxs) ))
				}
			}
			val ctrSize = getSize($ctrs, 0)
			$accum.reset
			recPipe( ctrSize - 1, Seq.empty[Rep[FixPt]] )
		})

		val pipe_1iter = static (Pipe) ("apply", Nil, CurriedMethodSignature(List(
			List(("func", MThunk(MUnit)))),
		MUnit)) 
		impl (pipe_1iter) (composite ${
			Pipe(CounterChain(Counter(max=1))) {case i => $func}
		})

		/* MetaPipeline */
		val MetaPipe = tpe("MetaPipe")

		/* MetaPipe Map  */
		val meta_map = static (MetaPipe) ("apply", Nil, CurriedMethodSignature(List(
			List(("pipelined", MBoolean), ("ctrs", CounterChain)),
			List(("mapFunc", varArgs(FixPt) ==> MUnit))),
		MUnit))
		impl (meta_map) (composite ${
			def recMetaPipe (idx:Int, idxs:Seq[Rep[FixPt]]): Rep[Unit] = {
				val ctr = $ctrs.chain.apply(unit(idx))
				if (idx == 0) {
					loop(ctr, ( (i:Rep[FixPt]) => $mapFunc( i+:idxs )) )
				} else {
					loop(ctr, ( (i:Rep[FixPt]) => recMetaPipe(idx - 1, i+:idxs) ))
				}
			}
			val ctrSize = getSize($ctrs, 0)
			recMetaPipe( ctrSize - 1, Seq.empty[Rep[FixPt]] )
		})
		static (MetaPipe) ("apply", Nil, CurriedMethodSignature(List(
			List(("ctrs", CounterChain)),
			List(("mapFunc", varArgs(FixPt) ==> MUnit))),
		MUnit)) implements redirect ${
				MetaPipe(unit(true), $ctrs) ($mapFunc)
			}
		direct (MetaPipe) ("Sequential", Nil, CurriedMethodSignature(List(
			List(("ctrs", CounterChain)),
			List(("mapFunc", varArgs(FixPt) ==> MUnit))),
		MUnit)) implements redirect ${
				MetaPipe(unit(false), $ctrs) ($mapFunc)
			}

		/* MetaPipe Reduction */
		val meta_reduce = static (MetaPipe) ("apply", T, CurriedMethodSignature(List(
			List(("pipelined", MBoolean), ("ctrs", CounterChain), ("accum", Reg(T)), ("reduceFunc", (T, T) ==> T)),
			List(("mapFunc", varArgs(FixPt) ==> T))),
		MUnit))
		impl (meta_reduce) (composite ${
			def recMetaPipe (idx:Int, idxs:Seq[Rep[FixPt]]): Rep[Unit] = {
				val ctr = $ctrs.chain.apply(unit(idx))
				if (idx == 0) {
					loop(ctr, ( (i:Rep[FixPt]) => $accum.write($reduceFunc($accum.value, $mapFunc(i+:idxs))) ))
				} else {
					loop(ctr, ( (i:Rep[FixPt]) => recMetaPipe(idx - 1, i+:idxs) ))
				}
			}
			$accum.reset
			val ctrSize = getSize($ctrs, 0)
			recMetaPipe( ctrSize - 1, Seq.empty[Rep[FixPt]] )
		})
		direct (MetaPipe) ("Sequential", T, CurriedMethodSignature(List(
			List(("ctrs", CounterChain), ("accum", Reg(T)), ("reduceFunc", (T, T) ==> T)),
			List(("mapFunc", varArgs(FixPt) ==> T))),
		MUnit)) implements
		redirect ${
			MetaPipe[T](unit(false), $ctrs, $accum, $reduceFunc) ($mapFunc)
		}

		val BRAM = lookupTpe("BRAM")
		val bram_reduce = direct (MetaPipe) ("BramReduce", T, CurriedMethodSignature(List(
			List(("pipelined", MBoolean), ("ctrs", CounterChain), ("bram", BRAM(T)),
			("reduceFunc", (T, T) ==> T)),
			List(("mapFunc", varArgs(FixPt) ==> BRAM(T)))),
		MUnit))
		impl (bram_reduce) (composite ${
			val bramSize = size($bram).size.reduce(_*_) 
			def recMetaPipe (idx:Int, idxs:Seq[Rep[FixPt]]): Rep[Unit] = {
				val ctr = $ctrs.chain.apply(unit(idx))
				if (idx == 0) {
					loop(ctr, { case i => 
						val bramCtr = CounterChain(Counter(max=bramSize))
						val resultBram = $mapFunc(i+:idxs)
						Pipe(bramCtr) {case j::_ =>
							bram.st(j, $reduceFunc($bram.ld(j), resultBram.ld(j))) 
						}
					})
				} else {
					loop(ctr, ( (i:Rep[FixPt]) => recMetaPipe(idx - 1, i+:idxs) ))
				}
			}
			//TODO: $brams.foreach(bram => bram.reset)
			val ctrSize = getSize($ctrs, 0)
			recMetaPipe( ctrSize - 1, Seq.empty[Rep[FixPt]] )
		})

		/* MetaPipe Parallel */
	 //TODO: should be list of funcs, but doesn't quite work
		val meta_parallel = direct (MetaPipe) ("Parallel", Nil, CurriedMethodSignature(List(
			List(("func", MThunk(MUnit) ))),
		MUnit)) 
		impl (meta_parallel) (codegen ($cala, ${
			$b[func]
		}))

		/* MetaPipe 1 iteration */
		val meta_1iter = static (MetaPipe) ("apply", Nil, CurriedMethodSignature(List(
			List(("func", MThunk(MUnit)))),
		MUnit)) 
		impl (meta_1iter) (composite ${
			Sequential(CounterChain(Counter(max=unit(1)))) { case i => $func}
		})

	}
}

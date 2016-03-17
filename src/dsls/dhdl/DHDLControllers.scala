package ppl.dsl.forge
package dsls
package dhdl

trait DHDLControllers {
  this: DHDLDSL =>

  def importDHDLControllers() {
    importCounters()
    importCounterChains()
    importPipes()
  }

  // TODO: Move to separate file?
  def importIndices() {
    val Indices = lookupTpe("Indices")
    val Fix = lookupTpe("Fix")
    internal (Indices) ("indices_new", Nil, SList(Fix) :: Indices) implements record(Indices, ("i", SList(Fix), quotedArg(0)))
    static (Indices) ("apply", Nil, varArgs(Fix) :: Indices) implements composite ${ indices_new($0.toList) }
    infix (Indices) ("apply", Nil, (Indices, SInt) :: Fix) implements composite ${ field[Fix]($0, "i_" + $1) }
    internal.infix (Indices) ("toList", Nil, (Indices, SInt) :: SList(Fix)) implements composite ${ List.tabulate($1){i => $0(i)} }
  }

  // TODO: Add syntax sugar to chain counters?
  def importCounters() {
    val Fix = lookupTpe("Fix")
    val Counter = lookupTpe("Counter")
    val LoopRange = lookupTpe("LoopRange")

    // --- Loop Range
    data(LoopRange, ("_start", Fix), ("_end", Fix), ("_step", Fix))
    internal.infix (LoopRange) ("start", Nil, LoopRange :: Fix) implements getter(0, "_start")
    internal.infix (LoopRange) ("end", Nil, LoopRange :: Fix) implements getter(0, "_end")
    internal.infix (LoopRange) ("step", Nil, LoopRange :: Fix) implements getter(0, "_step")
    internal.static (LoopRange) ("apply", Nil, (Fix,Fix,Fix) :: LoopRange) implements allocates(LoopRange, ${$0},${$1},${$2})

    fimplicit (LoopRange) ("rangeToCounter", Nil, LoopRange :: Counter) implements composite ${ counter_create(None, $0, 1) }


    // --- Nodes
		val counter_new = internal (Counter) ("counter_new", Nil, (("start", Fix), ("end", Fix), ("step", Fix)) :: Counter)
    val counter_mkstring = internal (Counter) ("counter_makestring", Nil, Counter :: MString)

    // --- Internals
    internal (Counter) ("counter_create", Nil, (SOption(SString), LoopRange, SInt) :: Counter) implements composite ${
      val ctr = counter_new($1.start, $1.end, $1.step)
      $0.foreach{name => nameOf(ctr) = name}
      par(ctr) = $2
      ctr
    }

    // --- API
    infix (LoopRange) ("until", Nil, (Fix,Fix) :: LoopRange) implements composite ${ LoopRange($0, $1, 1) }
    infix (LoopRange) ("by", Nil, (LoopRange, Fix) :: LoopRange) implements composite ${ LoopRange($0.start, $0.end, $1) }
    infix (LoopRange) ("by", Nil, (Fix, Fix) :: LoopRange) implements composite ${ LoopRange(0, $0, $1) }

    static (Counter) ("apply", Nil, ("max", Fix) :: Counter) implements composite ${ counter_create(None, 0 until $max, 1) }
    static (Counter) ("apply", Nil, (("name", SString), ("max",Fix)) :: Counter) implements composite ${ counter_create(Some($name), 0 until $max, 1) }
		static (Counter) ("apply", Nil, LoopRange :: Counter) implements composite ${ counter_create(None, $0, 1) }
    static (Counter) ("apply", Nil, (SString, LoopRange) :: Counter) implements composite ${ counter_create(Some($0), $1, 1) }
	  static (Counter) ("apply", Nil, (SString, LoopRange, SInt) :: Counter) implements composite ${ counter_create(Some($0), $1, $2) }

		val Counter_API = withTpe(Counter)
		Counter_API {
			infix ("name") (Nil :: SString) implements composite ${ nameOf($self) }
      infix ("mkString") (Nil :: MString) implements composite ${ counter_makestring($self) }
		}

    // --- Scala Backend
    impl (counter_new) (codegen($cala, ${ ($start until $end by $step) }))
    impl (counter_mkstring) (codegen($cala, ${
      @ val name = nameOf($0)
      "counter(name: " + $name + ", start: " + $0.start + ", end: " + $0.end + ", step: " + $0.step + ")"
    }))

    // --- Dot Backend
    impl (counter_new) (codegen(dot, ${
			@ val cic = "\\"" + quote(counterInnerColor) + "\\""
      $sym [ label="\$sym" shape="box" style="filled,rounded"
						color=$cic ]
			$start -> $sym [ label="start" ]
			$end -> $sym [ label="end" ]
			$step -> $sym [ label="step" ]
			$start [style="invisible" height=0 size=0 margin=0 label=""]
			$end [style="invisible" height=0 size=0 margin=0 label=""]
			$step [style="invisible" height=0 size=0 margin=0 label=""]
		}))

  }


  def importCounterChains() {
    val Counter = lookupTpe("Counter")
    val CounterChain = lookupTpe("CounterChain")

    // --- Nodes
    // counterchain_new - see extern
    val cchain_mkstring = internal (CounterChain) ("counterchain_makestring", Nil, (CounterChain, Counter ==> MString) :: MString)

    // --- API
    static (CounterChain) ("apply", Nil, varArgs(Counter) :: CounterChain) implements composite ${
      val chain = counterchain_new($0.toList)
      sizeOf(chain) = List($0.length)
      chain
    }

		val CounterChain_API = withTpe(CounterChain)
		CounterChain_API {
			infix ("length") (Nil :: SInt) implements composite ${ getDim($self, 0) }
      infix ("mkString") (Nil :: MString) implements composite ${ counterchain_makestring($self, {ctr => ctr.mkString}) }
		}

    // --- Scala Backend
    //impl (cchain_new) (codegen($cala, ${ $0 }))
    impl (cchain_mkstring) (codegen($cala, ${ "ctrchain[" + $0.map{ctr => $b[1](ctr) }.mkString(", ") + "]" }))
  }


  // TODO: API for reduction across multiple block RAMs?
  def importPipes() {
    val T = tpePar("T")
    val C = hkTpePar("C", T) // high kinded type parameter - memory of type T

    val BRAM = lookupTpe("BRAM")
    val Fix = lookupTpe("Fix")

    val Indices = lookupTpe("Indices")

    val Counter = lookupTpe("Counter")
    val CounterChain = lookupTpe("CounterChain")

    val Pipe = grp("Pipe")
		val MetaPipe = grp("MetaPipe")
    val Sequential = grp("Sequential")

    // --- Nodes
    // pipe_foreach, pipe_reduce, pipe_bram_reduce - see Template in extern
    val pipe_parallel = internal (MetaPipe) ("pipe_parallel", Nil, ("func", MThunk(MUnit)) :: MUnit, effect = simple)
    val block_reduce = internal (MetaPipe) ("block_reduce", T, (CounterChain, CounterChain, BRAM(T), Fix ==> BRAM(T), (T,T) ==> T) :: MUnit, effect = simple)

    // --- API
    val grps = List(Pipe, MetaPipe, Sequential)
    val styles = List("Fine", "Coarse", "Disabled")
    val objs = List("Pipe", "MetaPipe", "Sequential")

    for (i <- 0 until 3) {  // Generate for Pipe, MetaPipe, and Sequential
      val Ctrl = grps(i)
      val style = styles(i)
      val obj = objs(i)

      /* Foreach */
      static (Ctrl) ("foreach", Nil, CurriedMethodSignature(List(List(CounterChain),List(Indices ==> MUnit)), MUnit)) implements composite ${
        val pipe = pipe_foreach($0, $1)
        styleOf(pipe) = \$style
      }
      static (Ctrl) ("apply", Nil, CurriedMethodSignature(List(List(Counter), List(Fix ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0)){inds => $1(inds(0)) }
      }
      static (Ctrl) ("apply", Nil, CurriedMethodSignature(List(List(Counter,Counter), List((Fix,Fix) ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0,$1)){inds => $2(inds(0),inds(1)) }
      }
      static (Ctrl) ("apply", Nil, CurriedMethodSignature(List(List(Counter,Counter,Counter), List((Fix,Fix,Fix) ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0,$1,$2)){inds => $3(inds(0),inds(1),inds(2)) }
      }

      /* Reduce */
      // Pipe.reduce(chain, accum){inds => f(inds)}{(a,b) => reduce(a,b) }
      static (Ctrl) ("reduce", (T,C), CurriedMethodSignature(List(List(CounterChain, C(T)), List(Indices ==> T), List((T,T) ==> T)), MUnit), TMem(T,C(T))) implements composite ${
        val pipe = pipe_reduce[T,C]($0, $1, $2, $3)
        styleOf(pipe) = \$style
      }
      // Pipe(counter, accum){i => f(i) }{(a,b) => reduce(a,b) }
      static (Ctrl) ("apply", (T,C), CurriedMethodSignature(List(List(Counter, C(T)), List(Fix ==> T), List((T,T) ==> T)), MUnit), TMem(T,C(T))) implements composite ${
        \$obj.reduce(CounterChain($0), $1){inds => $2(inds(0)) }($3)
      }
      // Pipe(counter, counter, accum){(i,j) => f(i,j)}{(a,b) => reduce(a,b) }
      static (Ctrl) ("apply", (T,C), CurriedMethodSignature(List(List(Counter, Counter, C(T)), List((Fix,Fix) ==> T), List((T,T) ==> T)), MUnit), TMem(T,C(T))) implements composite ${
        \$obj.reduce(CounterChain($0,$1), $2){inds => $3(inds(0), inds(1))}($4)
      }
      // Pipe(counter, counter, counter, accum){(i,j,k) => f(i,j,k)}{(a,b) => reduce(a,b) }
      static (Ctrl) ("apply", (T,C), CurriedMethodSignature(List(List(Counter, Counter, Counter, C(T)), List((Fix,Fix,Fix) ==> T), List((T,T) ==> T)), MUnit), TMem(T,C(T))) implements composite ${
        \$obj.reduce(CounterChain($0,$1,$2), $3){inds => $4(inds(0), inds(1), inds(2))}($5)
      }

      /* Single iteration */
      static (Ctrl) ("apply", Nil, MThunk(MUnit) :: MUnit) implements composite ${ \$obj(Counter(max=1)){i => $0} }
    }

    /* Parallel */
    direct (MetaPipe) ("Parallel", Nil, MThunk(MUnit) :: MUnit) implements composite ${ pipe_parallel($0) }


    // TODO: Only single dimensional for now
    direct (MetaPipe) ("BlockReduce", T, CurriedMethodSignature(List(List(Counter, BRAM(T)), List(Fix ==> BRAM(T)), List((T,T) ==> T)), MUnit)) implements composite ${
      val bramSize = sizeOf($1).reduce(_*_)
      block_reduce(CounterChain($0), CounterChain(bramSize by 1), $1, $2, $3)
    }


    // --- Scala Backend
    // See TemplateOpsExp for others
    impl (pipe_parallel) (codegen ($cala, ${ $b[func] }))

    impl (block_reduce) (codegen($cala, ${
      for (__iter <- $0.apply(0)) {
        val __res = $b[3](__iter)

        for (__upIter <- $1.apply(0)) {
          val __a = __res.apply(__upIter.toInt)
          val __b = $2.apply(__upIter.toInt)
          val __r = $b[4](__a,__b)
          $2.update(__upIter.toInt, __r)
        }
      }
    }))

    // --- Dot Backend
    impl (pipe_parallel) (codegen (dot, ${
      subgraph cluster_$sym {
      	label = "parallel_\$sym"
      	style = "filled"
      	fillcolor = "$parallelFillColor "
      	color = "$parallelBorderColor "
				$b[func]
			}
			
		}))

    impl (block_reduce) (codegen(dot, ${
      subgraph $sym {
      	label = "\$sym"
      	style = "filled"
      	fillcolor = "$mpFillColor "
      	color = "$mpBorderColor "
				@ val sym_ctrl = sym + "_ctrl"
      	$sym_ctrl [label="ctrl" height=0 style="filled" fillcolor="$mpBorderColor "]
			}
    }))

		/*val bram_reduce = direct (MetaPipe) ("BramReduce", T, CurriedMethodSignature(List(
			List(("pipelined", MBoolean), ("ctrs", CounterChain), ("bram", BRAM(T)),
			("reduceFunc", (T, T) ==> T)),
			List(("mapFunc", varArgs(Fix) ==> BRAM(T)))),
		MUnit))
		impl (bram_reduce) (composite ${
			val bramSize = sizeOf($bram).reduce(_*_)
			def recMetaPipe (idx:Int, idxs:Seq[Rep[Fix]]): Rep[Unit] = {
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
					loop(ctr, ( (i:Rep[Fix]) => recMetaPipe(idx - 1, i+:idxs) ))
				}
			}
			//TODO: $brams.foreach(bram => bram.reset)
			val ctrSize = getDim($ctrs, 0)
			recMetaPipe( ctrSize - 1, Seq.empty[Rep[Fix]] )
		})*/
	}
}

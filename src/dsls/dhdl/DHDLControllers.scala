package ppl.dsl.forge
package dsls
package dhdl

trait DHDLControllers {
  this: DHDLDSL =>

  def importDHDLControllers() {
    importCounters()
    importPipes()
  }

  // TODO: Add syntax sugar to chain counters?
  def importCounters() {
    val Counter      = lookupTpe("Counter")
    val CounterChain = lookupTpe("CounterChain")
    val Idx          = lookupAlias("SInt")

    // --- Nodes
    val counter_new = internal (Counter) ("counter_new", Nil, (("start", Idx), ("end", Idx), ("step", Idx)) :: Counter, effect = simple)

    // --- Internals
    direct (Counter) ("counter_create", Nil, (SOption(SString), Idx, Idx, Idx, SInt) :: Counter) implements composite ${
      val ctr = counter_new($1, $2, $3)
      $0.foreach{name => nameOf(ctr) = name}
      par(ctr) = $4
      ctr
    }

    // --- API
    static (Counter) ("apply", Nil, ("max", Idx) :: Counter) implements redirect ${ counter_create(None, 0, $max, 1, 1) }
    static (Counter) ("apply", Nil, (("min", Idx), ("max", Idx)) :: Counter) implements redirect ${ counter_create(None, $min, $max, 1, 1) }
    static (Counter) ("apply", Nil, (("min", Idx), ("max", Idx), ("step", Idx)) :: Counter) implements redirect ${ counter_create(None, $min, $max, $step, 1) }
    static (Counter) ("apply", Nil, (("name",SString), ("max",Idx)) :: Counter) implements redirect ${ counter_create(Some($name), 0, $max, 1, 1) }
    static (Counter) ("apply", Nil, (("name",SString), ("min", Idx), ("max", Idx)) :: Counter) implements redirect ${ counter_create(Some($name), $min, $max, 1, 1) }
    static (Counter) ("apply", Nil, (("name",SString), ("min",Idx), ("max",Idx), ("step",Idx)) :: Counter) implements redirect ${ counter_create(Some($name), $min, $max, $step, 1) }
    static (Counter) ("apply", Nil, (("name",SString), ("min",Idx), ("max",Idx), ("step",Idx), ("par",SInt)) :: Counter) implements redirect ${ counter_create(Some($name), $min, $max, $step, $par) }


    static (CounterChain) ("apply", Nil, varArgs(Counter) :: CounterChain) implements composite ${
      val chain = counterchain_new($0.toList) // Defined in extern
      sizeOf(chain) = $0.length
      chain
    }

    // --- Scala backend
    impl (counter_new) (codegen($cala, ${ ($start until $end by $step) }))

    // --- Dot Backend
    // Moved this outside - was confusing sublime

    impl (counter_new) (codegen(dot, ${
			@ var l = "\\"" + quote(sym)
			@ if (quote(start).forall(_.isDigit)) {
			@ 	l += "|start=" + quote(start)
			@ } else {
					$start -> $sym [headlabel="start"]
			@ }
			@ if (quote(end).forall(_.isDigit)) {
			@ 	l += "|end=" + quote(end)
			@ } else {
					$end -> $sym [headlabel="end"]
			@ }
			@ if (quote(step).forall(_.isDigit)) {
			@ 	l += "|step=" + quote(step)
			@ } else {
					$step -> $sym [headlabel="step"]
			@ }
			@ l += "\\"" 
      $sym [ label=$l shape="record" style="filled,rounded"
						color=$counterInnerColor ]
		}))

    // --- MaxJ Backend
    impl (counter_new) (codegen(maxj, ${
		}))
  }


  def importPipes() {
    val T = tpePar("T")
    val C = hkTpePar("C", T) // high kinded type parameter - memory of type T

    val BRAM = lookupTpe("BRAM")
    val Idx  = lookupAlias("SInt")

    val Indices = lookupTpe("Indices")

    val Counter = lookupTpe("Counter")
    val CounterChain = lookupTpe("CounterChain")

    val Pipe = grp("Pipe")
    val MetaPipe = grp("MetaPipe")
    val Sequential = grp("Sequential")

    // --- Nodes
    // pipe_foreach, pipe_reduce, pipe_bram_reduce - see Template in extern
    val pipe_parallel = internal (MetaPipe) ("pipe_parallel", Nil, ("func", MThunk(MUnit)) :: MUnit, effect = simple)
    val block_reduce = internal (MetaPipe) ("block_reduce", T, (CounterChain, CounterChain, BRAM(T), Idx ==> BRAM(T), (T,T) ==> T) :: MUnit, effect = simple)

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
      static (Ctrl) ("apply", Nil, CurriedMethodSignature(List(List(Counter), List(Idx ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0)){inds => $1(inds(0)) }
      }
      static (Ctrl) ("apply", Nil, CurriedMethodSignature(List(List(Counter,Counter), List((Idx,Idx) ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0,$1)){inds => $2(inds(0),inds(1)) }
      }
      static (Ctrl) ("apply", Nil, CurriedMethodSignature(List(List(Counter,Counter,Counter), List((Idx,Idx,Idx) ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0,$1,$2)){inds => $3(inds(0),inds(1),inds(2)) }
      }

      /* Reduce */
      // Pipe.reduce(chain, accum){inds => f(inds)}{(a,b) => reduce(a,b) }
      static (Ctrl) ("reduce", (T,C), CurriedMethodSignature(List(List(CounterChain, C(T)), List(Indices ==> T), List((T,T) ==> T)), MUnit), TMem(T,C(T))) implements composite ${
        val pipe = pipe_reduce[T,C]($0, $1, $2, $3)
        styleOf(pipe) = \$style
      }
      // Pipe(counter, accum){i => f(i) }{(a,b) => reduce(a,b) }
      static (Ctrl) ("apply", (T,C), CurriedMethodSignature(List(List(Counter, C(T)), List(Idx ==> T), List((T,T) ==> T)), MUnit), TMem(T,C(T))) implements composite ${
        \$obj.reduce(CounterChain($0), $1){inds => $2(inds(0)) }($3)
      }
      // Pipe(counter, counter, accum){(i,j) => f(i,j)}{(a,b) => reduce(a,b) }
      static (Ctrl) ("apply", (T,C), CurriedMethodSignature(List(List(Counter, Counter, C(T)), List((Idx,Idx) ==> T), List((T,T) ==> T)), MUnit), TMem(T,C(T))) implements composite ${
        \$obj.reduce(CounterChain($0,$1), $2){inds => $3(inds(0), inds(1))}($4)
      }
      // Pipe(counter, counter, counter, accum){(i,j,k) => f(i,j,k)}{(a,b) => reduce(a,b) }
      static (Ctrl) ("apply", (T,C), CurriedMethodSignature(List(List(Counter, Counter, Counter, C(T)), List((Idx,Idx,Idx) ==> T), List((T,T) ==> T)), MUnit), TMem(T,C(T))) implements composite ${
        \$obj.reduce(CounterChain($0,$1,$2), $3){inds => $4(inds(0), inds(1), inds(2))}($5)
      }

      /* Single iteration */
      static (Ctrl) ("apply", Nil, MThunk(MUnit) :: MUnit) implements composite ${ \$obj(Counter(max=1)){i => $0} }
    }

    /* Parallel */
    direct (MetaPipe) ("Parallel", Nil, MThunk(MUnit) :: MUnit) implements composite ${ pipe_parallel($0) }


    // TODO: Only single dimensional for now
    direct (MetaPipe) ("BlockReduce", T, CurriedMethodSignature(List(List(Counter, BRAM(T)), List(Idx ==> BRAM(T)), List((T,T) ==> T)), MUnit)) implements composite ${
      val bramSize = sizeOf($1)
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
          val __r = if (__iter > FixedPoint[Signed,B32,B0](1)) { // Only reduce after first iteration
            val __b = $2.apply(__upIter.toInt)
            val __out = $b[4](__a,__b)
            __out
          }
          else {
            __a
          }
          $2.update(__upIter.toInt, __r)
        }
      }
    }))

    // --- Dot Backend
    impl (pipe_parallel) (codegen (dot, ${
      subgraph cluster_$sym {
      	label = "parallel_\$sym"
      	style = "filled, bold"
      	fillcolor = $parallelFillColor
      	color = $parallelBorderColor
      	@ stream.println(emitBlock(func) + "")
			}
		}))

		//TODO
    impl (block_reduce) (codegen(dot, ${
      subgraph $sym {
      	label = "\$sym"
      	style = "filled"
      	fillcolor = $mpFillColor
      	color = $mpBorderColor
				@ val sym_ctrl = quote(sym) + "_blockreduce"
      	$sym_ctrl [label="ctrl" height=0 style="filled" fillcolor=$mpBorderColor ]
			}
    }))

    impl (block_reduce) (codegen(maxj, ${
    }))
	}

}

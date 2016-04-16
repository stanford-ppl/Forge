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
    val Idx          = lookupAlias("Index")

    // --- Nodes
    // counter_new - see extern

    // --- Internals
    direct (Counter) ("counter_create", Nil, (SOption(SString), Idx, Idx, Idx, MInt) :: Counter) implements composite ${
      val ctr = counter_new($1, $2, $3, $4)
      $0.foreach{name => nameOf(ctr) = name}
      ctr
    }

    // --- API
    static (Counter) ("apply", Nil, ("max", Idx) :: Counter) implements redirect ${ counter_create(None, 0.as[Index], $max, 1.as[Index], param(1)) }
    static (Counter) ("apply", Nil, (("min", Idx), ("max", Idx)) :: Counter) implements redirect ${ counter_create(None, $min, $max, 1.as[Index], param(1)) }
    static (Counter) ("apply", Nil, (("min", Idx), ("max", Idx), ("step", Idx)) :: Counter) implements redirect ${ counter_create(None, $min, $max, $step, param(1)) }
    static (Counter) ("apply", Nil, (("min", Idx), ("max",Idx), ("step",Idx), ("par",MInt)) :: Counter) implements redirect ${ counter_create(None, $min, $max, $step, $par) }
    static (Counter) ("apply", Nil, (("name",SString), ("max",Idx)) :: Counter) implements redirect ${ counter_create(Some($name), 0.as[Index], $max, 1.as[Index], param(1)) }
    static (Counter) ("apply", Nil, (("name",SString), ("min", Idx), ("max", Idx)) :: Counter) implements redirect ${ counter_create(Some($name), $min, $max, 1.as[Index], param(1)) }
    static (Counter) ("apply", Nil, (("name",SString), ("min",Idx), ("max",Idx), ("step",Idx)) :: Counter) implements redirect ${ counter_create(Some($name), $min, $max, $step, param(1)) }
    static (Counter) ("apply", Nil, (("name",SString), ("min",Idx), ("max",Idx), ("step",Idx), ("par",MInt)) :: Counter) implements redirect ${ counter_create(Some($name), $min, $max, $step, $par) }


    static (CounterChain) ("apply", Nil, varArgs(Counter) :: CounterChain) implements composite ${
      val chain = counterchain_new($0.toList) // Defined in extern
      lenOf(chain) = $0.length
      chain
    }

  }


  def importPipes() {
    val T = tpePar("T")
    val C = hkTpePar("C", T) // high kinded type parameter - memory of type T

    val BRAM = lookupTpe("BRAM")
    val Idx  = lookupAlias("Index")

    val Indices = lookupTpe("Indices")

    val Counter = lookupTpe("Counter")
    val CounterChain = lookupTpe("CounterChain")
    val Pipeline = lookupTpe("Pipeline")

    val Pipe = grp("Pipe")
    val MetaPipe = grp("MetaPipe")
    val Sequential = grp("Sequential")

    // --- Nodes
    // pipe_foreach, pipe_reduce, block_reduce - see Template in extern
    val pipe_parallel = internal (MetaPipe) ("pipe_parallel", Nil, ("func", MThunk(MUnit)) :: MUnit, effect = simple)
    val unit_pipe = internal (MetaPipe) ("unit_pipe", Nil, ("func", MThunk(MUnit)) :: Pipeline, effect = simple)

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
      // Single iteration MetaPipe makes no sense
      if (style != "MetaPipe") {
        static (Ctrl) ("apply", Nil, MThunk(MUnit) :: MUnit) implements composite ${
          val pipe = unit_pipe($0)
          styleOf(pipe) = \$style
        }
      }
    }

    /* Parallel */
    direct (MetaPipe) ("Parallel", Nil, MThunk(MUnit) :: MUnit) implements composite ${ pipe_parallel($0) }

    direct (MetaPipe) ("block_reduce_create", T, (CounterChain, MInt, BRAM(T), Indices ==> BRAM(T), (T,T) ==> T) :: MUnit) implements composite ${
      val tileDims = dimsOf($2)
      val ctrs = tileDims.zipWithIndex.map{ case (d,i) =>
        if (i != tileDims.length - 1) Counter(max = d)
        else Counter(min = 0.as[Index], max = d, step = 1.as[Index], par = $1)
      }
      val chain = CounterChain(ctrs:_*)
      val pipe = block_reduce($0, chain, $2, $3, $4)
      styleOf(pipe) = Coarse
    }

    /* BlockReduce */
    // BlockReduce(counter, accum){i => f(i) }{(a,b) => reduce(a,b) }
    direct (MetaPipe) ("BlockReduce", T, CurriedMethodSignature(List(List(Counter, BRAM(T)), List(Idx ==> BRAM(T)), List((T,T) ==> T)), MUnit)) implements composite ${
      block_reduce_create[T](CounterChain($0), param(1), $1, {inds: Rep[Indices] => $2(inds(0)) }, $3)
    }
    // BlockReduce(counter, counter, accum){i => f(i) }{(a,b) => reduce(a,b) }
    direct (MetaPipe) ("BlockReduce", T, CurriedMethodSignature(List(List(Counter, Counter, BRAM(T)), List((Idx,Idx) ==> BRAM(T)), List((T,T) ==> T)), MUnit)) implements composite ${
      block_reduce_create[T](CounterChain($0, $1), param(1), $2, {inds: Rep[Indices] => $3(inds(0), inds(1))}, $4)
    }
    // BlockReduce(counter, counter, counter, accum){i => f(i) }{(a,b) => reduce(a,b) }
    direct (MetaPipe) ("BlockReduce", T, CurriedMethodSignature(List(List(Counter, Counter, Counter, BRAM(T)), List((Idx,Idx,Idx) ==> BRAM(T)), List((T,T) ==> T)), MUnit)) implements composite ${
      block_reduce_create[T](CounterChain($0, $1, $2), param(1), $3, {inds: Rep[Indices] => $4(inds(0), inds(1), inds(2))}, $5)
    }

    // HACK: Explicit par factor
    direct (MetaPipe) ("BlockReduce", T, CurriedMethodSignature(List(List(Counter, BRAM(T), MInt), List(Idx ==> BRAM(T)), List((T,T) ==> T)), MUnit)) implements composite ${
      block_reduce_create[T](CounterChain($0), $2, $1, {inds: Rep[Indices] => $3(inds(0)) }, $4)
    }

    // --- Scala Backend
    // See TemplateOpsExp for others
    impl (pipe_parallel) (codegen ($cala, ${ $b[func] }))
    impl (unit_pipe) (codegen ($cala, ${ $b[func] }))

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

	}

}

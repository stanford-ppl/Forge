package ppl.dsl.forge
package dsls
package dhdl

@dsl
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
    /** @nodoc **/
    direct (Counter) ("counter_create", Nil, (SOption(SString), Idx, Idx, Idx, MInt) :: Counter) implements composite ${
      val ctr = counter_new($1, $2, $3, $4)
      $0.foreach{name => nameOf(ctr) = name}
      ctr
    }

    // --- API
    /** Creates an unnamed Counter with min of 0, given max, and step size of 1 **/
    static (Counter) ("apply", Nil, ("max", Idx) :: Counter) implements redirect ${ counter_create(None, 0.as[Index], $max, 1.as[Index], param(1)) }
    /** Creates an unnamed Counter with given min and max, and step size of 1 **/
    static (Counter) ("apply", Nil, (("min", Idx), ("max", Idx)) :: Counter) implements redirect ${ counter_create(None, $min, $max, 1.as[Index], param(1)) }
    /** Creates an unnamed Counter with given min, max and step size **/
    static (Counter) ("apply", Nil, (("min", Idx), ("max", Idx), ("step", Idx)) :: Counter) implements redirect ${ counter_create(None, $min, $max, $step, param(1)) }
    /** Creates an unnamed Counter with given min, max, step size, and parallelization factor **/
    static (Counter) ("apply", Nil, (("min", Idx), ("max",Idx), ("step",Idx), ("par",MInt)) :: Counter) implements redirect ${ counter_create(None, $min, $max, $step, $par) }
    /** Creates a named Counter with min of 0, given max, and step size of 1 **/
    static (Counter) ("apply", Nil, (("name",SString), ("max",Idx)) :: Counter) implements redirect ${ counter_create(Some($name), 0.as[Index], $max, 1.as[Index], param(1)) }
    /** Creates a named Counter with given min and max, and step size of 1 **/
    static (Counter) ("apply", Nil, (("name",SString), ("min", Idx), ("max", Idx)) :: Counter) implements redirect ${ counter_create(Some($name), $min, $max, 1.as[Index], param(1)) }
    /** Creates a named Counter with given min, max and step size **/
    static (Counter) ("apply", Nil, (("name",SString), ("min",Idx), ("max",Idx), ("step",Idx)) :: Counter) implements redirect ${ counter_create(Some($name), $min, $max, $step, param(1)) }
    /** Creates a named Counter with given min, max, step size, and parallelization factor **/
    static (Counter) ("apply", Nil, (("name",SString), ("min",Idx), ("max",Idx), ("step",Idx), ("par",MInt)) :: Counter) implements redirect ${ counter_create(Some($name), $min, $max, $step, $par) }

    /** Creates a chain of counters. Order is specified as outermost to innermost **/
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

    val ControlType = lookupTpe("ControlType", stage=compile)
    val Pipe = grp("Pipe")
    val Sequential = grp("Sequential")
    val Parallel = grp("Parallel")

    // --- Nodes
    // pipe_foreach, pipe_reduce, block_reduce - see Template in extern
    val pipe_parallel = internal (Parallel) ("pipe_parallel", Nil, ("func", MThunk(MUnit)) :: MUnit, effect = simple)
    val unit_pipe = internal (Pipe) ("unit_pipe", Nil, ("func", MThunk(MUnit)) :: MUnit, effect = simple)

    val controllers = List(Pipe, Sequential)
    val styles  = List("Fine", "Disabled")
    val objects = List("Pipe", "Sequential")

    (controllers, styles, objects).zipped.foreach{case (ctrl, style, obj) =>

      static (ctrl) ("foreach", Nil, CurriedMethodSignature(List(List(CounterChain),List(Indices ==> MUnit)), MUnit)) implements composite ${
        val pipe = pipe_foreach($0, $1)
        styleOf(pipe) = \$style
      }
      static (ctrl) ("foreach", Nil, CurriedMethodSignature(List(List(Counter),List(Idx ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0)){inds => $1(inds(0))}
      }
      static (ctrl) ("foreach", Nil, CurriedMethodSignature(List(List(Counter,Counter),List((Idx,Idx) ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0,$1)){inds => $2(inds(0),inds(1))}
      }
      static (ctrl) ("foreach", Nil, CurriedMethodSignature(List(List(Counter,Counter,Counter),List((Idx,Idx,Idx) ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0,$1,$2)){inds => $3(inds(0),inds(1),inds(2))}
      }

      // Conflicts with unit pipe...
      static (ctrl) ("apply", Nil, CurriedMethodSignature(List(List(CounterChain),List(Indices ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach($0)($1)
      }
      static (ctrl) ("apply", Nil, CurriedMethodSignature(List(List(Counter),List(Idx ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0)){inds => $1(inds(0))}
      }
      static (ctrl) ("apply", Nil, CurriedMethodSignature(List(List(Counter,Counter),List((Idx,Idx) ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0,$1)){inds => $2(inds(0),inds(1))}
      }
      static (ctrl) ("apply", Nil, CurriedMethodSignature(List(List(Counter,Counter,Counter),List((Idx,Idx,Idx) ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0,$1,$2)){inds => $3(inds(0),inds(1),inds(2))}
      }

      // Scalar reduction
      // Originally wanted to have scalar and collection reduction have the same syntax, but the difference
      // between Idx => T and Idx => C[T] is ambiguous
      static (ctrl) ("reduce", (T,C), CurriedMethodSignature(List(List(CounterChain), List(C(T)), List(Indices ==> T), List((T,T) ==> T)), C(T)), TMem(T,C(T))) implements composite ${
        val pipe = pipe_fold[T,C]($0, $1, $2, $3)
        styleOf(pipe) = \$style
        $1
      }
      static (ctrl) ("reduce", (T,C), CurriedMethodSignature(List(List(Counter), List(C(T)), List(Idx ==> T), List((T,T) ==> T)), C(T)), TMem(T,C(T))) implements composite ${
        \$obj.reduce(CounterChain($0))($1){inds => $2(inds(0))}($3)
      }
      static (ctrl) ("reduce", (T,C), CurriedMethodSignature(List(List(Counter,Counter), List(C(T)), List((Idx,Idx) ==> T), List((T,T) ==> T)), C(T)), TMem(T,C(T))) implements composite ${
        \$obj.reduce(CounterChain($0,$1))($2){inds => $3(inds(0),inds(1))}($4)
      }
      static (ctrl) ("reduce", (T,C), CurriedMethodSignature(List(List(Counter,Counter,Counter), List(C(T)), List((Idx,Idx,Idx) ==> T), List((T,T) ==> T)), C(T)), TMem(T,C(T))) implements composite ${
        \$obj.reduce(CounterChain($0,$1,$2))($3){inds => $4(inds(0),inds(1),inds(2))}($5)
      }

      // Accumulator reduction
      static (ctrl) ("fold", (T,C), CurriedMethodSignature(List(List(CounterChain, MInt),List(C(T)),List(Indices ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T))) implements composite ${
        val ccInner = $2.iterator(List($1))
        val pipe = accum_fold[T,C]($0, ccInner, $2, $3, $4)
        styleOf(pipe) = \$style
        $2
      }
      static (ctrl) ("fold", (T,C), CurriedMethodSignature(List(List(Counter),List(C(T)),List(Idx ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T))) implements composite ${
        \$obj.fold(CounterChain($0),param(1))($1){inds => $2(inds(0))}($3)
      }
      static (ctrl) ("fold", (T,C), CurriedMethodSignature(List(List(Counter,Counter),List(C(T)),List((Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T))) implements composite ${
        \$obj.fold(CounterChain($0,$1),param(1))($2){inds => $3(inds(0),inds(1))}($4)
      }
      static (ctrl) ("fold", (T,C), CurriedMethodSignature(List(List(Counter,Counter,Counter),List(C(T)),List((Idx,Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T))) implements composite ${
        \$obj.fold(CounterChain($0,$1,$2),param(1))($3){inds => $4(inds(0),inds(1),inds(2))}($5)
      }

      // Fold with explicit inner parallelization factor (unused for Regs)
      static (ctrl) ("fold", (T,C), CurriedMethodSignature(List(List(Counter,MInt),List(C(T)),List(Idx ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T))) implements composite ${
        \$obj.fold(CounterChain($0),$1)($2){inds => $3(inds(0))}($4)
      }
      static (ctrl) ("fold", (T,C), CurriedMethodSignature(List(List(Counter,Counter,MInt),List(C(T)),List((Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T))) implements composite ${
        \$obj.fold(CounterChain($0,$1),$2)($3){inds => $4(inds(0),inds(1))}($5)
      }
      static (ctrl) ("fold", (T,C), CurriedMethodSignature(List(List(Counter,Counter,Counter,MInt),List(C(T)),List((Idx,Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T))) implements composite ${
        \$obj.fold(CounterChain($0,$1,$2),$3)($4){inds => $5(inds(0),inds(1),inds(2))}($6)
      }

      // Unit Pipe
      static (ctrl) ("apply", Nil, MThunk(MUnit) :: MUnit) implements composite ${
        val pipe = unit_pipe($0)
        styleOf(pipe) = \$style
      }
    }

    // --- Parallel
    static (Parallel) ("apply", Nil, MThunk(MUnit) :: MUnit) implements composite ${
      val pipe = pipe_parallel($0)
      styleOf(pipe) = ForkJoin
    }

    // --- Scala Backend
    // See TemplateOpsExp for others
    impl (pipe_parallel) (codegen ($cala, ${
      $b[func]
      ()
    }))
    impl (unit_pipe) (codegen($cala, ${
      $b[func]
      ()
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
    // --- MaxJ Backend
    //pipe_parallel (extern)

	}

}

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
    val StateMachine = lookupTpe("StateMachine")

    val ControlType = lookupTpe("ControlType", stage=compile)
    val Pipe = grp("Pipe")
    val Sequential = grp("Sequential")
    val Parallel = grp("Parallel")

    data(StateMachine, ("_cchain", CounterChain))

    internal (StateMachine) ("pipeline_new", Nil, CounterChain :: StateMachine) implements allocates(StateMachine, ${$0})
    internal (StateMachine) ("pipeline", Nil, (CounterChain, ControlType) :: StateMachine) implements composite ${
      val pipe = pipeline_new($0)
      styleOf(pipe) = $1
      pipe
    }
    internal.infix (StateMachine) ("cchain", Nil, StateMachine :: CounterChain) implements getter(0, "_cchain")

    // --- Nodes
    // pipe_foreach, pipe_reduce, block_reduce - see Template in extern
    val pipe_parallel = internal (Parallel) ("pipe_parallel", Nil, ("func", MThunk(MUnit)) :: MUnit, effect = simple)
    val unit_pipe = internal (Pipe) ("unit_pipe", T, ("func", MThunk(T)) :: T)

    // --- Pipe API
    static (Pipe) ("apply", Nil, CounterChain :: StateMachine) implements composite ${ pipeline($0, Fine) }
    static (Pipe) ("apply", Nil, varArgs(Counter) :: StateMachine) implements composite ${
      if ($0.length > 0) pipeline(CounterChain($0:_*), Fine)
      else pipeline(CounterChain(Counter(max=1)), Fine)
    }
    static (Pipe) ("apply", T, MThunk(T) :: T) implements composite ${
      val pipe = unit_pipe($0)
      styleOf(pipe) = Fine
      pipe
    }
    // Conflicts with Pipe(CounterChain)
    /*static (Pipe) ("apply", Nil, CurriedMethodSignature(List(List(CounterChain),List(Indices ==> MUnit)), MUnit)) implements composite ${
      val pipe = pipe_foreach($0, $1)
      styleOf(pipe) = Fine
    }*/
    static (Pipe) ("apply", Nil, CurriedMethodSignature(List(List(Counter),List(Idx ==> MUnit)), MUnit)) implements composite ${
      val pipe = pipe_foreach(CounterChain($0), {inds => $1(inds(0))})
      styleOf(pipe) = Fine
    }
    static (Pipe) ("apply", Nil, CurriedMethodSignature(List(List(Counter,Counter),List((Idx,Idx) ==> MUnit)), MUnit)) implements composite ${
      val pipe = pipe_foreach(CounterChain($0,$1), {inds => $2(inds(0),inds(1))})
      styleOf(pipe) = Fine
    }
    static (Pipe) ("apply", Nil, CurriedMethodSignature(List(List(Counter,Counter,Counter),List((Idx,Idx,Idx) ==> MUnit)), MUnit)) implements composite ${
      val pipe = pipe_foreach(CounterChain($0,$1,$2), {inds => $3(inds(0),inds(2),inds(3))})
      styleOf(pipe) = Fine
    }


    // --- Sequential
    static (Sequential) ("apply", Nil, CounterChain :: StateMachine) implements composite ${ pipeline($0, Disabled) }
    static (Sequential) ("apply", Nil, varArgs(Counter) :: StateMachine) implements composite ${
      if ($0.length > 0) pipeline(CounterChain($0:_*), Disabled)
      else pipeline(CounterChain(Counter(max=1)), Disabled)
    }
    static (Sequential) ("apply", T, MThunk(T) :: T) implements composite ${
      val pipe = unit_pipe($0)
      styleOf(pipe) = Disabled
      pipe
    }

    // Conflicts with Sequential(CounterChain)
    /*static (Sequential) ("apply", Nil, CurriedMethodSignature(List(List(CounterChain),List(Indices ==> MUnit)), MUnit)) implements composite ${
      val pipe = pipe_foreach($0, $1)
      styleOf(pipe) = Disabled
    }*/
    static (Sequential) ("apply", Nil, CurriedMethodSignature(List(List(Counter),List(Idx ==> MUnit)), MUnit)) implements composite ${
      val pipe = pipe_foreach(CounterChain($0), {inds => $1(inds(0))})
      styleOf(pipe) = Disabled
    }
    static (Sequential) ("apply", Nil, CurriedMethodSignature(List(List(Counter,Counter),List((Idx,Idx) ==> MUnit)), MUnit)) implements composite ${
      val pipe = pipe_foreach(CounterChain($0,$1), {inds => $2(inds(0),inds(1))})
      styleOf(pipe) = Disabled
    }
    static (Sequential) ("apply", Nil, CurriedMethodSignature(List(List(Counter,Counter,Counter),List((Idx,Idx,Idx) ==> MUnit)), MUnit)) implements composite ${
      val pipe = pipe_foreach(CounterChain($0,$1,$2), {inds => $3(inds(0),inds(2),inds(3))})
      styleOf(pipe) = Disabled
    }

    // --- Parallel
    static (Parallel) ("apply", Nil, MThunk(MUnit) :: MUnit) implements composite ${
      val pipe = pipe_parallel($0)
      styleOf(pipe) = ForkJoin
    }


    // --- StateMachine
    val StateMachine_API = withTpe(StateMachine)
    StateMachine_API {
      infix ("forIndices") ((Indices ==> MUnit) :: MUnit) implements composite ${
        val pipe = pipe_foreach($self.cchain, $1)
        styleOf(pipe) = styleOf($self)
      }
      infix ("foreach") ((Idx ==> MUnit) :: MUnit) implements composite ${ $self.forIndices{inds => $1(inds(0)) } }
      infix ("foreach") (((Idx,Idx) ==> MUnit) :: MUnit) implements composite ${ $self.forIndices{inds => $1(inds(0),inds(1)) } }
      infix ("foreach") (((Idx,Idx,Idx) ==> MUnit) :: MUnit) implements composite ${ $self.forIndices{inds => $1(inds(0),inds(1),inds(2)) } }

      // Scalar result variant
      infix ("foldIndices") (CurriedMethodSignature(List(List(C(T)),List(Indices ==> T), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        val pipe = pipe_fold[T,C]($self.cchain, $1, $2, $3)
        styleOf(pipe) = styleOf($self)
        $1
      }
      infix ("fold") (CurriedMethodSignature(List(List(C(T)),List(Idx ==> T), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.foldIndices($1){inds => $2(inds(0))}($3)
      }
      infix ("fold") (CurriedMethodSignature(List(List(C(T)),List((Idx,Idx) ==> T), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.foldIndices($1){inds => $2(inds(0),inds(1))}($3)
      }
      infix ("fold") (CurriedMethodSignature(List(List(C(T)),List((Idx,Idx,Idx) ==> T), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.foldIndices($1){inds => $2(inds(0),inds(1),inds(2))}($3)
      }

      // Collection result variant (a la BlockReduce)
      infix ("foldIndices") (CurriedMethodSignature(List(List(C(T), MInt),List(Indices ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        val ccInner = $1.iterator(List($2))
        val pipe = accum_fold[T,C]($self.cchain, ccInner, $1, $3, $4)
        styleOf(pipe) = styleOf($self)
        $1
      }
      infix ("fold") (CurriedMethodSignature(List(List(C(T)),List(Idx ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.foldIndices($1, param(1)){inds => $2(inds(0))}($3)
      }
      infix ("fold") (CurriedMethodSignature(List(List(C(T)),List((Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.foldIndices($1, param(1)){inds => $2(inds(0),inds(1))}($3)
      }
      infix ("fold") (CurriedMethodSignature(List(List(C(T)),List((Idx,Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.foldIndices($1, param(1)){inds => $2(inds(0),inds(1),inds(2))}($3)
      }

      // Fold with explicit inner parallelization factor (unused for Regs)
      infix ("fold") (CurriedMethodSignature(List(List(C(T), MInt),List(Idx ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.foldIndices($1, $2){inds => $3(inds(0))}($4)
      }
      infix ("fold") (CurriedMethodSignature(List(List(C(T), MInt),List((Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.foldIndices($1, $2){inds => $3(inds(0),inds(1))}($4)
      }
      infix ("fold") (CurriedMethodSignature(List(List(C(T), MInt),List((Idx,Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.foldIndices($1, $2){inds => $3(inds(0),inds(1),inds(2))}($4)
      }

      // Reduce (no explicit accumulator)
      /*infix ("reduceIndices") (CurriedMethodSignature(List(List(Indices ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        val ccInner = $1.iterator(Nil)
        val (accum, pipe) = accum_reduce[T,C]($self.cchain, ccInner, $1, $2)
        styleOf(pipe) = styleOf($self)
        accum
      }
      infix ("reduce") (CurriedMethodSignature(List(List(Idx ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.reduceIndices{inds => $1(inds(0))}($2)
      }
      infix ("reduce") (CurriedMethodSignature(List(List((Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.reduceIndices{inds => $1(inds(0),inds(1))}($2)
      }
      infix ("reduce") (CurriedMethodSignature(List(List((Idx,Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.reduceIndices{inds => $1(inds(0),inds(1),inds(2))}($2)
      }*/
    }

    // --- Scala Backend
    // See TemplateOpsExp for others
    impl (pipe_parallel) (codegen ($cala, ${ $b[func] }))
    impl (unit_pipe) (codegen($cala, ${ $b[func] }))

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

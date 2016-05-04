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
    val Pipeline     = lookupTpe("Pipeline")

    /* Controller style enum */
    val ControlStyle = tpe("ControlStyle", stage=compile)
    identifier (ControlStyle) ("Pipe")
    identifier (ControlStyle) ("Sequential")
    identifier (ControlStyle) ("Parallel")

    val Pipe = grp("Pipe")
    val Sequential = grp("Sequential")
    val Parallel = grp("Parallel")

    data(Pipeline, ("_cchain", CounterChain))

    internal (Pipeline) ("pipeline_new", CounterChain :: Pipeline) implements allocates(Pipeline, ${$0})
    internal (Pipeline) ("pipeline", (CounterChain, PipeStyle) :: Pipeline) implements composite ${
      val pipe = pipeline_new($0)
      styleOf(pipe) = $1
      pipe
    }
    internal.infix (Pipeline) ("cchain", Pipeline :: CounterChain) implements getter(0, "_cchain")

    // --- Nodes
    // pipe_foreach, pipe_reduce, block_reduce - see Template in extern
    val pipe_parallel = internal (Pipe) ("pipe_parallel", Nil, ("func", MThunk(MUnit)) :: MUnit, effect = simple)


    // --- API
    static (Pipe) ("apply", Nil, CounterChain :: Pipeline) implements composite ${ pipeline($0, Pipe) }
    static (Pipe) ("apply", Nil, varArgs(Counter) :: Pipeline) implements composite ${
      if ($0.length > 0) pipeline(CounterChain($0:_*), Pipe)
      else pipeline(CounterChain(Counter(max=1)), Pipe)
    }
    static (Pipe) ("apply", T, MThunk(T) :: T) implements composite ${ pipeline(CounterChain(Counter(max=1.as[Index])), Pipe) }
    static (Sequential) ("apply", Nil, CounterChain:: Pipeline) implements composite ${ pipeline($0, Sequential) }
    static (Sequential) ("apply", Nil, varArgs(Counter) :: Pipeline) implements composite ${
      if ($0.length > 0) pipeline(CounterChain($0:_*), Sequential)
      else pipeline(CounterChain(Counter(max=1)), Sequential)
    }
    static (Sequential) ("apply", T, MThunk(T) :: T) implements composite ${ pipeline(CounterChain(Counter(max=1.as[Index])), Sequential) }

    static (Parallel) ("apply", Nil, MThunk(MUnit) :: MUnit) implements composite ${
      val pipe = pipe_parallel($0)
      styleOf(pipe) = Parallel
    }

    val Pipeline_API = withTpe(Pipeline)
    Pipeline_API {
      infix ("forIndices") ((Indices ==> MUnit) :: MUnit) implements composite ${
        val pipe = pipe_foreach($self.cchain, $1)
        styleOf(pipe) = styleOf($self)
      }
      infix ("apply") ((Idx ==> MUnit) :: MUnit) implements composite ${ $self.forIndices{inds => $1(inds(0)) } }
      infix ("apply") ((Idx,Idx) ==> MUnit) :: MUnit) implements composite ${ $self.forIndices{inds => $1(inds(0),inds(1)) } }
      infix ("apply") ((Idx,Idx,Idx) ==> MUnit :: MUnit) implements composite ${ $self.forIndices{inds => $1(inds(0),inds(1),inds(2))} }

      // Scalar result variant
      infix ("foldIndices", CurriedMethodSignature(List(List(C(T)),List(Indices ==> T), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        val pipe = pipe_reduce[T,C]($self.cchain, $1, $2, $3)
        styleOf(pipe) = styleOf($self)
        $1
      }
      infix ("fold", CurriedMethodSignature(List(List(C(T)),List(Idx ==> T), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.fold($1){inds => $2(inds(0))}($3)
      }
      infix ("fold", CurriedMethodSignature(List(List(C(T)),List((Idx,Idx) ==> T), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.fold($1){inds => $2(inds(0),inds(1))}($3)
      }
      infix ("fold", CurriedMethodSignature(List(List(C(T)),List((Idx,Idx,Idx) ==> T), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.fold($1){inds => $2(inds(0),inds(1),inds(2))}($3)
      }

      // Collection result variant (a la BlockReduce)
      infix ("foldIndices", CurriedMethodSignature(List(List(C(T)),List(Indices ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        val ccInner = $1.iterator(Nil)
        val pipe = accum_reduce[T,C]($self.cchain, ccInner, $1, $2, $3)
        styleOf(pipe) = styleOf($self)
        $1
      }
      infix ("fold", CurriedMethodSignature(List(List(C(T)),List(Idx ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.fold($1){inds => $2(inds(0))}($3)
      }
      infix ("fold", CurriedMethodSignature(List(List(C(T)),List((Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.fold($1){inds => $2(inds(0),inds(1))}($3)
      }
      infix ("fold", CurriedMethodSignature(List(List(C(T)),List((Idx,Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), TMem(T,C(T)), addTpePars=(T,C)) implements composite ${
        $self.fold($1){inds => $2(inds(0),inds(1),inds(2))}($3)
      }

      // TODO: Reduce (no explicit accumulator)?
    }

    // --- Scala Backend
    // See TemplateOpsExp for others
    impl (pipe_parallel) (codegen ($cala, ${ $b[func] }))

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

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

    // --- API
    /** Creates a Counter with min of 0, given max, and step size of 1 **/
    static (Counter) ("apply", Nil, ("max", Idx) :: Counter) implements composite ${ counter_new(0.as[Index], $max, 1.as[Index], param(1)) }
    /** Creates a Counter with given min and max, and step size of 1 **/
    static (Counter) ("apply", Nil, (("min", Idx), ("max", Idx)) :: Counter) implements composite ${ counter_new($min, $max, 1.as[Index], param(1)) }
    /** Creates a Counter with given min, max and step size **/
    static (Counter) ("apply", Nil, (("min", Idx), ("max", Idx), ("step", Idx)) :: Counter) implements composite ${ counter_new($min, $max, $step, param(1)) }
    /** Creates a Counter with given min, max, step size, and parallelization factor **/
    static (Counter) ("apply", Nil, (("min", Idx), ("max",Idx), ("step",Idx), ("par",MInt)) :: Counter) implements composite ${ counter_new($min, $max, $step, $par) }

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

    val Reg  = lookupTpe("Reg")
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
    // pipe_foreach, pipe_reduce, accum_fold - see Template in extern
    val pipe_parallel = internal (Parallel) ("pipe_parallel", Nil, ("func", MThunk(MUnit)) :: MUnit, effect = simple)
    val unit_pipe = internal (Pipe) ("unit_pipe", Nil, ("func", MThunk(MUnit)) :: MUnit, effect = simple)

    val controllers = List(Pipe, Sequential)
    val styles  = List("InnerPipe", "SequentialPipe")
    val objects = List("Pipe", "Sequential")

    (controllers, styles, objects).zipped.foreach{case (ctrl, style, obj) =>
      val desc = if (style == "InnerPipe") "pipelined, parallelizable" else "sequential"
      val exec = if (style == "InnerPipe") "pipelined" else "sequential"

      /** Creates a $desc state machine which iterates through the ND domain defined by the supplied counterchain,
       * executing the specified function every iteration. If the function contains other state machines, this is executed
       * as an outer loop with each inner state machine run as a stage in a $exec fashion.
       * Note that this is the general form for N-dimensional domains. Use the specialized 1, 2, or 3D forms when possible.
       * @param cchain: counterchain determining index domain
       * @param func: the function to be executed each iteration
       **/
      static (ctrl) ("foreach", Nil, CurriedMethodSignature(List(List(CounterChain),List(Indices ==> MUnit)), MUnit)) implements composite ${
        val pipe = pipe_foreach($0, $1)
        styleOf(pipe) = \$style
      }
      /** Creates a $desc state machine which iterates through the 1D domain defined by the supplied counter, executing the specified function
       * every iteration. If the function contains other state machines, this is executed as an outer as an outer loop with each inner state machine
       * run as a stage in a $exec fashion.
       * @param c0: counter specifying 1D index domain
       * @param func: the function to be executed each iteration
       **/
      static (ctrl) ("foreach", Nil, CurriedMethodSignature(List(List(Counter),List(Idx ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0)){inds => $1(inds(0))}
      }
      /** Creates a $desc state machine which iterates through the 1D domain defined by the supplied counter, executing the specified function
       * every iteration. If the function contains other state machines, this is executed as an outer as an outer loop with each inner state machine
       * run as a stage in a $exec fashion.
       * @param c0: counter for first dimension
       * @param c1: counter for second dimension
       * @param func: the function to be executed each iteration
       **/
      static (ctrl) ("foreach", Nil, CurriedMethodSignature(List(List(Counter,Counter),List((Idx,Idx) ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0,$1)){inds => $2(inds(0),inds(1))}
      }
      /** Creates a $desc state machine which iterates through the 3D domain defined by the supplied counter, executing the specified function
       * every iteration. If the function contains other state machines, this is executed as an outer as an outer loop with each inner state machine
       * run as a stage in a $exec fashion.
       * @param c0: counter for first dimension
       * @param c1: counter for second dimension
       * @param c2: counter for third dimension
       * @param func: the function to be executed each iteration
       **/
      static (ctrl) ("foreach", Nil, CurriedMethodSignature(List(List(Counter,Counter,Counter),List((Idx,Idx,Idx) ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0,$1,$2)){inds => $3(inds(0),inds(1),inds(2))}
      }

      /** Shorthand form for foreach. Creates a $desc state machine which iterates through the ND domain defined by the supplied counterchain,
       * executing the specified function every iteration. If the function contains other state machines, this is executed
       * as an outer loop with each inner state machine run as a stage in a $exec fashion.
       *
       * Note that this is the general form for N-dimensional domains. Use the specialized 1, 2, or 3D forms when possible.
       * @param cchain: counterchain determining index domain
       * @param func: the function to be executed each iteration
       **/
      static (ctrl) ("apply", Nil, CurriedMethodSignature(List(List(CounterChain),List(Indices ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach($0)($1)
      }
      /** Shorthand form for foreach. Creates a $desc state machine which iterates through the 1D domain defined by the supplied counter, executing the specified function
       * every iteration. If the function contains other state machines, this is executed as an outer as an outer loop with each inner state machine
       * run as a stage in a $exec fashion.
       * @param c0: counter specifying 1D index domain
       * @param func: the function to be executed each iteration
       **/
      static (ctrl) ("apply", Nil, CurriedMethodSignature(List(List(Counter),List(Idx ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0)){inds => $1(inds(0))}
      }

      /** Shorthand form for foreach. Creates a $desc state machine which iterates through the 2D domain defined by the supplied counter, executing the specified function
       * every iteration. If the function contains other state machines, this is executed as an outer as an outer loop with each inner state machine
       * run as a stage in a $exec fashion.
       * @param c0: counter for first dimension
       * @param c1: counter for second dimension
       * @param func: the function to be executed each iteration
       **/
      static (ctrl) ("apply", Nil, CurriedMethodSignature(List(List(Counter,Counter),List((Idx,Idx) ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0,$1)){inds => $2(inds(0),inds(1))}
      }

      /** Shorthand form for foreach. Creates a $desc state machine which iterates through the 3D domain defined by the supplied counter, executing the specified function
       * every iteration. If the function contains other state machines, this is executed as an outer as an outer loop with each inner state machine
       * run as a stage in a $exec fashion.
       * @param c0: counter for first dimension
       * @param c1: counter for second dimension
       * @param c2: counter for third dimension
       * @param func: the function to be executed each iteration
       **/
      static (ctrl) ("apply", Nil, CurriedMethodSignature(List(List(Counter,Counter,Counter),List((Idx,Idx,Idx) ==> MUnit)), MUnit)) implements composite ${
        \$obj.foreach(CounterChain($0,$1,$2)){inds => $3(inds(0),inds(1),inds(2))}
      }

      // Scalar reduction
      // Originally wanted to have scalar and collection reduction have the same syntax, but the difference
      // between Idx => T and Idx => C[T] is ambiguous

      /** Multi-dimensional scalar fused map-reduce.
       * Creates a state machine which iterates over the given multi-dimensional domain, reducing the scalar
       * result of each iteration of the map using the supplied associative
       * reduction function. If the map function contains other state machines, this is executed
       * as an outer loop with each inner state machine run as a stage in a $exec fashion.
       * Note that this is the general form for N-dimensional domains. Use the specialized 1, 2, or 3D forms when possible.
       * @param cchain: counterchain determining index domain
       * @param accum: scalar accumulator for holding intermediate reduction values
       * @param map: scalar map function
       * @param reduce: associative reduction function
       * @return the accumulator used in this reduction (identical to *accum*)
       **/
      static (ctrl) ("reduce", (T,C), CurriedMethodSignature(List(List(CounterChain), List(C(T)), List(Indices ==> T), List((T,T) ==> T)), C(T)), (TMem(T,C(T)), TNum(T))) implements composite ${
        val pipe = pipe_fold[T,C]($0, $1, None, $2, $3)
        styleOf(pipe) = \$style
        $1
      }
      /** 1-dimensional scalar fused map-reduce.
       * Creates a state machine which iterates over the supplied 1D domain, reducing the scalar result of
       * each iteration of the map using the supplied associative reduction function. If the map function
       * contains other state machines, this is executed as an outer loop with each inner state machine
       * run as a stage in a $exec fashion.
       * @param c0: counter specifying the 1D index domain
       * @param accum: scalar accumulator for holding intermediate reduction values
       * @param map: scalar map function
       * @param reduce: associative reduction function
       * @return the accumulator used in this reduction (identical to *accum*)
       **/
      static (ctrl) ("reduce", (T,C), CurriedMethodSignature(List(List(Counter), List(C(T)), List(Idx ==> T), List((T,T) ==> T)), C(T)), (TMem(T,C(T)), TNum(T))) implements composite ${
        \$obj.reduce(CounterChain($0))($1){inds => $2(inds(0))}($3)
      }
      /** 2-dimensional scalar fused map-reduce.
       * Creates a state machine which iterates over the supplied 2D domain, reducing the scalar result of
       * each iteration of the map using the supplied associative reduction function. If the map function
       * contains other state machines, this is executed as an outer loop with each inner state machine
       * run as a stage in a $exec fashion.
       * @param c0: counter for the first dimension
       * @param c1: counter for the second dimension
       * @param accum: scalar accumulator for holding intermediate reduction values
       * @param map: scalar map function
       * @param reduce: associative reduction function
       * @return the accumulator used in this reduction (identical to *accum*)
       **/
      static (ctrl) ("reduce", (T,C), CurriedMethodSignature(List(List(Counter,Counter), List(C(T)), List((Idx,Idx) ==> T), List((T,T) ==> T)), C(T)), (TMem(T,C(T)), TNum(T))) implements composite ${
        \$obj.reduce(CounterChain($0,$1))($2){inds => $3(inds(0),inds(1))}($4)
      }
      /** 3-dimensional scalar fused map-reduce.
       * Creates a state machine which iterates over the supplied 3D domain, reducing the scalar result of
       * each iteration of the map using the supplied associative reduction function. If the map function
       * contains other state machines, this is executed as an outer loop with each inner state machine
       * run as a stage in a $exec fashion.
       * @param c0: counter for the first dimension
       * @param c1: counter for the second dimension
       * @param c2: counter for the third dimension
       * @param accum: scalar accumulator for holding intermediate reduction values
       * @param map: scalar map function
       * @param reduce: associative reduction function
       * @return the accumulator used in this reduction (identical to *accum*)
       **/
      static (ctrl) ("reduce", (T,C), CurriedMethodSignature(List(List(Counter,Counter,Counter), List(C(T)), List((Idx,Idx,Idx) ==> T), List((T,T) ==> T)), C(T)), (TMem(T,C(T)),TNum(T))) implements composite ${
        \$obj.reduce(CounterChain($0,$1,$2))($3){inds => $4(inds(0),inds(1),inds(2))}($5)
      }

      // Accumulator reduction
      /** Multi-dimensional fused map-reduce of memories.
       * Creates a state machine which iterates over the supplied multi-dimensional domain, reducing the collection resulting from
       * each iteration of the map using the supplied associative scalar reduction function. This state machine is always
       * run as an outer loop of state machines. If the memory result of the map function has multiple elements (e.g. BRAMs), the reduction is
       * run as an inner loop where the supplied associative reduction is used on each iteration.
       * Supported memory types are: Regs and BRAMs.
       * @param cchain: counterchain specifying the index domain
       * @param accum: scalar accumulator for holding intermediate reduction values
       * @param map: map function
       * @param reduce: associative reduction function
       * @return the accumulator used in this reduction (identical to *accum*)
       **/
      static (ctrl) ("fold", (T,C), CurriedMethodSignature(List(List(CounterChain, MInt),List(C(T)),List(Indices ==> C(T)), List((T,T) ==> T)), C(T)), (TMem(T,C(T)),TNum(T))) implements composite ${
        val ccInner = $2.iterator(List($1))
        val pipe = accum_fold[T,C]($0, ccInner, $2, None, $3, $4)
        styleOf(pipe) = \$style
        $2
      }
      /** 1-dimensional fused map-reduce of memories.
       * Creates a state machine which iterates over the supplied 1D domain, reducing the collection resulting from
       * each iteration of the map using the supplied associative scalar reduction function. This state machine is always
       * run as an outer loop of state machines. If the memory result of the map function has multiple elements (e.g. BRAMs), the reduction is
       * run as an inner loop where the supplied associative reduction is used on each iteration.
       * Supported memory types are: Regs and BRAMs.
       * @param c0: counter specifying the 1D index domain
       * @param accum: scalar accumulator for holding intermediate reduction values
       * @param map: map function
       * @param reduce: associative reduction function
       * @return the accumulator used in this reduction (identical to *accum*)
       **/
      static (ctrl) ("fold", (T,C), CurriedMethodSignature(List(List(Counter),List(C(T)),List(Idx ==> C(T)), List((T,T) ==> T)), C(T)), (TMem(T,C(T)), TNum(T))) implements composite ${
        \$obj.fold(CounterChain($0),param(1))($1){inds => $2(inds(0))}($3)
      }
      /** 2-dimensional fused map-reduce of memories.
       * Creates a state machine which iterates over the supplied 2D domain, reducing the collection resulting from
       * each iteration of the map using the supplied associative scalar reduction function. This state machine is always
       * run as an outer loop of state machines. If the memory result of the map function has multiple elements (e.g. BRAMs), the reduction is
       * run as an inner loop where the supplied associative reduction is used on each iteration.
       * Supported memory types are: Regs and BRAMs.
       * @param c0: counter for the first dimension
       * @param c1: counter for the second dimension
       * @param accum: scalar accumulator for holding intermediate reduction values
       * @param map: map function
       * @param reduce: associative reduction function
       * @return the accumulator used in this reduction (identical to *accum*)
       **/
      static (ctrl) ("fold", (T,C), CurriedMethodSignature(List(List(Counter,Counter),List(C(T)),List((Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), (TMem(T,C(T)),TNum(T))) implements composite ${
        \$obj.fold(CounterChain($0,$1),param(1))($2){inds => $3(inds(0),inds(1))}($4)
      }
      /** 3-dimensional fused map-reduce of memories.
       * Creates a state machine which iterates over the supplied 3D domain, reducing the collection resulting from
       * each iteration of the map using the supplied associative scalar reduction function. This state machine is always
       * run as an outer loop of state machines. If the memory result of the map function has multiple elements (e.g. BRAMs), the reduction is
       * run as an inner loop where the supplied associative reduction is used on each iteration.
       * Supported memory types are: Regs and BRAMs.
       * @param c0: counter for the first dimension
       * @param c1: counter for the second dimension
       * @param c2: counter for the third dimension
       * @param accum: scalar accumulator for holding intermediate reduction values
       * @param map: map function
       * @param reduce: associative reduction function
       * @return the accumulator used in this reduction (identical to *accum*)
       **/
      static (ctrl) ("fold", (T,C), CurriedMethodSignature(List(List(Counter,Counter,Counter),List(C(T)),List((Idx,Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), (TMem(T,C(T)),TNum(T))) implements composite ${
        \$obj.fold(CounterChain($0,$1,$2),param(1))($3){inds => $4(inds(0),inds(1),inds(2))}($5)
      }

      // Fold with explicit inner parallelization factor (unused for Regs)
      /** @nodoc -- syntax TBD **/
      static (ctrl) ("fold", (T,C), CurriedMethodSignature(List(List(Counter,MInt),List(C(T)),List(Idx ==> C(T)), List((T,T) ==> T)), C(T)), (TMem(T,C(T)),TNum(T))) implements composite ${
        \$obj.fold(CounterChain($0),$1)($2){inds => $3(inds(0))}($4)
      }
      /** @nodoc -- syntax TBD **/
      static (ctrl) ("fold", (T,C), CurriedMethodSignature(List(List(Counter,Counter,MInt),List(C(T)),List((Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), (TMem(T,C(T)),TNum(T))) implements composite ${
        \$obj.fold(CounterChain($0,$1),$2)($3){inds => $4(inds(0),inds(1))}($5)
      }
      /** @nodoc -- syntax TBD **/
      static (ctrl) ("fold", (T,C), CurriedMethodSignature(List(List(Counter,Counter,Counter,MInt),List(C(T)),List((Idx,Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), (TMem(T,C(T)),TNum(T))) implements composite ${
        \$obj.fold(CounterChain($0,$1,$2),$3)($4){inds => $5(inds(0),inds(1),inds(2))}($6)
      }

      // Unit Pipe
      /** Creates a "unit" pipeline. Used as a wrapper node around simple logic in the body.
       * @param body
       **/
      static (ctrl) ("apply", Nil, MThunk(MUnit) :: MUnit) implements composite ${
        val pipe = unit_pipe($0)
        styleOf(pipe) = \$style
      }
    }

    // --- Parallel
    /** Creates a parallel fork-join controller. Synchronizes all state machines in the body.
     * @param body
     **/
    static (Parallel) ("apply", Nil, MThunk(MUnit) :: MUnit) implements composite ${
      val pipe = pipe_parallel($0)
      styleOf(pipe) = ForkJoin
    }

    // --- Reduction
    // Reduce(N by B)(0){i => f(i)}{_+_}

    direct (Pipe) ("Reduce", T, CurriedMethodSignature(List(List(CounterChain), List(T), List(Indices ==> T), List((T,T) ==> T)), Reg(T)), TNum(T)) implements composite ${
      val accumulator = Reg[T]
      val pipe = pipe_fold($0, accumulator, Some($1), $2, $3)
      styleOf(pipe) = InnerPipe
      accumulator
    }
    direct (Pipe) ("Reduce", T, CurriedMethodSignature(List(List(Counter), List(T), List(Idx ==> T), List((T,T) ==> T)), Reg(T)), TNum(T)) implements composite ${
      Reduce(CounterChain($0))($1){inds => $2(inds(0))}($3)
    }
    direct (Pipe) ("Reduce", T, CurriedMethodSignature(List(List(Counter, Counter), List(T), List((Idx,Idx) ==> T), List((T,T) ==> T)), Reg(T)), TNum(T)) implements composite ${
      Reduce(CounterChain($0, $1))($2){inds => $3(inds(0),inds(1))}($4)
    }
    direct (Pipe) ("Reduce", T, CurriedMethodSignature(List(List(Counter, Counter, Counter), List(T), List((Idx,Idx,Idx) ==> T), List((T,T) ==> T)), Reg(T)), TNum(T)) implements composite ${
      Reduce(CounterChain($0, $1, $2))($3){inds => $4(inds(0),inds(1),inds(2))}($5)
    }

    // Fold(N by B)(accum, 0){i => f(i)}{_+_}

    direct (Pipe) ("Fold", (T,C), CurriedMethodSignature(List(List(CounterChain, MInt),List(C(T),T),List(Indices ==> C(T)), List((T,T) ==> T)), C(T)), (TMem(T,C(T)),TNum(T))) implements composite ${
      val ccInner = $2.iterator(List($1))
      val pipe = accum_fold[T,C]($0, ccInner, $2, Some($3), $4, $5)
      styleOf(pipe) = InnerPipe
      $2
    }
    direct (Pipe) ("Fold", (T,C), CurriedMethodSignature(List(List(Counter),List(C(T),T),List(Idx ==> C(T)), List((T,T) ==> T)), C(T)), (TMem(T,C(T)),TNum(T))) implements composite ${
      Fold(CounterChain($0), param(1))($1, $2){inds => $3(inds(0))}($4)
    }
    direct (Pipe) ("Fold", (T,C), CurriedMethodSignature(List(List(Counter,Counter),List(C(T),T),List((Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), (TMem(T,C(T)),TNum(T))) implements composite ${
      Fold(CounterChain($0,$1), param(1))($2, $3){inds => $4(inds(0),inds(1))}($5)
    }
    direct (Pipe) ("Fold", (T,C), CurriedMethodSignature(List(List(Counter,Counter,Counter),List(C(T),T),List((Idx,Idx,Idx) ==> C(T)), List((T,T) ==> T)), C(T)), (TMem(T,C(T)),TNum(T))) implements composite ${
      Fold(CounterChain($0,$1,$2), param(1))($3, $4){inds => $5(inds(0),inds(1),inds(2))}($6)
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

    // --- MaxJ Backend
    //pipe_parallel (extern)

    //unit_pipe (extern)
	}

}

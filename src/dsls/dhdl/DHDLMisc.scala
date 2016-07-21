package ppl.dsl.forge
package dsls
package dhdl

@dsl
trait DHDLMisc {
  this: DHDLDSL =>

  def importDHDLMisc() {
    importDHDLHelpers()
    importDHDLTestingOps()
  }

  def importDHDLHelpers() {
    /**
     * @name Miscellaneous
     *
     **/
    val Misc = grp("DHDLMisc")

    val T = tpePar("T")
    val CT = tpePar("T", stage=compile)

    val Idx = lookupAlias("Index")

    /** Creates a design parameter with the given name and default value **/
    direct (Misc) ("param", T, (("name",SString),("default",CT)) :: T) implements composite ${
      val p = param($1)
      nameOf(p) = $0
      p
    }

    // --- Staging time warnings and errors
    // TODO: These aren't DSL specific, move elsewhere
    internal (Misc) ("stageWarn", Nil, SAny :: SUnit, effect = simple) implements composite ${
      val ctx = topContext(__pos)
      System.out.println("[\u001B[33mwarn\u001B[0m] " + ctx.fileName + ":" + ctx.line + ": " + $0)
    }
    internal (Misc) ("stageError", Nil, SAny :: SNothing, effect = simple) implements composite ${
      val ctx = topContext(__pos)
      System.out.println("[\u001B[31merror\u001B[0m] " + ctx.fileName + ":" + ctx.line + ": " + $0)
      sys.exit(-1)
    }

    internal (Misc) ("stageInfo", Nil, SAny :: SUnit, effect = simple) implements composite ${
      val ctx = topContext(__pos)
      System.out.println("[\u001B[34minfo\u001B[0m] " + ctx.fileName + ":" + ctx.line + ": " + $0)
    }

    // --- Powers of 2 checks
    internal (Misc) ("isPow2", Nil, SInt :: SBoolean) implements composite ${ ($0 & ($0 - 1)) == 0 }
    internal (Misc) ("isPow2", Nil, SLong :: SBoolean) implements composite ${ ($0 & ($0 - 1)) == 0 }


    // --- Multi-dimensional addressing
    internal (Misc) ("constDimsToStrides", Nil, ("dims", SList(SInt)) :: SList(SInt)) implements composite ${
      List.tabulate($dims.length){d => $dims.drop(d + 1).fold(1){_*_} }
    }

    internal (Misc) ("dimsToStrides", Nil, ("dims", SList(Idx)) :: SList(Idx)) implements composite ${
      List.tabulate($dims.length){d =>
        if (d == $dims.length - 1) 1.as[SInt]
        else productTree( $dims.drop(d + 1) )
      }
    }
    internal (Misc) ("calcAddress", Nil, (("indices", SList(Idx)), ("dims", SList(Idx))) :: Idx) implements composite ${
      if ($indices.length == 1) indices.apply(0)
      else {
        if ($indices.length != $dims.length)
          stageWarn("Trying to address far " + $dims.length + "D memory using " + $indices.length + "D addressing")

        val strides = dimsToStrides($1)
        sumTree( List.tabulate($indices.length){i => $indices(i) * strides(i) } )
      }
    }
  }

  // Basic Array API (change out with OptiML / etc. later)
  // This is a bit strange - we don't generally lift Int to Rep[Int], but Array API requires Rep[Int].
  // For now, getting around this by defining all Array API on fix point values and converting internally
  def importArrayAPI() {
    val T = tpePar("T")
    val S = tpePar("S")
    val R = tpePar("R")

    val Idx  = lookupAlias("Index")
    val Bit  = lookupTpe("Bit")
    val Tup2 = lookupTpe("Tup2")

    val Coll = lookupTpeClass("Coll").get
    val ArrColl = tpeClassInst("ArrayColl", T, Coll(MArray(T)))
    infix (ArrColl) ("empty", T, Nil :: MArray(T)) implements composite ${ array_empty_imm[T](unit(0)) }
    //infix (ArrColl) ("zeros", T, MArray(T) :: MArray(T)) implements composite ${ array_empty_imm[T](array_length($0)) }  // TODO: Should be recursive?

    /** Unsynthesizable helper object for creating arrays on the CPU **/
    val Arr = grp("Array")
    /** Creates an empty array with given length
     * @param length
     **/
    static (Arr) ("empty", T, Idx :: MArray(T)) implements composite ${ array_empty[T](fix_to_int($0)) }

    /**
     * Creates an array with given length whose elements are determined by the supplied function
     * @param length
     * @param f
     **/
    static (Arr) ("fill", T, CurriedMethodSignature(List(List(Idx), List(MThunk(T))), MArray(T))) implements composite ${
      array_fromfunction(fix_to_int($0), {i: Rep[Int] => $1})
    }
    /** Creates an array with the given length whose elements are determined by the supplied indexed function
     * @param length
     * @param f
     **/
    static (Arr) ("tabulate", T, CurriedMethodSignature(List(List(Idx),List(Idx ==> T)), MArray(T))) implements composite ${
      array_fromfunction(fix_to_int($0), {i: Rep[Int] => $1(int_to_fix[Signed,B32](i)) })
    }
    /** Creates an array directly with apply function
     **/
    static (Arr) ("apply", T, varArgs(T) :: MArray(T)) implements composite ${
      array_fromseq( $0 )
    }


    val API = grp("ForgeArrayAPI") // ForgeArrayOps already exists...
    /** Returns the length of this Array **/
    infix (API) ("length", T, MArray(T) :: Idx) implements composite ${ int_to_fix[Signed,B32](array_length($0)) }
    /** Returns the element at the given index
     * @param i
     **/
    infix (API) ("apply", T, (MArray(T), Idx) :: T) implements composite ${ array_apply($0, fix_to_int($1)) }
    /** Updates the array at the given index
     * @param i
     * @param x
     **/
    infix (API) ("update", T, (MArray(T), Idx, T) :: MUnit, effect = write(0)) implements composite ${ array_update($0, fix_to_int($1), $2) }

    infix (API) ("map", (T,R), (MArray(T), T ==> R) :: MArray(R)) implements composite ${ array_map($0, $1) }
    infix (API) ("zip", (T,S,R), CurriedMethodSignature(List(List(MArray(T),MArray(S)), List( (T,S) ==> R)), MArray(R))) implements composite ${
      array_zip($0, $1, $2)
    }
    infix (API) ("reduce", T, (MArray(T), (T,T) ==> T) :: T, TColl(T)) implements composite ${ array_reduce($0, $1, implicitly[Coll[T]].empty) }
    infix (API) ("flatten", T, (MArray(MArray(T))) :: MArray(T)) implements composite ${ array_flatmap($0, {e: Rep[ForgeArray[T]] => e}) }
    infix (API) ("mkString", T, (MArray(T), MString) :: MString) implements composite ${ array_mkstring($0, $1) }

    infix (API) ("zipWithIndex", T, MArray(T) :: MArray(Tup2(T, Idx))) implements composite ${
      Array.tabulate($0.length){i => pack(($0(i), i)) }
    }

    direct (API) ("__equal", T, (MArray(T), MArray(T)) :: Bit, TOrder(T)) implements composite ${
      array_zip($0, $1, {(a:Rep[T], b:Rep[T]) => implicitly[Order[T]].equals(a,b)}).reduce{_&&_}
    }

  }

  def importRandomOps() {
    /** Unsynthesizable group of operations for generating random data for testing **/
    val Rand = grp("Rand")

    val A = tpePar("A")

    val Bit   = lookupTpe("Bit")
    val FixPt = lookupTpe("FixPt")
    val FltPt = lookupTpe("FltPt")

    // The "effect" here is a change to the pseudorandom generator (lifts out of loops otherwise)
    val rand_fix_bnd = internal (Rand) ("rand_fix_bnd", (S,I,F), FixPt(S,I,F) :: FixPt(S,I,F), effect = simple)
    val rand_fix = internal (Rand) ("rand_fix", (S,I,F), Nil :: FixPt(S,I,F), effect = simple)
    val rand_flt = internal (Rand) ("rand_flt", (G,E), Nil :: FltPt(G,E), effect = simple)
    val rand_bit = internal (Rand) ("rand_bit", Nil, Nil :: Bit, effect = simple)

    internal (Rand) ("randomFixPt", (A,S,I,F), A :: FixPt(S,I,F)) implements composite ${
      val fix = $0.asInstanceOf[Rep[FixPt[S,I,F]]]
      rand_fix_bnd[S,I,F](fix)
    }
    internal (Rand) ("randomFltPt", (A,G,E), A :: FltPt(G,E)) implements composite ${
      val flt = $0.asInstanceOf[Rep[FltPt[G,E]]]
      rand_flt[G,E] * flt
    }

    /** Returns a uniformly distributed random value of type A with the given maximum value **/
    direct (Rand) ("random", A, A :: A) implements composite ${
      manifest[A] match {
        case mA if isFixPtType(mA) =>
          randomFixPt($0)(manifest[A], mA.typeArguments(0), mA.typeArguments(1), mA.typeArguments(2), implicitly[SourceContext]).asInstanceOf[Rep[A]]

        case mA if isFltPtType(mA) =>
          randomFltPt($0)(manifest[A], mA.typeArguments(0), mA.typeArguments(1), implicitly[SourceContext]).asInstanceOf[Rep[A]]

        case mA => stageError("No random implementation for type " + mA.runtimeClass.getSimpleName)
      }
    }
    UnstagedNumerics.foreach{ (T, _) =>
      direct (Rand) ("random", A, T :: A) implements composite ${ random[A]($0.as[A]) }
    }

    // Holy hackery, batman!
    /** Returns a uniformly distributed random value of type A. Fixed point types are unbounded, while floating point types are between 0 and 1 **/
    direct (Rand) ("random", A, Nil :: A) implements composite ${
      manifest[A] match {
        case mA if isFixPtType(mA) =>
          rand_fix()(mA.typeArguments(0), mA.typeArguments(1), mA.typeArguments(2), implicitly[SourceContext]).asInstanceOf[Rep[A]]

        case mA if isFltPtType(mA) =>
          rand_flt()(mA.typeArguments(0), mA.typeArguments(1), implicitly[SourceContext]).asInstanceOf[Rep[A]]

        case mA if isBitType(mA) =>
          rand_bit.asInstanceOf[Rep[A]]

        case mA => stageError("No random implementation for type " + mA.runtimeClass.getSimpleName)
      }
    }

    // --- Scala Backend
    impl (rand_fix_bnd) (codegen($cala, ${ FixedPoint.randbnd[$t[S],$t[I],$t[F]]($0) }))
    impl (rand_fix) (codegen($cala, ${ FixedPoint.rand[$t[S],$t[I],$t[F]] }))
    impl (rand_flt) (codegen($cala, ${ FloatPoint.rand[$t[G],$t[E]] }))
    impl (rand_bit) (codegen($cala, ${ java.util.concurrent.ThreadLocalRandom.current().nextBoolean() }))

    // --- C++ Backend
    impl (rand_fix_bnd) (codegen(cpp, ${
      @ val remapTp = remap($0.tp)
      ($remapTp) rand() % $0
    }))
    impl (rand_fix) (codegen(cpp, ${
      @ val remapTp = remap(sym.tp)
      ($remapTp) rand()
    }))
    impl (rand_flt) (codegen(cpp, ${
      @ val remapTp = remap(sym.tp)
      ($remapTp) rand() / ($remapTp) RAND_MAX
    }))
    impl (rand_bit) (codegen(cpp, ${
      (bool) rand() % 2
    }))


    // --- MaxJ Backend
    //impl (rand_fix_bnd) (codegen(maxj, ${  }))
    //impl (rand_fix) (codegen(maxj, ${  }))
    impl (rand_flt) (codegen(maxj, ${
      DFEVar $sym = Rand_flt
    }))
    //impl (rand_bit) (codegen(maxj, ${  }))
  }


  // --- Unsynthesizable operations used for data transfer and/or testing
  def importDHDLTestingOps() {
    importArrayAPI()
    importRandomOps()
    importIOOps()

    val Tst = grp("Nosynth")

    val T = tpePar("T")

    val Bit = lookupTpe("Bit")
    val Idx = lookupAlias("Index")

    val LoopRange = lookupTpe("LoopRange")
    val OffChip   = lookupTpe("OffChipMem")
    val Reg       = lookupTpe("Reg")
    val BRAM      = lookupTpe("BRAM")

    // --- Nodes
    val set_mem = internal (Tst) ("set_mem", T, (OffChip(T), MArray(T)) :: MUnit, effect = write(0), aliasHint = aliases(Nil))
    val get_mem = internal (Tst) ("get_mem", T, (OffChip(T), MArray(T)) :: MUnit, effect = write(1), aliasHint = aliases(Nil))
    val set_arg = internal (Tst) ("set_arg", T, (Reg(T), T) :: MUnit, effect = write(0))
    val get_arg = internal (Tst) ("get_arg", T, Reg(T) :: T, effect = simple)
    val hwblock = internal (Tst) ("hwblock", Nil, MThunk(MUnit,cold) :: MUnit, effect = simple)

    val ifThenElse = direct (Tst) ("__ifThenElse", List(T), List(MBoolean,MThunk(T,cold),MThunk(T,cold)) :: T)
    val whileDo = direct (Tst) ("__whileDo", Nil, List(MThunk(MBoolean),MThunk(MUnit)) :: MUnit)

    val forLoop = internal (Tst) ("forloop", Nil, (("start", Idx), ("end", Idx), ("step", Idx), ("func", Idx ==> MUnit)) :: MUnit)

    // --- API
    val print = direct (Tst) ("print", Nil, MAny :: MUnit, effect = simple)
    val println  = direct (Tst) ("println", Nil, MAny :: MUnit, effect = simple)
    val println2 = direct (Tst) ("println", Nil, Nil :: MUnit, effect = simple)
    val assert   = direct (Tst) ("assert", Nil, Bit :: MUnit, effect = simple)
    /** Set content of BRAM to an array (debugging purpose only)
     * @param bram
     * @param array
     **/
    val set_bram = direct (Tst) ("setBram", T, (BRAM(T), MArray(T)) :: MUnit, effect = write(0), aliasHint = aliases(Nil))
    /** Get content of BRAM in an array format (debugging purpose only)
     * @param bram
     **/
    val get_bram = direct (Tst) ("getBram", T, BRAM(T) :: MArray(T), effect = simple, aliasHint = aliases(Nil))

    /** Print content of a BRAM (debugging purpose only)
     * @param bram
     **/
    direct (Tst) ("printBram", T, BRAM(T) :: MUnit, effect = simple) implements composite ${
      println(nameOf($0) + ": "+ getBram($0).mkString(","))
    }
    /** Print content of a OffChip (debugging purpose only)
     * @param bram
     **/
    direct (Tst) ("printMem", T, OffChip(T) :: MUnit, effect = simple) implements composite ${
      println(nameOf($0) + ": "+ getMem($0).mkString(","))
    }

    // Allows for(i <- x until y by z) construction
    infix (Tst) ("foreach", Nil, (LoopRange, Idx ==> MUnit) :: MUnit) implements composite ${ forloop($0.start, $0.end, $0.step, $1) }


    // --- Memory transfers
    direct (Tst) ("setMem", T, (OffChip(T), MArray(T)) :: MUnit, effect = write(0)) implements composite ${ set_mem($0, $1) }
    direct (Tst) ("getMem", T, OffChip(T) :: MArray(T)) implements composite ${
      val arr = Array.empty[T](sizeOf($0))
      get_mem($0, arr)
      arr // could call unsafeImmutable here if desired
    }
    direct (Tst) ("setArg", T, (Reg(T), T) :: MUnit, effect = write(0)) implements composite ${
      if (regType($0) != ArgumentIn) stageError("Can only set value of ArgIn registers")
      set_arg($0, $1)
    }
    UnstagedNumerics.foreach{(ST,_) =>
      direct (Tst) ("setArg", T, (Reg(T), ST) :: MUnit, effect = write(0)) implements composite ${ setArg($0, $1.as[T]) }
    }

    direct (Tst) ("getArg", T, Reg(T) :: T) implements composite ${
      if (regType($0) != ArgumentOut) stageError("Can only get value of ArgOut registers")
      get_arg($0)
    }

    // TODO: Naming isn't final. Your favorite keyword here :)
    // TODO: This is a quick hack for scheduling acceleration initialization. Eventually this should act as a true annotation
    direct (Tst) ("Accel", Nil, MThunk(MUnit) :: MUnit) implements composite ${
      val accel = hwblock($0)
      styleOf(accel) = SequentialPipe
      accel
    }



    // --- Scala Backend
    impl (print)  (codegen($cala, ${ print($0) }))
    impl (println)  (codegen($cala, ${ println($0) }))
    impl (println2) (codegen($cala, ${ println() }))
    impl (assert)   (codegen($cala, ${ assert($0) }))
    impl (ifThenElse) (codegen($cala, ${
      if ($0) {
        $b[1]
      }
      else {
        $b[2]
      }
    }))
    impl (whileDo) (codegen($cala, ${
      while ($b[0]) {
        $b[1]
      }
    }))
    impl (forLoop) (codegen($cala, ${
      var i = $start
      while (i < $end) {
        $b[func](i)
        i += $step
      }
    }))
    impl (set_bram) (codegen($cala, ${ System.arraycopy($1, 0, $0, 0, $1.length) }))
    impl (get_bram) (codegen($cala, ${ val arr = Array.tabulate($0.length) {i => $0(i)}; arr }))

    impl (set_mem)  (codegen($cala, ${ System.arraycopy($1, 0, $0, 0, $1.length) }))
    impl (get_mem)  (codegen($cala, ${ System.arraycopy($0, 0, $1, 0, $0.length) }))
    impl (set_arg)  (codegen($cala, ${ $0.update(0, $1) }))
    impl (get_arg)  (codegen($cala, ${ $0.apply(0) }))
    impl (hwblock)  (codegen($cala, ${
      $b[0]
      ()
    }))

    // C++ backend
    impl (set_mem)  (codegen(cpp, ${ memcpy($1, 0, $0, 0, $1.length) }))
    impl (get_mem)  (codegen(cpp, ${ memcpy($0, 0, $1, 0, $0.length) }))
    impl (set_arg)  (codegen(cpp, ${ *$0 = $1}))
    impl (get_arg)  (codegen(cpp, ${ *$0 }))
    impl (hwblock)  (codegen(cpp, ${
      std::cout << "hwblock" << std::endl
    }))
    impl (println)  (codegen(cpp, ${ std::cout << $0 << std::endl }))
    impl (println2) (codegen(cpp, ${ std::cout << std::endl }))
    impl (assert)   (codegen(cpp, ${ assert($0) }))
    impl (ifThenElse) (codegen(cpp, ${
      if ($0) {
        $b[1]
      }
      else {
        $b[2]
      }
    }))
    impl (whileDo) (codegen(cpp, ${
      while ($b[0]) {
        $b[1]
      }
    }))
    impl (forLoop) (codegen(cpp, ${
      @ val itp = remap($start.tp)
      $itp i = $start ;
      for (i = $start ; i < $end ; i += $step) {
        $b[func](i)
      }
    }))

    // --- MaxJ Backend
    impl (println)  (codegen(maxj, ${
			@ emitComment("println")
		}))
		//TODO: maxj shouldn't see these nodes if nodes inside hwblock aren't ramdomly moved outside
    impl (set_mem)  (codegen(maxj, ${ }))
    impl (get_mem)  (codegen(maxj, ${ }))
    impl (set_arg)  (codegen(maxj, ${ }))
    impl (get_arg)  (codegen(maxj, ${ }))
    impl (hwblock)  (codegen(maxj, ${
			@ inHwScope = true
      @ emitBlock(__arg0)
			@ inHwScope = false
    }))

    // --- Rewrites
    rewrite (ifThenElse) using forwarding ${ delite_ifThenElse($0, $1, $2, false, true) }
    rewrite (whileDo) using forwarding ${ delite_while($0, $1) }
  }

  def importIOOps() {
    val IO = grp("GraphIO")
    val T = tpePar("T")
    val K = tpePar("K")
    val V = tpePar("V")
    val OffChip   = lookupTpe("OffChipMem")
    val Idx = lookupAlias("Index")
    val SArray = tpe("scala.Array", T)
    val SHashMap = tpe("scala.collection.mutable.HashMap", (K,V))
    val SListBuffer = tpe("scala.collection.mutable.ListBuffer", T)

    /**
     * Returns Array(smap,dmap), where smap is the adjacency list in hashMap for source nodes and dmap is
     * the adjacency list of the destination nodes
     * @param path full path to graph file
     * @param numVert number of vertices
     * @param dot whether is loading from a dot graph
     **/
    direct (IO) ("loadDirEdgeList", Nil, (MString, Idx, SBoolean) :: MArray(SHashMap(SInt,SListBuffer(SInt))), effect = simple) implements composite ${
      load_dir_edge_list($0, Some(fix_to_int($1)), Some($2))
    }

    /**
     * Returns Array(smap,dmap), where smap is the adjacency list in hashMap for source nodes and dmap is
     * the adjacency list of the destination nodes
     * @param path full path to graph file
     * @param dot whether is loading from a dot graph
     **/
    direct (IO) ("loadDirEdgeList", Nil, (MString,SBoolean) :: MArray(SHashMap(SInt,SListBuffer(SInt))), effect = simple) implements composite ${
      load_dir_edge_list($0, None, Some($1))
    }

    /**
     * Returns Array(smap,dmap), where smap is the adjacency list in hashMap for source nodes and dmap is
     * the adjacency list of the destination nodes
     * @param path full path to graph file
     * @param numVert number of vertices
     **/
    direct (IO) ("loadDirEdgeList", Nil, (MString, Idx) :: MArray(SHashMap(SInt,SListBuffer(SInt))), effect = simple) implements composite ${
      load_dir_edge_list($0, Some(fix_to_int($1)), None)
    }

    /**
     * Returns Array(smap,dmap), where smap is the adjacency list in hashMap for source nodes and dmap is
     * the adjacency list of the destination nodes
     * @param path full path to graph file
     **/
    direct (IO) ("loadDirEdgeList", Nil, (MString) :: MArray(SHashMap(SInt,SListBuffer(SInt))), effect = simple) implements composite ${
      load_dir_edge_list($0, None, None)
    }

    direct (IO) ("load_dir_edge_list", Nil, (MString, SOption(MInt), SOption(SBoolean)) :: MArray(SHashMap(SInt,SListBuffer(SInt))), effect = simple) implements codegen ($cala, ${
      val path = $0
      @ val qnumVert = $1.getOrElse(-1)
      val numVert = $qnumVert
      @ val qdot = $2.getOrElse(false)
      val dot = $qdot

      import scala.collection.mutable.{HashMap, ListBuffer}
      import scala.io.Source
      val smap = new HashMap[Int, ListBuffer[Int]]
      val dmap = new HashMap[Int, ListBuffer[Int]]
      val edgeStr =  if (dot) "->" else " "
      def parseLine(line:String):Unit = {
        val fields = line.split(edgeStr)
        val s = fields(0).trim.toInt
        val d = fields(1).trim.toInt
        if (!smap.contains(s))
          smap += (s -> ListBuffer(d))
        else
          smap.get(s).get += d
        if (!dmap.contains(d))
          dmap += (d -> ListBuffer(s))
        else
          dmap.get(d).get += s
      }
      for (line <- Source.fromFile(path).getLines()) {
        if (dot) {
          if (line.contains(edgeStr))
            parseLine(line)
        } else {
          parseLine(line)
        }
      }

      if (smap.keys.size < numVert) {
        for (v <- 0 until numVert) {
          if (!smap.contains(v))
            smap += (v -> ListBuffer())
        }
      }

      if (dmap.keys.size < numVert) {
        for (v <- 0 until numVert) {
          if (!dmap.contains(v))
            dmap += (v -> ListBuffer())
        }
      }

      Array(smap, dmap)
    })

    /**
     * Returns an adjacency list in HashMap
     * @param path full path to graph file
     * @param numVert number of vertices
     * @param dot whether is loading from a dot graph
     **/
    direct (IO) ("loadUnDirEdgeList", Nil, (MString, Idx, SBoolean) :: SHashMap(SInt,SListBuffer(SInt)), effect = simple) implements composite ${
      load_undir_edge_list($0, Some(fix_to_int($1)), Some($2))
    }

    /**
     * Returns an adjacency list in HashMap
     * @param path full path to graph file
     * @param dot whether is loading from a dot graph
     **/
    direct (IO) ("loadUnDirEdgeList", Nil, (MString,SBoolean) :: SHashMap(SInt,SListBuffer(SInt)), effect = simple) implements composite ${
      load_undir_edge_list($0, None, Some($1))
    }

    /**
     * Returns an adjacency list in HashMap
     * @param path full path to graph file
     * @param numVert number of vertices
     **/
    direct (IO) ("loadUnDirEdgeList", Nil, (MString, Idx) :: SHashMap(SInt,SListBuffer(SInt)), effect = simple) implements composite ${
      load_undir_edge_list($0, Some(fix_to_int($1)), None)
    }

    /**
     * Returns an adjacency list in HashMap
     * @param path full path to graph file
     **/
    direct (IO) ("loadUnDirEdgeList", Nil, (MString) :: SHashMap(SInt,SListBuffer(SInt)), effect = simple) implements composite ${
      load_undir_edge_list($0, None, None)
    }

    direct (IO) ("load_undir_edge_list", Nil, (MString, SOption(MInt), SOption(SBoolean)) :: SHashMap(SInt,SListBuffer(SInt)), effect = simple) implements codegen ($cala, ${
      val path = $0
      @ val qnumVert = $1.getOrElse(-1)
      val numVert = $qnumVert
      @ val qdot = $2.getOrElse(false)
      val dot = $qdot

      import scala.collection.mutable.{HashMap, ListBuffer}
      import scala.io.Source
      val map = new HashMap[Int, ListBuffer[Int]]
      val edgeStr =  if (dot) "--" else " "
      def parseLine(line:String):Unit = {
        val fields = line.split(edgeStr)
        val n1 = fields(0).trim.toInt
        val n2 = fields(1).trim.toInt
        if (!map.contains(n1))
          map += (n1 -> ListBuffer(n2))
        else
          map.get(n1).get += n2
        if (!map.contains(n2))
          map += (n2 -> ListBuffer(n1))
        else
          map.get(n2).get += n1
      }
      for (line <- Source.fromFile(path).getLines()) {
        if (dot) {
          if (line.contains(edgeStr))
            parseLine(line)
        } else {
          parseLine(line)
        }
      }

      if (map.keys.size < numVert) {
        for (v <- 0 until numVert) {
          if (!map.contains(v))
            map += (v -> ListBuffer())
        }
      }
      map
    })

    /** Generating an array of array representing vertices and their pointer to the edge list based on
     *  the adjacency list represented in the map.
     *  If explicitVert=true, withSize=true, returns an N by 3 Array of Array(vertex #, ptr, size)
     *  If explicitVert=true, withSize=false, returns an N by 2 Array of Array(vertex #, ptr)
     *  If explicitVert=false, withSize=true, returns an N by 2 Array of Array(ptr, size)
     *  If explicitVert=false, withSize=false, returns an N by 1 Array of Array(ptr)
     *  size is the number of edges for correponding vertex.
     * @param map
     * @param explicitVert
     * @param withSize
     **/
    direct (IO) ("getVertList", Nil, (SHashMap(SInt, SListBuffer(SInt)), SBoolean, SBoolean) :: MArray(Idx), effect = simple) implements codegen ($cala, ${
      val map = $0
      val explicitVert = $1
      val withSize = $2
      val vertices = {
        val temp = (map.values.flatMap(i=>i).toSet ++ map.keys.toSet).toArray
        scala.util.Sorting.quickSort(temp)
        temp
      }
      val numVert = vertices.length

      // Size of neighbors
      val sizes = vertices.map {n => map.get(n).getOrElse(Nil).size}

      // Pointers to neighbors in edge list
      val pts = Array.fill(numVert)(0)
      (0 until numVert).fold(0) {case (p, i) => pts(i) = if (sizes(i)==0) 0 else p; p+sizes(i)}

      def fixpt(i:Int) = FixedPoint[Signed,B32,B0](i)

      val vertList =
        if (explicitVert) {
          if (withSize) {
            vertices.zipWithIndex.flatMap {case (n,i) => Array(fixpt(n), fixpt(pts(i)), fixpt(sizes(i)))}
          } else {
            vertices.zipWithIndex.flatMap {case (n,i) => Array(fixpt(n), fixpt(pts(i)))}
          }
        } else {
          if (withSize) {
            vertices.zipWithIndex.flatMap {case (n,i) => Array(fixpt(pts(i)), fixpt(sizes(i)))}
          } else {
            vertices.zipWithIndex.flatMap {case (n,i) => Array(fixpt(pts(i)))}
          }
        }
      vertList
    })

    direct (IO) ("getEdgeList", Nil, SHashMap(SInt, SListBuffer(SInt)) :: MArray(Idx), effect = simple) implements codegen ($cala, ${
      val map = $0
      import scala.util.Sorting
      val vertices = {
        val temp = (map.values.flatMap(i=>i).toSet ++ map.keys.toSet).toArray
        Sorting.quickSort(temp)
        temp
      }
      // Flat edge list
      val edgeList = vertices.flatMap{n =>
        val edges = map.get(n).getOrElse(Nil).toArray
        Sorting.quickSort(edges)
        edges
      }
      edgeList.map(i => FixedPoint[Signed,B32,B0](i))
    })

    direct (IO) ("genRandDirEdgeList", Nil, (MString, Idx, Idx, SBoolean) :: MUnit, effect = simple) implements codegen ($cala, ${
      val path = $0
      val numVert = $1.toInt
      val numEdge = $2.toInt
      val dot = $3

      import java.io.File
      import java.io.PrintWriter
      def randInt(max:Int) = scala.util.Random.nextInt( max )
      val edgeStr =  if (dot) "->" else " "
      def printEdge(pw:PrintWriter) = {
        val src = randInt( numVert )
        var dst = randInt( numVert )
        while ( src==dst ) {
          dst = randInt( numVert )
        }
        pw.println(src + edgeStr + dst)
      }
      val numLine = if (dot) (numEdge + 2) else numEdge

      val pw = new PrintWriter(new File(path))
      for (i <- 0 until numLine) {
        if (dot) {
          if (i==0)
            pw.println("digraph {")
          else if (i==(numLine - 1))
            pw.println("}")
          else
            printEdge(pw)
        } else {
          printEdge(pw)
        }
      }
      pw.close()
    })

  }
}

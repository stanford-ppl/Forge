package ppl.dsl.forge
package dsls
package dhdl

trait DHDLMisc {
  this: DHDLDSL =>

  def importDHDLMisc() {
    importDHDLHelpers()
    importDHDLTestingOps()
  }

  def importDHDLHelpers() {
    val Misc = grp("DHDLMisc")

    val T = tpePar("T")

    val Idx = lookupAlias("Index")

    // --- Staging time warnings and errors
    internal (Misc) ("stageWarn", Nil, SAny :: SUnit, effect = simple) implements composite ${
      System.out.println("[\u001B[33mwarn\u001B[0m] " + __pos.fileName + ":" + __pos.line + ": " + $0)
    }
    internal (Misc) ("stageError", Nil, SAny :: SNothing, effect = simple) implements composite ${
      System.out.println("[\u001B[31merror\u001B[0m] " + __pos.fileName + ":" + __pos.line + ": " + $0)
      sys.exit(-1)
    }

    // --- Powers of 2 checks
    internal (Misc) ("isPow2", Nil, SInt :: SBoolean) implements composite ${ ($0 & ($0 - 1)) == 0 }
    internal (Misc) ("isPow2", Nil, SLong :: SBoolean) implements composite ${ ($0 & ($0 - 1)) == 0 }


    // --- Multi-dimensional addressing
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

    // Scala's fold and reduce don't produce a binary tree - use these functions instead
    internal (Misc) ("reductionTree", T, (SList(T), ((T,T) ==> T)) :: SList(T)) implements composite ${
      if ($0.length == 1) $0
      else if ($0.length % 2 == 0) reductionTree( List.tabulate($0.length/2){i => $1( $0(2*i), $0(2*i+1)) }, $1)
      else reductionTree( List.tabulate($0.length/2){i => $1( $0(2*i), $0(2*i+1)) } :+ $0.last, $1)
    }
    internal (Misc) ("productTree", T, SList(T) :: T, TArith(T)) implements composite ${
      reductionTree[T]($0, {(a,b) => implicitly[Arith[T]].mul(a,b) }).head
    }
    internal (Misc) ("sumTree", T, SList(T) :: T, TArith(T)) implements composite ${
      reductionTree[T]($0, {(a,b) => implicitly[Arith[T]].add(a,b) }).head
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
    infix (ArrColl) ("zeros", T, MArray(T) :: MArray(T)) implements composite ${ array_empty_imm[T](array_length($0)) }  // TODO: Should be recursive?

    val Arr = grp("Array")
    static (Arr) ("empty", T, Idx :: MArray(T)) implements composite ${ array_empty[T](fix_to_int($0)) }

    static (Arr) ("fill", T, CurriedMethodSignature(List(List(Idx), List(MThunk(T))), MArray(T))) implements composite ${
      array_fromfunction(fix_to_int($0), {i: Rep[Int] => $1})
    }
    static (Arr) ("tabulate", T, CurriedMethodSignature(List(List(Idx),List(Idx ==> T)), MArray(T))) implements composite ${
      array_fromfunction(fix_to_int($0), {i: Rep[Int] => $1(int_to_fix[Signed,B32](i)) })
    }


    val API = grp("ForgeArrayAPI") // ForgeArrayOps already exists...
    infix (API) ("length", T, MArray(T) :: Idx) implements composite ${ int_to_fix[Signed,B32](array_length($0)) }
    infix (API) ("apply", T, (MArray(T), Idx) :: T) implements composite ${ array_apply($0, fix_to_int($1)) }
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
      array_zip($0, $1, {(a:Rep[T], b:Rep[T]) => implicitly[Order[T]].eql(a,b)}).reduce{_&&_}
    }
  }

  def importRandomOps() {
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

    // --- Dot Backend
    //impl (rand_fix_bnd) (codegen(dot, ${  }))
    //impl (rand_fix) (codegen(dot, ${  }))
    //impl (rand_flt) (codegen(dot, ${  }))
    //impl (rand_bit) (codegen(dot, ${  }))

    // --- MaxJ Backend
    //impl (rand_fix_bnd) (codegen(maxj, ${  }))
    //impl (rand_fix) (codegen(maxj, ${  }))
    //impl (rand_flt) (codegen(maxj, ${  }))
    //impl (rand_bit) (codegen(maxj, ${  }))
  }


  // --- Unsynthesizable operations used for data transfer and/or testing
  def importDHDLTestingOps() {
    importArrayAPI()
    importRandomOps()

    val Tst = grp("Nosynth")

    val T = tpePar("T")

    val Bit = lookupTpe("Bit")
    val Idx = lookupAlias("Index")

    val LoopRange = lookupTpe("LoopRange")
    val OffChip   = lookupTpe("OffChipMem")
    val Reg       = lookupTpe("Reg")

    // --- Nodes
    val set_mem = internal (Tst) ("set_mem", T, (OffChip(T), MArray(T)) :: MUnit, effect = write(0), aliasHint = aliases(Nil))
    val get_mem = internal (Tst) ("get_mem", T, (OffChip(T), MArray(T)) :: MUnit, effect = write(1), aliasHint = aliases(Nil))
    val set_arg = internal (Tst) ("set_arg", T, (Reg(T), T) :: MUnit, effect = write(0))
    val get_arg = internal (Tst) ("get_arg", T, Reg(T) :: T, effect = simple)
    val hwblock = internal (Tst) ("hwblock", Nil, MThunk(MUnit) :: MUnit, effect = simple)

    val ifThenElse = direct (Tst) ("__ifThenElse", List(T), List(MBoolean,MThunk(T,cold),MThunk(T,cold)) :: T)
    val whileDo = direct (Tst) ("__whileDo", Nil, List(MThunk(MBoolean),MThunk(MUnit)) :: MUnit)

    val forLoop = internal (Tst) ("forloop", Nil, (("start", Idx), ("end", Idx), ("step", Idx), ("func", Idx ==> MUnit)) :: MUnit)

    // --- API
    val println  = direct (Tst) ("println", Nil, MAny :: MUnit, effect = simple)
    val println2 = direct (Tst) ("println", Nil, Nil :: MUnit, effect = simple)
    val assert   = direct (Tst) ("assert", Nil, Bit :: MUnit, effect = simple)

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
    direct (Tst) ("Accel", Nil, MThunk(MUnit) :: MUnit) implements composite ${ hwblock($0) }



    // --- Scala Backend
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

    impl (set_mem)  (codegen($cala, ${ System.arraycopy($1, 0, $0, 0, $1.length) }))
    impl (get_mem)  (codegen($cala, ${ System.arraycopy($0, 0, $1, 0, $0.length) }))
    impl (set_arg)  (codegen($cala, ${ $0.update(0, $1) }))
    impl (get_arg)  (codegen($cala, ${ $0.apply(0) }))
    impl (hwblock)  (codegen($cala, ${
      $b[0]
      ()
    }))

    // --- Dot Backend
    impl (println)  (codegen(dot, ${
			@ emitComment("println")
		}))
		//TODO: dot shouldn't see these nodes if nodes inside hwblock aren't ramdomly moved
		//outside
    impl (set_mem)  (codegen(dot, ${ }))
    impl (get_mem)  (codegen(dot, ${ }))
		impl (set_arg)  (codegen(dot, ${ $1 -> $0 }))
    impl (get_arg)  (codegen(dot, ${ }))
    impl (hwblock)  (codegen(dot, ${
			@ inHwScope = true
      @ stream.println(emitBlock(__arg0) + "")
			@ inHwScope = false
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
      @ stream.println(emitBlock(__arg0) + "")
			@ inHwScope = false
    }))

    // --- Rewrites
    rewrite (ifThenElse) using forwarding ${ delite_ifThenElse($0, $1, $2, false, true) }
    rewrite (whileDo) using forwarding ${ delite_while($0, $1) }
  }
}

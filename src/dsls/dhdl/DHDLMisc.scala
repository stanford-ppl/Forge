package ppl.dsl.forge
package dsls
package dhdl

trait DHDLMisc {
  this: DHDLDSL =>

  def importDHDLMisc() {
    importDHDLHelper()
    importDHDLTestingOps()
  }

	def importDHDLHelper() {
    val Misc = grp("DHDLMisc")

    val T = tpePar("T")
    val Fix = lookupTpe("Fix")

    // Multi-dimensional addressing
    internal (Misc) ("calcAddress", Nil, (("indices", SList(Fix)), ("dims", SList(SInt))) :: Fix) implements composite ${
      if ($indices.length == 1) indices.apply(0)  // Flat indexing is always allowed
      else {
        if ($indices.length != $dims.length) {
          stageWarn("Trying to address " + $dims.length + "D memory using " + $indices.length + "D addressing")
        }
        val strides = List.tabulate($dims.length){d =>
          if (d == $dims.length - 1) 1
          else $dims.drop(d + 1).reduce(_*_)
        }
        List.tabulate($indices.length){i => strides(i).toFixPt * $indices(i) }.reduce{_+_}
      }
    }

    // Scala's fold and reduce don't produce a binary tree - use these functions instead
    internal (Misc) ("reductionTree", T, (SList(T), ((T,T) ==> T)) :: SList(T)) implements composite ${
      if ($0.length == 1) $0
      else if ($0.length % 2 == 0) reductionTree( List.tabulate($0.length/2){i => $1( $0(2*i), $0(2*i+1)) }, $1)
      else reductionTree( List.tabulate($0.length/2){i => $1( $0(2*i), $0(2*i+1)) } :+ $0.last, $1)
    }
    internal (Misc) ("productTree", T, SList(T) :: T, TArith(T)) implements composite ${
      reductionTree[T]($0, {(a,b) => a * b}).head
    }
    internal (Misc) ("sumTree", T, SList(T) :: T, TArith(T)) implements composite ${
      reductionTree[T]($0, {(a,b) => a + b}).head
    }
  }

  // Basic Array API (change out with OptiML / etc. later)
  // This is a bit strange - we don't generally lift Int to Rep[Int], but Array API requires Rep[Int].
  // For now, getting around this by defining all Array API on fix point values and converting internally
  def importArrayAPI() {
    val T = tpePar("T")
    val S = tpePar("S")
    val R = tpePar("R")
    val Fix = lookupTpe("Fix")
    val Bit = lookupTpe("Bit")
    val Coll = lookupGrp("Coll").asInstanceOf[Rep[DSLTypeClass]]

    val ArrColl = tpeClassInst("ArrayColl", T, Coll(MArray(T)))
    infix (ArrColl) ("empty", T, Nil :: MArray(T)) implements composite ${ array_empty_imm[T](unit(0)) }
    infix (ArrColl) ("zeros", T, MArray(T) :: MArray(T)) implements composite ${ array_empty_imm[T]($0.length) }  // TODO: Should be recursive?

    val Arr = grp("Array")
    static (Arr) ("empty", T, Fix :: MArray(T)) implements composite ${ array_empty[T](fix_to_int($0)) }

    static (Arr) ("fill", T, CurriedMethodSignature(List(List(Fix), List(MThunk(T))), MArray(T))) implements composite ${
      array_fromfunction(fix_to_int($0), {i: Rep[Int] => $1})
    }
    static (Arr) ("tabulate", T, CurriedMethodSignature(List(List(Fix),List(Fix ==> T)), MArray(T))) implements composite ${
      array_fromfunction(fix_to_int($0), {i: Rep[Int] => $1(int_to_fix(i)) })
    }


    val API = grp("ForgeArrayAPI") // ForgeArrayOps already exists...
    infix (API) ("length", T, MArray(T) :: MInt) implements composite ${ array_length($0) }
    infix (API) ("apply", T, (MArray(T), Fix) :: T) implements composite ${ array_apply($0, fix_to_int($1)) }
    infix (API) ("update", T, (MArray(T), Fix, T) :: MUnit, effect = write(0)) implements composite ${ array_update($0, fix_to_int($1), $2) }

    infix (API) ("map", (T,R), (MArray(T), T ==> R) :: MArray(R)) implements composite ${ array_map($0, $1) }
    infix (API) ("zip", (T,S,R), CurriedMethodSignature(List(List(MArray(T),MArray(S)), List( (T,S) ==> R)), MArray(R))) implements composite ${
      array_zip($0, $1, $2)
    }
    infix (API) ("reduce", T, (MArray(T), (T,T) ==> T) :: T, TColl(T)) implements composite ${ array_reduce($0, $1, implicitly[Coll[T]].empty) }
    infix (API) ("flatten", T, (MArray(MArray(T))) :: MArray(T)) implements composite ${ array_flatmap($0, {e: Rep[ForgeArray[T]] => e}) }
    infix (API) ("mkString", T, (MArray(T), MString) :: MString) implements composite ${ array_mkstring($0, $1) }

    direct (API) ("__equal", T, (MArray(T), MArray(T)) :: Bit, TOrder(T)) implements composite ${
      $0.zip($1){(a,b) => implicitly[Order[T]].eql(a,b) }.reduce{_&&_}
    }
  }

  def importRandomOps() {
    val Rand = grp("Rand")
    val A = tpePar("A")
    val Bit = lookupTpe("Bit")
    val Fix = lookupTpe("Fix")
    val Flt = lookupTpe("Flt")

    // The "effect" here is a change to the pseudorandom generator (lifts out of loops otherwise)
    val rand_fix_bnd = internal (Rand) ("rand_fix_bnd", Nil, Fix :: Fix, effect = simple)
    val rand_fix = internal (Rand) ("rand_fix", Nil, Nil :: Fix, effect = simple)
    val rand_flt = internal (Rand) ("rand_flt", Nil, Nil :: Flt, effect = simple)
    val rand_bit = internal (Rand) ("rand_bit", Nil, Nil :: Bit, effect = simple)

    direct (Rand) ("randomFix", Nil, Fix :: Fix) implements composite ${ rand_fix_bnd($0) }

    direct (Rand) ("random", A, Nil :: A) implements composite ${
      manifest[A] match {
        case mA if mA == manifest[Fix] => rand_fix.asInstanceOf[Rep[A]]
        case mA if mA == manifest[Flt] => rand_flt.asInstanceOf[Rep[A]]
        case mA if mA == manifest[Bit] => rand_bit.asInstanceOf[Rep[A]]
        case mA => stageError("No random implementation for type " + mA.toString)
      }
    }

    // --- Scala backend
    // TODO: Assumes maximum value is an integer (not a Long)
    impl (rand_fix_bnd) (codegen($cala, ${ java.util.concurrent.ThreadLocalRandom.current().nextInt($0.toInt).toLong }))
    impl (rand_fix) (codegen($cala, ${ java.util.concurrent.ThreadLocalRandom.current().nextInt().toLong }))
    impl (rand_flt) (codegen($cala, ${ java.util.concurrent.ThreadLocalRandom.current().nextDouble() }))
    impl (rand_bit) (codegen($cala, ${ java.util.concurrent.ThreadLocalRandom.current().nextBoolean() }))
  }


  // --- Unsynthesizable operations used for data transfer and/or testing
  // TODO: getMem should probably take care of allocation too (rather than just copy)
  def importDHDLTestingOps() {
    importArrayAPI()
    importRandomOps()

    val Tst = grp("Testing")
    val Bit = lookupTpe("Bit")
    val Fix = lookupTpe("Fix")
    val Flt = lookupTpe("Flt")
    val LoopRange = lookupTpe("LoopRange")
    val OffChip = lookupTpe("OffChipMem")
    val Reg = lookupTpe("Reg")
    val T = tpePar("T")

    // Staging time warnings and errors
    internal (Tst) ("stageWarn", Nil, SAny :: SUnit, effect = simple) implements composite ${
      System.out.println("[\u001B[33mwarn\u001B[0m] " + $0)
    }
    internal (Tst) ("stageError", Nil, SAny :: SNothing, effect = simple) implements composite ${
      System.out.println("[\u001B[31merror\u001B[0m] " + $0)
      sys.exit(-1)
    }


    // --- Nodes
    val set_mem = internal (Tst) ("set_mem", T, (OffChip(T), MArray(T)) :: MUnit, effect = write(0), aliasHint = aliases(Nil))
    val get_mem = internal (Tst) ("get_mem", T, (OffChip(T), MArray(T)) :: MUnit, effect = write(1), aliasHint = aliases(Nil))
    val set_arg = internal (Tst) ("set_arg", T, (Reg(T), T) :: MUnit, effect = write(0))
    val get_arg = internal (Tst) ("get_arg", T, Reg(T) :: T)
    val hwblock = internal (Tst) ("hwblock", T, MThunk(T) :: MUnit, effect = simple)

    val fix_to_int = internal (Tst) ("fix_to_int", Nil, Fix :: MInt)
    val int_to_fix = internal (Tst) ("int_to_fix", Nil, MInt :: Fix)
    val bit_to_bool = internal (Tst) ("bit_to_bool", Nil, Bit :: MBoolean)
    //val fix_to_dbl = internal (Tst) ("fix_to_dbl", Nil, Fix :: MDouble)
    //val dbl_to_fix = internal (Tst) ("dbl_to_fix", Nil, MDouble :: Fix)
    //val flt_to_dbl = internal (Tst) ("flt_to_dbl", Nil, Flt :: MDouble)
    //val dbl_to_flt = internal (Tst) ("dbl_to_flt", Nil, MDouble :: Flt)
    val ifThenElse = direct (Tst) ("__ifThenElse", List(T), List(MBoolean,MThunk(T,cold),MThunk(T,cold)) :: T)
    val whileDo = direct (Tst) ("__whileDo", Nil, List(MThunk(MBoolean),MThunk(MUnit)) :: MUnit)

    val forLoop = internal (Tst) ("forloop", Nil, (("start", Fix), ("end", Fix), ("step", Fix), ("func", Fix ==> MUnit)) :: MUnit)
    infix (Tst) ("foreach", Nil, (LoopRange, Fix ==> MUnit) :: MUnit) implements composite ${ forloop($0.start, $0.end, $0.step, $1) }

    // --- API
    val println  = direct (Tst) ("println", Nil, MAny :: MUnit, effect = simple)
    val println2 = direct (Tst) ("println", Nil, Nil :: MUnit, effect = simple)
    val assert   = direct (Tst) ("assert", Nil, Bit :: MUnit, effect = simple)

    direct (Tst) ("setMem", T, (OffChip(T), MArray(T)) :: MUnit, effect = write(0)) implements composite ${ set_mem($0, $1) }
    direct (Tst) ("getMem", T, OffChip(T) :: MArray(T)) implements composite ${
      val arr = Array.empty[T](productTree(symSizeOf($0)))
      get_mem($0, arr)
      arr // could call unsafeImmutable here if desired
    }

    direct (Tst) ("setArg", T, (Reg(T), T) :: MUnit, effect = write(0)) implements composite ${
      if (regtpe($0) != ArgumentIn) stageError("Can only set value of ArgIn registers")
      set_arg($0, $1)
    }
    direct (Tst) ("getArg", T, Reg(T) :: T) implements composite ${
      if (regtpe($0) != ArgumentOut) stageError("Can only get value of ArgOut registers")
      get_arg($0)
    }

    // TODO: Should this always return Unit? (since need to facilitate communication in some other way)
    // TODO: Naming isn't final. Your favorite keyword here :)
    // TODO: This is a quick hack for scheduling acceleration initialization. Eventually this should act as a true annotation
    direct (Tst) ("Accel", T, MThunk(T) :: MUnit) implements composite ${ hwblock($0) }


    // Needed for ifThenElse (since default requires Rep[Boolean] and overloading is tough in this case)
    fimplicit (Tst) ("bitToBoolean", Nil, Bit :: MBoolean) implements composite ${ bit_to_bool($0) }

    // --- Scala backend
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

    impl (fix_to_int) (codegen($cala, ${ $0.toInt }))
    impl (int_to_fix) (codegen($cala, ${ $0.toLong }))
    impl (bit_to_bool) (codegen($cala, ${ $0 }))

    // --- Rewrites
    rewrite (ifThenElse) using forwarding ${ delite_ifThenElse($0, $1, $2, false, true) }
    rewrite (whileDo) using forwarding ${ delite_while($0, $1) }
	}
}

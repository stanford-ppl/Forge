package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait DenseVectorOps {
  this: OptiLADSL =>

  def importDenseVectorOps() {
    val T = tpePar("T")
    val R = tpePar("R")
    val B = tpePar("B")

    val SArray = lookupTpe("scala.Array")
    val DenseVector = lookupTpe("DenseVector") // tpe("DenseVector", T)
    val IndexVector = lookupTpe("IndexVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val SparseVector = lookupTpe("SparseVector")

    // data fields
    data(DenseVector, ("_length", MInt), ("_isRow", MBoolean), ("_data", MArray(T)))

    // operations on literal sequences are made available via tuple conversions to DenseVector
    // These are temporarily commented due to a bug in Scala 2.11: https://issues.scala-lang.org/browse/SI-8992
    /*
    for (arity <- (2 until 23)) {
      // we use "Reppable" to allow heterogeneous tuples (that vary in A, Rep[A], and Var[A]) to still be converted
      val pars = tpePar("A") :: (1 until arity).map(i => tpePar(('A'.toInt+i).toChar.toString, Nil)).toList
      val impls = (1 until arity).map(i => TReppable(pars(i),pars(0))).toList

      // we need a version where A (the return type) is A, Rep[A], and Var[A] in order to get type inference to "always" work
      // using a different type parameter to specify the return type (like RR) almost works, but isn't always inferred
      val elems = ((2 to arity).map(i => implicitOpArgPrefix + (i-2) + ".view(t._"+i+")").toList).mkString(",")
      val TT = tpe("Tuple" + arity, pars, stage = compile)
      fimplicit (DenseVector) ("tupleToDense" + arity, pars, (("t",TT) :: DenseVector(pars(0))), impls) implements redirect ${ DenseVector[A](unit(t._1),\$elems) }

      // we hack the first argument to be Rep[A] or Var[A] as needed here.
      val TR = tpe("Tuple" + arity, tpePar("Rep[A]") :: pars.drop(1), stage = compile)
      fimplicit (DenseVector) ("repTupleToDense" + arity, pars, (("t",TR) :: DenseVector(pars(0))), impls) implements redirect ${ DenseVector[A](t._1,\$elems) }

      val TV = tpe("Tuple" + arity, tpePar("Var[A]") :: pars.drop(1), stage = compile)
      fimplicit (DenseVector) ("varTupleToDense" + arity, pars, (("t",TV) :: DenseVector(pars(0))), impls) implements redirect ${ DenseVector[A](readVar(t._1),\$elems) }
    }
    */

    // static methods
    static (DenseVector) ("apply", T, (MInt, MBoolean) :: DenseVector(T), effect = mutable) implements allocates(DenseVector, ${$0}, ${$1}, ${array_empty[T]($0)})
    static (DenseVector) ("apply", T, varArgs(T) :: DenseVector(T)) implements allocates(DenseVector, ${unit($0.length)}, ${unit(true)}, ${array_fromseq[T]($0)})
    static (DenseVector) ("apply", T, MethodSignature(List(MArray(T), ("isRow", MBoolean, "unit(true)")), DenseVector(T))) implements redirect ${ densevector_fromarray($0, isRow) }

    // helper
    direct (DenseVector) ("densevector_fromarray", T, (MArray(T), MBoolean) :: DenseVector(T)) implements allocates(DenseVector, ${array_length($0)}, ${$1}, ${$0})
    direct (DenseVector) ("densevector_fromfunc", T, (MInt, MBoolean, MInt ==> T) :: DenseVector(T)) implements composite ${
      IndexVector(0,$0,$1) map { i => $2(i) }
    }
    compiler (DenseVector) ("densevector_alloc_raw", T, (MInt, MBoolean, MArray(T)) :: DenseVector(T)) implements allocates(DenseVector, ${$0}, ${$1}, ${$2})
    static (DenseVector) ("zeros", Nil, MInt :: DenseVector(MDouble)) implements composite ${ (0::$0) { i => 0.0 } }
    static (DenseVector) ("zerosf", Nil, MInt :: DenseVector(MFloat)) implements composite ${ (0::$0) { i => 0f } }
    static (DenseVector) ("ones", Nil, MInt :: DenseVector(MDouble)) implements composite ${ (0::$0) { i => 1.0 } }
    static (DenseVector) ("onesf", Nil, MInt :: DenseVector(MFloat)) implements composite ${ (0::$0) { i => 1f } }
    static (DenseVector) ("rand", Nil, MInt :: DenseVector(MDouble)) implements composite ${ (0::$0) { i => random[Double] } }
    static (DenseVector) ("randf", Nil, MInt :: DenseVector(MFloat)) implements composite ${ (0::$0) { i => random[Float] } }
    static (DenseVector) ("uniform", Nil, MethodSignature(List(("start", MDouble), ("step_size", MDouble), ("end", MDouble), ("isRow", MBoolean, "unit(true)")), DenseVector(MDouble))) implements composite ${
      fassert(end > start+step_size, "end <= start+step_size in DenseVector.uniform")
      val length = ceil(($end-$start)/$step_size)
      (0::length) { i => $step_size*i + $start }
    }

    static (DenseVector) ("flatten", T, ("pieces",DenseVector(DenseVector(T))) :: DenseVector(T)) implements composite ${
      if ($pieces.length == 0){
        DenseVector[T](0, $pieces.isRow).unsafeImmutable
      }
      else {
        val sizes = $pieces map { e => e.length }
        val (total,begins) = unpack(densevector_precumulate[Int](sizes, 0, (_: Rep[Int]) + (_: Rep[Int])))
        val result = DenseVector[T](total, $pieces.isRow)
        for (i <- 0 until $pieces.length) {
          result.copyFrom(begins(i), $pieces(i))
        }
        result.unsafeImmutable
      }
    }

    // only composite ops can return non-lifted tuples (or anything else). using CTuple2 should work, but there is a problem with iFThenElse that I don't fully understand yet.
    // val Tuple2 = CTuple2
    val Tuple2 = lookupTpe("Tup2")
    compiler (DenseVector) ("densevector_precumulate", T, ((("v",DenseVector(T)), ("identity",T), ("func",(T,T) ==> T)) :: Tuple2(T,DenseVector(T)))) implements composite ${
      if ($v.length == 0) {
        pack(($identity,DenseVector[T](0,$v.isRow).unsafeImmutable))
      }
      else {
        val result = DenseVector[T](0, $v.isRow)
        var accum = $identity
        for (i <- 0 until $v.length) {
          result <<= accum
          accum = $func(accum, $v(i))
        }
        pack((accum,result.unsafeImmutable))
      }
    }

    // a non-type-safe way of passing the metadata required to allocate a DenseVector in a parallel op
    // ideally we would encode this is as a type class, but it's not clear we would get an instance of this type class in dc_alloc
    val CR = tpePar("CR")
    compiler (DenseVector) ("densevector_dc_alloc", (R,CR), (CR,MInt) :: DenseVector(R)) implements composite ${
      val simpleName = manifest[CR].erasure.getSimpleName
      val isRow = simpleName match {
        case s if s.startsWith("IndexVector") => indexvector_isrow($0.asInstanceOf[Rep[IndexVector]])
        case s if s.startsWith("DenseVectorView") => densevectorview_isrow($0.asInstanceOf[Rep[DenseVectorView[Any]]])
        case s if s.startsWith("DenseVector") => densevector_isrow($0.asInstanceOf[Rep[DenseVector[Any]]])
      }
      DenseVector[R]($1, isRow)
    }

    compiler (DenseVector) ("densevector_sortindex_helper", T, (MInt,MInt,MArray(T)) :: SArray(MInt), TOrdering(T)) implements codegen($cala, ${
      // this is a hack for int64 mode, where ints get remapped to longs. By adding / subtracting $1,
      // the return type of promoteInt64 should be Long when int64 is on, and Int otherwise.
      // (Note if $1 is a constant, this trick doesn't work. More often though, $0 is a constant.)
      def promoteInt64(x: Int) = $1 + x - $1
      ($0.toInt until $1.toInt: scala.Range).toArray.map(i => promoteInt64(i)).sortWith((a,b) => $2(a) < $2(b))
    })

    val K = tpePar("K")
    val V = tpePar("V")

    compiler (DenseVector) ("densevector_groupby_helper", (T,K,V), (DenseVector(T), T ==> K, T ==> V) :: MHashMap(K, MArrayBuffer(V))) implements groupBy((T,K,V), 0, ${e => $1(e)}, ${e => $2(e)})

    infix (DenseVector) ("toVector", (T,R), MHashMap(T,R) :: DenseVector(R)) implements composite ${
      densevector_fromarray(fhashmap_values($0), true)
    }


    val DenseVectorOps = withTpe (DenseVector)
    DenseVectorOps {
      /**
       * Accessors
       */
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("isRow") (Nil :: MBoolean) implements getter(0, "_isRow")
      infix ("apply") (MInt :: T) implements composite ${ array_apply(densevector_raw_data($self), $1) }
      infix ("apply") (IndexVector :: DenseVector(T)) implements composite ${
        val out = $1.map(i => $self(i))
        if ($self.isRow != $1.isRow) out.t else out // preserve orientation of original vector
      }
      infix ("slice") ((("start",MInt),("end",MInt)) :: DenseVector(T)) implements redirect ${ $self(start::end) }

      /**
       * Miscellaneous
       */
      infix ("t") (Nil :: DenseVector(T)) implements allocates(DenseVector, ${densevector_length($0)}, ${!(densevector_isrow($0))}, ${array_soft_clone(densevector_raw_data($0))})
      infix ("mt") (Nil :: MUnit, effect = write(0)) implements composite ${
        densevector_set_isrow($0, !$0.isRow)
      }

      infix ("toMat") (Nil :: DenseMatrix(T)) implements composite ${
        if ($self.isRow) {
          densematrix_fromarray(array_soft_clone(densevector_raw_data($self)), 1, $self.length)
        }
        else {
          densematrix_fromarray(array_soft_clone(densevector_raw_data($self)), $self.length, 1)
        }
      }

      // No-op; just available so that we can call it generically on vectors.
      infix ("toDense") (Nil :: DenseVector(T)) implements redirect ${ $self }

      infix ("Clone") (Nil :: DenseVector(T), aliasHint = copies(0)) implements composite ${
        densevector_alloc_raw($self.length, $self.isRow, array_clone(densevector_raw_data($self)))
      }

      /**
       * Data operations
       */
      compiler ("densevector_raw_data") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("densevector_set_raw_data") (MArray(T) :: MUnit, effect = write(0)) implements setter(0, "_data", ${$1})
      compiler ("densevector_set_length") (MInt :: MUnit, effect = write(0)) implements setter(0, "_length", ${$1})
      compiler ("densevector_set_isrow") (MBoolean :: MUnit, effect = write(0)) implements setter(0, "_isRow", ${$1})

      infix ("update") ((("i",MInt),("e",T)) :: MUnit, effect = write(0)) implements composite ${
        array_update(densevector_raw_data($self), $i, $e)
      }

      infix ("update") ((("indices",IndexVector),("e",T)) :: MUnit, effect = write(0)) implements composite ${
        (0::indices.length) foreach { i =>
          fassert(indices(i) >= 0 && indices(i) < $self.length, "index out of bounds: bulk vector update")
          array_update(densevector_raw_data($self), indices(i), e)
        }
      }

      infix ("update") ((("indices",IndexVector),("v",DenseVector(T))) :: MUnit, effect = write(0)) implements single ${
        fassert(indices.length == v.length, "dimension mismatch: bulk vector update")

        // cannot be parallel unless indices contains only disjoint indices (why is why we use 'single' here)
        // however, maybe this should be a property that we guarantee of all IndexVectors
        (0::indices.length) foreach { i =>
          fassert(indices(i) >= 0 && indices(i) < $self.length, "index out of bounds: bulk vector update")
          array_update(densevector_raw_data($self), indices(i), v(i))
        }
      }

      infix ("<<") (T :: DenseVector(T)) implements composite ${
        val out = $self.mutable
        out <<= $1
        out.unsafeImmutable
      }
      infix("<<") (DenseVector(T) :: DenseVector(T)) implements composite ${
        val out = DenseVector[T]($self.length+$1.length, $self.isRow)
        for (i <- 0 until $self.length){
          out(i) = $self(i)
        }
        for (i <- 0 until $1.length){
          out(i+$self.length) = $1(i)
        }
        out.unsafeImmutable
      }

      // workaround for type inference failing in DenseVectorSuite line 159
      noInfixList :::= List("<<=","<<|=")

      infix ("<<=") (T :: MUnit, effect = write(0)) implements composite ${ $self.insert($self.length,$1) }
      infix ("<<=") (DenseVector(T) :: MUnit, effect = write(0)) implements composite ${ $self.insertAll($self.length,$1) }

      infix ("insert") ((MInt,T) :: MUnit, effect = write(0)) implements single ${
        densevector_insertspace($self,$1,1)
        $self($1) = $2
      }
      infix ("insertAll") ((MInt,DenseVector(T)) :: MUnit, effect = write(0)) implements single ${
        densevector_insertspace($self, $1, $2.length)
        $self.copyFrom($1, $2)
      }
      infix ("remove") (MInt :: MUnit, effect = write(0)) implements composite ${ $self.removeAll($1, 1) }
      infix ("removeAll") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements single ${
        val data = densevector_raw_data($self)
        array_copy(data, $pos + $len, data, $pos, $self.length - ($pos + $len))
        densevector_set_length($self, $self.length - $len)
      }

      infix ("copyFrom") ((MInt,DenseVector(T)) :: MUnit, effect = write(0)) implements single ${
        val d = densevector_raw_data($self)
        for (i <- 0 until $2.length) {
          array_update(d,$1+i,$2(i))
        }
      }
      infix ("trim") (Nil :: MUnit, effect = write(0)) implements single ${
        val data = densevector_raw_data($self)
        if ($self.length < array_length(data)) {
          val d = array_empty[T]($self.length)
          array_copy(data, 0, d, 0, $self.length)
          densevector_set_raw_data($self, d.unsafeImmutable)
        }
      }
      infix ("clear") (Nil :: MUnit, effect = write(0)) implements single ${
        densevector_set_length($self, 0)
        densevector_set_raw_data($self, (array_empty[T](0)).unsafeImmutable)
      }

      compiler ("densevector_insertspace") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements single ${
        densevector_ensureextra($self,$len)
        val data = densevector_raw_data($self)
        array_copy(data,$pos,data,$pos+$len,$self.length-$pos)
        densevector_set_length($self,$self.length+$len)
      }
      compiler ("densevector_ensureextra") (("extra",MInt) :: MUnit, effect = write(0)) implements single ${
        val data = densevector_raw_data($self)
        if (array_length(data) - $self.length < $extra) {
          densevector_realloc($self, $self.length+$extra)
        }
      }
      compiler ("densevector_realloc") (("minLen",MInt) :: MUnit, effect = write(0)) implements single ${
        val data = densevector_raw_data($self)
        var n = max(4, array_length(data)*2)
        while (n < $minLen) n = n*2
        val d = array_empty[T](n)
        array_copy(data, 0, d, 0, $self.length)
        densevector_set_raw_data($self, d.unsafeImmutable)
      }


      /**
       * Math
       */
      infix ("+=") (DenseVector(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) + $1(i) }
      }
      infix ("+=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) + $1 }
      }
      // so that we can add without converting to Dense
      infix ("+=") (DenseVectorView(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) + $1(i) }
      }

      infix ("*=") (DenseVector(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) * $1(i) }
      }
      infix ("*=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) * $1 }
      }
      infix ("*=") (DenseVectorView(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) * $1(i) }
      }

      infix ("-=") (DenseVector(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) - $1(i) }
      }
      infix ("-=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) - $1 }
      }
      infix ("-=") (DenseVectorView(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) - $1(i) }
      }

      infix ("/=") (DenseVector(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) / $1(i) }
      }
      infix ("/=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) / $1 }
      }
      infix ("/=") (DenseVectorView(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) / $1(i) }
      }

      /**
       * Ordering
       */
      infix ("sort") (Nil :: DenseVector(T), TOrdering(T)) implements composite ${
        val v2 = $self.mutable
        v2.trim()
        val a = array_sort(densevector_raw_data(v2))
        densevector_fromarray(a, $self.isRow)
      }

      infix ("sortBy") ((T ==> B) :: DenseVector(T), TOrdering(B), addTpePars = B) implements composite ${
        val sortedIndicesRaw = farray_from_sarray(densevector_sortindex_helper(0, $self.length, densevector_raw_data($self.map($1))))
        val sortedIndices = IndexVector(densevector_fromarray(sortedIndicesRaw,$self.isRow))
        $self(sortedIndices)
      }

      infix ("sortWithIndex") (Nil :: CTuple2(DenseVector(T),IndexVector), TOrdering(T)) implements composite ${
        val sortedIndicesRaw = farray_from_sarray(densevector_sortindex_helper(0, $self.length, densevector_raw_data($self)))
        val sortedIndices = IndexVector(densevector_fromarray(sortedIndicesRaw,$self.isRow))
        ($self(sortedIndices),sortedIndices)
      }

      infix ("median") (Nil :: T, (TNumeric(T),TOrdering(T))) implements composite ${
        val x = $self.sort
        val mid = x.length / 2
        if (x.length % 2 == 0) {
          ((x(mid).AsInstanceOf[Double] + x(mid-1).AsInstanceOf[Double]) / 2).AsInstanceOf[T]
        }
        else x(mid)
      }
      infix (":>") (DenseVector(T) :: DenseVector(MBoolean), TOrdering(T)) implements zip((T,T,MBoolean), (0,1), ${ (a,b) => a > b })
      infix (":<") (DenseVector(T) :: DenseVector(MBoolean), TOrdering(T)) implements zip((T,T,MBoolean), (0,1), ${ (a,b) => a < b })

      for (rhs <- List(DenseVector(T),DenseVectorView(T),IndexVector)) {
        direct ("__equal") (rhs :: MBoolean) implements composite ${
          if ($self.length != $1.length || $self.isRow != $1.isRow) false
          else {
            val c = $self.indices.count(i => $self(i) != $1(i))
            c == 0
          }
        }
      }

      direct ("__equal") (SparseVector(T) :: MBoolean) implements composite ${ $self == $1.toDense }

      /**
       * Bulk
       */
      infix ("groupByReduce") ((T ==> K,T ==> V,(V,V) ==> V) :: MHashMap(K,V), TArith(V), addTpePars = (K,V)) implements groupByReduce((T,K,V), 0, ${e => $1(e)}, ${e => $2(e)}, ${implicitly[Arith[V]].empty}, ${(a,b) => $3(a,b)})

      infix ("groupBy") ((T ==> K,T ==> V) :: MHashMap(K, DenseVector(V)), addTpePars = (K,V)) implements composite ${
        val hash = densevector_groupby_helper($self,$1,$2)
        val vals = fhashmap_values(hash).map(ab => densevector_fromarray(array_buffer_result(ab), true))
        fhashmap_from_arrays(fhashmap_keys(hash), vals)
      }

      // filter is here, instead of Vector.scala, so that other Vector types can have a different return value
      infix ("filter") ((T ==> MBoolean) :: DenseVector(T)) implements filter((T,T), 0, ${e => $1(e)}, ${e => e})

      /**
       * Required for parallel collection
       */
      compiler ("densevector_appendable") ((MInt,T) :: MBoolean) implements composite("true")
      compiler ("densevector_append") ((MInt,T) :: MUnit, effect = write(0)) implements composite ${
        $self.insert($self.length, $2)
      }
      compiler ("densevector_copy") ((MInt,DenseVector(T),MInt,MInt) :: MUnit, effect = write(2)) implements composite ${
        val src = densevector_raw_data($self)
        val dest = densevector_raw_data($2)
        array_copy(src, $1, dest, $3, $4)
      }

      parallelize as ParallelCollectionBuffer(T, lookupOp("densevector_dc_alloc"), lookupOp("length"), lookupOverloaded("apply",3), lookupOp("update"), lookupOp("densevector_set_length"), lookupOp("densevector_appendable"), lookupOp("densevector_append"), lookupOp("densevector_copy"))
    }

    // the generic Vector.scala reduce requires an Arithmetic type class, so we handle some convenient other cases here
    compiler (DenseVector) ("reduce_and", Nil, DenseVector(MBoolean) :: MBoolean) implements reduce(MBoolean, 0, ${unit(true)}, ${ (a,b) => a && b })
    compiler (DenseVector) ("reduce_or", Nil, DenseVector(MBoolean) :: MBoolean) implements reduce(MBoolean, 0, ${unit(false)}, ${ (a,b) => a || b })

    // Bulk of vector operations is imported
    addVectorCommonOps(DenseVector,T)

    // label DenseVector *:* DenseVectorView so that we can rewrite it in RewriteOpsExp
    label(lookupOverloaded("DenseVector","*:*",1), "densevector_dot_densevectorview")

    // Add DenseVector to Arith
    val Arith = lookupGrp("Arith").asInstanceOf[Rep[DSLTypeClass]]
    val DenseVectorArith = tpeClassInst("ArithDenseVector", T withBound TArith, Arith(DenseVector(T)))
    infix (DenseVectorArith) ("zero", T withBound TArith, DenseVector(T) :: DenseVector(T)) implements composite ${ densevector_fromfunc[T]($0.length, $0.isRow, i => implicitly[Arith[T]].empty) }
    infix (DenseVectorArith) ("empty", T withBound TArith, Nil :: DenseVector(T)) implements composite ${ densevector_fromarray[T](array_empty_imm[T](unit(0)),unit(true)) }
    infix (DenseVectorArith) ("+", T withBound TArith, (DenseVector(T),DenseVector(T)) :: DenseVector(T)) implements composite ${ densevector_pl($0,$1) }
    infix (DenseVectorArith) ("-", T withBound TArith, (DenseVector(T),DenseVector(T)) :: DenseVector(T)) implements composite ${ densevector_sub($0,$1) }
    infix (DenseVectorArith) ("*", T withBound TArith, (DenseVector(T),DenseVector(T)) :: DenseVector(T)) implements composite ${ densevector_mul($0,$1) }
    infix (DenseVectorArith) ("/", T withBound TArith, (DenseVector(T),DenseVector(T)) :: DenseVector(T)) implements composite ${ densevector_div($0,$1) }
    infix (DenseVectorArith) ("abs", T withBound TArith, DenseVector(T) :: DenseVector(T)) implements composite ${ densevector_abs($0) }
    infix (DenseVectorArith) ("exp", T withBound TArith, DenseVector(T) :: DenseVector(T)) implements composite ${ densevector_exp($0) }
    infix (DenseVectorArith) ("log", T withBound TArith, DenseVector(T) :: DenseVector(T)) implements composite ${ densevector_log($0) }

    importDenseVectorPrimitiveOps()
  }



  /**
   * Special cases for DenseVector primitive arithmetic. This is annoying, so let's hide it at the bottom.
   */
  def importDenseVectorPrimitiveOps() {
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")

    // the conversions here will be costly unless things fuse. alternatively, we could convert element by element.
    // TODO: unfortunately, these have priority over operators defined in VectorCommonOps, so they can sometimes force conversions.
    infix (DenseVector) ("+", Nil, (MInt,DenseVector(MInt)) :: DenseVector(MInt)) implements redirect ${ densevector_pl[Int]($1,$0) }
    infix (DenseVector) ("+", Nil, (MInt,DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($1,$0.toFloat) }
    infix (DenseVector) ("+", Nil, (MInt,DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($1,$0.toDouble) }
    infix (DenseVector) ("+", Nil, (MFloat,DenseVector(MInt)) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($1.toFloat,$0) }
    infix (DenseVector) ("+", Nil, (MFloat,DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($1,$0) }
    infix (DenseVector) ("+", Nil, (MFloat,DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($1,$0.toDouble) }
    infix (DenseVector) ("+", Nil, (MDouble,DenseVector(MInt)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($1.toDouble,$0) }
    infix (DenseVector) ("+", Nil, (MDouble,DenseVector(MFloat)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($1.toDouble,$0) }
    infix (DenseVector) ("+", Nil, (MDouble,DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($1,$0) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),MInt) :: DenseVector(MInt)) implements redirect ${ densevector_pl[Int]($0,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),MFloat) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($0.toFloat,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0.toDouble,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),MInt) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($0,$1.toFloat) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),MFloat) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($0,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0.toDouble,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),MInt) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0,$1.toDouble) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),MFloat) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0.toDouble,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),DenseVector(MInt)) :: DenseVector(MInt)) implements redirect ${ densevector_pl[Int]($0,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($0.toFloat,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0.toDouble,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),DenseVector(MInt)) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($0,$1.toFloat) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_pl[Float]($0,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0.toDouble,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),DenseVector(MInt)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0,$1.toDouble) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),DenseVector(MFloat)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0,$1.toDouble) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_pl[Double]($0,$1) }

    infix (DenseVector) ("-", Nil, (MInt,DenseVector(MInt)) :: DenseVector(MInt)) implements redirect ${ densevector_map[Int,Int]($1, e => forge_int_minus($0,e)) }
    infix (DenseVector) ("-", Nil, (MInt,DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_map[Float,Float]($1, e => forge_float_minus($0.toFloat,e)) }
    infix (DenseVector) ("-", Nil, (MInt,DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_map[Double,Double]($1, e => forge_double_minus($0.toDouble,e)) }
    infix (DenseVector) ("-", Nil, (MFloat,DenseVector(MInt)) :: DenseVector(MFloat)) implements redirect ${ densevector_map[Int,Float]($1, e => forge_float_minus($0,e)) }
    infix (DenseVector) ("-", Nil, (MFloat,DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_map[Float,Float]($1, e => forge_float_minus($0,e)) }
    infix (DenseVector) ("-", Nil, (MFloat,DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_map[Double,Double]($1, e => forge_double_minus($0.toDouble,e)) }
    infix (DenseVector) ("-", Nil, (MDouble,DenseVector(MInt)) :: DenseVector(MDouble)) implements redirect ${ densevector_map[Int,Double]($1, e => forge_double_minus($0,e.toDouble)) }
    infix (DenseVector) ("-", Nil, (MDouble,DenseVector(MFloat)) :: DenseVector(MDouble)) implements redirect ${ densevector_map[Float,Double]($1, e => forge_double_minus($0,e.toDouble)) }
    infix (DenseVector) ("-", Nil, (MDouble,DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_map[Double,Double]($1, e => forge_double_minus($0,e)) }
    infix (DenseVector) ("-", Nil, (DenseVector(MInt),MInt) :: DenseVector(MInt)) implements redirect ${ densevector_sub[Int]($0,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MInt),MFloat) :: DenseVector(MFloat)) implements redirect ${ densevector_sub[Float]($0.toFloat,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MInt),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0.toDouble,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),MInt) :: DenseVector(MFloat)) implements redirect ${ densevector_sub[Float]($0,$1.toFloat) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),MFloat) :: DenseVector(MFloat)) implements redirect ${ densevector_sub[Float]($0,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0.toDouble,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),MInt) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0,$1.toDouble) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),MFloat) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0.toDouble,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MInt),DenseVector(MInt)) :: DenseVector(MInt)) implements redirect ${ densevector_sub[Int]($0,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MInt),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_sub[Float]($0.toFloat,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MInt),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0.toDouble,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),DenseVector(MInt)) :: DenseVector(MFloat)) implements redirect ${ densevector_sub[Float]($0,$1.toFloat) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_sub[Float]($0,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0.toDouble,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),DenseVector(MInt)) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0,$1.toDouble) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),DenseVector(MFloat)) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0,$1.toDouble) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_sub[Double]($0,$1) }

    infix (DenseVector) ("unary_-", Nil, (DenseVector(MInt)) :: DenseVector(MInt)) implements redirect ${ densevector_mul[Int]($0,unit(-1)) }
    infix (DenseVector) ("unary_-", Nil, (DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0,unit(-1f)) }
    infix (DenseVector) ("unary_-", Nil, (DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,unit(-1.0)) }
    infix (DenseVector) ("*", Nil, (MInt,DenseVector(MInt)) :: DenseVector(MInt)) implements redirect ${ densevector_mul[Int]($1,$0) }
    infix (DenseVector) ("*", Nil, (MInt,DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($1,$0.toFloat) }
    infix (DenseVector) ("*", Nil, (MInt,DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($1,$0.toDouble) }
    infix (DenseVector) ("*", Nil, (MFloat,DenseVector(MInt)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($1.toFloat,$0) }
    infix (DenseVector) ("*", Nil, (MFloat,DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($1,$0) }
    infix (DenseVector) ("*", Nil, (MFloat,DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($1,$0.toDouble) }
    infix (DenseVector) ("*", Nil, (MDouble,DenseVector(MInt)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($1.toDouble,$0) }
    infix (DenseVector) ("*", Nil, (MDouble,DenseVector(MFloat)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($1.toDouble,$0) }
    infix (DenseVector) ("*", Nil, (MDouble,DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($1,$0) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),MInt) :: DenseVector(MInt)) implements redirect ${ densevector_mul[Int]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),MFloat) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0.toFloat,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),MInt) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0,$1.toFloat) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),MFloat) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),MInt) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,$1.toDouble) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),MFloat) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),DenseVector(MInt)) :: DenseVector(MInt)) implements redirect ${ densevector_mul[Int]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0.toFloat,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),DenseVector(MInt)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0,$1.toFloat) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),DenseVector(MInt)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,$1.toDouble) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),DenseVector(MFloat)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,$1.toDouble) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),DenseMatrix(MInt)) :: DenseVector(MInt)) implements redirect ${ densevector_mul[Int]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),DenseMatrix(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0.toFloat,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),DenseMatrix(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),DenseMatrix(MInt)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0,$1.toFloat) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),DenseMatrix(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_mul[Float]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),DenseMatrix(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),DenseMatrix(MInt)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,$1.toDouble) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),DenseMatrix(MFloat)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,$1.toDouble) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),DenseMatrix(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_mul[Double]($0,$1) }

    infix (DenseVector) ("/", Nil, (DenseVector(MInt),MInt) :: DenseVector(MInt)) implements redirect ${ densevector_div[Int]($0,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MInt),MFloat) :: DenseVector(MFloat)) implements redirect ${ densevector_div[Float]($0.toFloat,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MInt),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0.toDouble,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),MInt) :: DenseVector(MFloat)) implements redirect ${ densevector_div[Float]($0,$1.toFloat) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),MFloat) :: DenseVector(MFloat)) implements redirect ${ densevector_div[Float]($0,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0.toDouble,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),MInt) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0,$1.toDouble) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),MFloat) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0.toDouble,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),MDouble) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MInt),DenseVector(MInt)) :: DenseVector(MInt)) implements redirect ${ densevector_div[Int]($0,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MInt),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_div[Float]($0.toFloat,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MInt),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0.toDouble,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),DenseVector(MInt)) :: DenseVector(MFloat)) implements redirect ${ densevector_div[Float]($0,$1.toFloat) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densevector_div[Float]($0,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0.toDouble,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),DenseVector(MInt)) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0,$1.toDouble) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),DenseVector(MFloat)) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0,$1.toDouble) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densevector_div[Double]($0,$1) }
  }
}

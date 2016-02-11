package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait IndexVectorOps {
  this: OptiLADSL =>

  def importIndexVectorOps() {
    val IndexVector = lookupTpe("IndexVector")
    val DenseVector = lookupTpe("DenseVector")

    // data fields - we take a tagged union approach to enable range- and sequence- based IndexVectors without subtyping
    data(IndexVector, ("_data", MArray(MInt)), ("_start", MInt), ("_end", MInt), ("_isRow", MBoolean), ("_isRange", MBoolean))

    // static methods
    static (IndexVector) ("apply", Nil, (MInt,MInt) :: IndexVector) implements redirect ${ IndexVector($0,$1,unit(true)) }
    static (IndexVector) ("apply", Nil, (MInt,MInt,MBoolean) :: IndexVector) implements
      allocates(IndexVector, ${ array_empty_imm[Int](unit(0)) }, quotedArg(0), quotedArg(1), quotedArg(2), ${ unit(true) })

    static (IndexVector) ("apply", Nil, DenseVector(MInt) :: IndexVector) implements redirect ${ IndexVector($0,$0.isRow) }
    static (IndexVector) ("apply", Nil, (DenseVector(MInt), MBoolean) :: IndexVector) implements
      allocates(IndexVector, ${ indexvector_copyarray($0) }, ${ unit(0) }, ${ unit(0) }, quotedArg(1), ${ unit(false) })

    static (IndexVector) ("apply", Nil, MethodSignature(List(MArray(MInt), ("isRow", MBoolean, "unit(true)")), IndexVector)) implements
      redirect ${ indexvector_fromarray($0,isRow) }

    direct (IndexVector) ("indexvector_fromarray", Nil, (MArray(MInt),MBoolean) :: IndexVector) implements
      allocates(IndexVector, quotedArg(0), ${ unit(0) }, ${ unit(0) }, quotedArg(1), ${ unit(false) })

    compiler (IndexVector) ("indexvector_copyarray", Nil, DenseVector(MInt) :: MArray(MInt)) implements composite ${
      val d = array_empty[Int]($0.length)
      $0.indices foreach { i => d(i) = $0(i) }
      d.unsafeImmutable

      // FIXME: using the pure version causes SparseVectorSuite testAccessors to fail with a fusion exception (applyAddCondition not implemented)
      // array_fromfunction($0.length, { i => $0(i) })
    }

    // this is unsafe because it uses the underlying input array directly instead of copying
    // they should only be used if we know the intermediate reference is dead or immutable (to avoid unsafe aliasing)
    // TODO: for some reason this does not work when we think it should, so we are reverting to the safer copy-always policy.

    // compiler (IndexVector) ("unsafe_dense_to_index", Nil, DenseVector(MInt) :: IndexVector) implements composite ${
    //   indexvector_fromarray(densevector_raw_data($0), $0.isRow)
    // }

    // index helpers
    for (arity <- 2 to 6) {
      val Tup = tpeInst(lookupTpe("Tuple"+arity, stage = compile), (0 until arity).map(i => MInt).toList)

      // unroll during staging to specialize for each arity
      val d = (2 to arity).map(k => "dims._" + k)
      val s1 = d.scanRight("1")((a,b) => a + "*" + b)

      val s2 = s1.zipWithIndex.map(t => "inds._"+(t._2+1) + "*" + t._1)
      val retFlat = s2.mkString(" + ")
      // e.g. for index (i,j,k,l) and dims (a,b,c,d), returns (i*b*c*d + j*c*d + k*d + l)
      direct (IndexVector) ("flatten", Nil, (("inds",Tup),("dims",Tup)) :: MInt) implements redirect ${ \$retFlat }

      val s3 = s1.zipWithIndex.map(t => "(i / (" + t._1 + ")) % dims._" + (t._2+1))
      val retTuple = s3.mkString("(",",",")")
      // e.g. for index i and dims (a,b,c,d), returns [i/dcb % a, i/dc % b, i/d % c, i/1 % d]
      direct (IndexVector) ("unflatten", Nil, (("i",MInt),("dims",Tup)) :: Tup) implements redirect ${ \$retTuple }
    }

    //val IndexVectorOps = withTpe(IndexVector)
    //IndexVectorOps {
    import org.scala_lang.virtualized.virtualize
    magic()
    @virtualize
    def magic[R]() = withTpee(IndexVector){
      compiler ("indexvector_start") (Nil :: MInt) implements getter(0, "_start")
      compiler ("indexvector_end") (Nil :: MInt) implements getter(0, "_end")
      compiler ("indexvector_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_data")
      compiler ("indexvector_is_range") (Nil :: MBoolean) implements getter(0, "_isRange")

      // TODO: the _isRange field should be a compile-time constant. can this be optimized (or does it already) eliminate the conditional in length/apply?

      infix ("length") (Nil :: MInt) implements composite ${
        if (indexvector_is_range($self)) {
          indexvector_end($self) - indexvector_start($self)
        }
        else {
          array_length(indexvector_raw_data($self))
        }
      }
      infix ("isRow") (Nil :: MBoolean) implements getter(0, "_isRow")
      infix ("apply") (MInt :: MInt) implements composite ${
        fassert($1 >= 0 && $1 < $self.length, "IndexVector apply out of bounds at index " + $1)
        if (indexvector_is_range($self)) {
          indexvector_start($self) + $1
        }
        else {
          indexvector_raw_data($self).apply($1)
        }
      }
      infix ("apply") (IndexVector :: IndexVector) implements composite ${
        val out = $1 { i => $self(i) }
        IndexVector(out, $self.isRow)
      }

      infix ("slice") ((("start",MInt),("end",MInt)) :: IndexVector) implements composite ${
        if (indexvector_is_range($self)) {
          fassert($start >= indexvector_start($self) && $end <= indexvector_end($self), "IndexVector slice (" + $start + "," + $end + ") out of bounds (" + indexvector_start($self) + "," + indexvector_end($self) + ")")
          IndexVector($start, $end, $self.isRow)
        }
        else {
          $self($start::$end)
        }
      }

      infix ("t") (Nil :: IndexVector) implements allocates(IndexVector, ${indexvector_raw_data($self)}, ${indexvector_start($self)}, ${indexvector_end($self)}, ${!(indexvector_isrow($self))}, ${indexvector_is_range($self)})

      infix ("Clone") (Nil :: IndexVector, aliasHint = copies(0)) implements composite ${
        if (indexvector_is_range($self)) {
          IndexVector(indexvector_start($self),indexvector_end($self),$self.isRow)
        }
        else {
          indexvector_fromarray(array_clone(indexvector_raw_data($self)), $self.isRow)
        }
      }

      infix ("toDense") (Nil :: DenseVector(MInt)) implements composite ${
        if (indexvector_is_range($self)) { $self.map(e => e) }
        else {
          // this is safe because we are constructing an immutable DenseVector and IndexVectors are always immutable
          densevector_fromarray(indexvector_raw_data($0), $0.isRow)
        }
      }

      direct ("infix_==") (IndexVector :: MBoolean) implements composite ${ $self.toDense == $1 }
      direct ("infix_==") (DenseVector(MInt) :: MBoolean) implements composite ${ $1 == $self }

      // compiler ("indexvector_filter_helper") (IndexVector, MInt ==> MBoolean) :: DenseVector(MInt)) implements filter((MInt,MInt), 0, ${e => $1(e)}, ${e => e})
      // infix ("filter") ((MInt ==> MBoolean) :: IndexVector) implements composite ${
      //   IndexVector(indexvector_filter_helper($self, $1))
      // }

      infix ("filter") ((MInt ==> MBoolean) :: IndexVector) implements composite ${
        // map over array instead of using toDense for fusion
        val data = array_filter($self.toArray, $1)
        indexvector_fromarray(data, $self.isRow)
      }

      // These are required because reduce currently requires a collection of type A to be matched with a signature (A,A) => A.
      // Therefore, we need a method that takes as input a collection of type Int in order to reduce it with the proper zero values.
      val T = tpePar("T")
      compiler ("min_index_of") (DenseVector(T) :: MInt, TOrdering(T), addTpePars = T) implements reduce(MInt, 0, "unit(Int.MaxValue)", ${ (a,b) => if ($1(a) < $1(b)) a else b})
      compiler ("max_index_of") (DenseVector(T) :: MInt, TOrdering(T), addTpePars = T) implements reduce(MInt, 0, "unit(Int.MinValue)", ${ (a,b) => if ($1(a) > $1(b)) a else b})

      // parallel, so the conversion can fuse with the consumer
      // is this fast and robust enough to capture parallel operators over index vectors?
      fimplicit ("indexToDense") (Nil :: DenseVector(MInt)) implements composite ${
        if (Settings.verbose > 0) println("(performance warning): automatic conversion from IndexVector to DenseVector")
        $self.toDense
      }

      // naming is by convention here, a little brittle. would it be better to put this in extern?
      val grpName = if (Config.fastCompile) "$Flat" else "DenseVector"
      fimplicit ("chainIndexToDenseOps") (Nil :: ephemeralTpe(grpName+"DenseVectorOpsCls[Int]", stage = now)) implements composite ${
        repTo\${grpName}DenseVectorOpsCls(indexToDense($self))
      }
      fimplicit ("chainIndexToDenseIntOps") (Nil :: ephemeralTpe(grpName+"DenseVectorIntOpsCls", stage = now)) implements composite ${
        repTo\${grpName}DenseVectorIntOpsCls(indexToDense($self))
      }

      compiler ("indexvector_illegalalloc") (MInt :: MNothing) implements composite ${ fatal("IndexVectors cannot be allocated from a parallel op") }
      compiler ("indexvector_illegalupdate") ((MInt, MInt) :: MNothing) implements composite ${ fatal("IndexVectors cannot be updated") }

      // IndexVectors can't be mapped over, but they can be zipped with or reduced
      parallelize as ParallelCollection(MInt, lookupOp("indexvector_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",5), lookupOp("indexvector_illegalupdate"))
    }

    // allows us to perform operations without converting to a DenseVector first
    addVectorCommonOps(IndexVector,MInt)
  }
}

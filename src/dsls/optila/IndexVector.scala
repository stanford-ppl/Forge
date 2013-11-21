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

    compiler (IndexVector) ("indexvector_fromarray", Nil, (MArray(MInt),MBoolean) :: IndexVector) implements
      allocates(IndexVector, quotedArg(0), ${ unit(0) }, ${ unit(0) }, quotedArg(1), ${ unit(false) })

    compiler (IndexVector) ("indexvector_copyarray", Nil, DenseVector(MInt) :: MArray(MInt)) implements composite ${
      val d = array_empty[Int]($0.length)
      $0.indices foreach { i => d(i) = $0(i) }
      d.unsafeImmutable
    }

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

    val IndexVectorOps = withTpe(IndexVector)
    IndexVectorOps {
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
        if (indexvector_is_range($self)) {
          indexvector_start($self) + $1
        }
        else {
          indexvector_raw_data($self).apply($1)
        }
      }

      infix ("slice") ((("start",MInt),("end",MInt)) :: IndexVector) implements composite ${
        if (indexvector_is_range($self)) {
          // if ($start < indexvector_start($self) || $end > indexvector_end($self))
          //   fatal("IndexVector slice (" + $start + "," + $end + ") out of bounds (" + indexvector_start($self) + "," + indexvector_end($self) + ")")
          IndexVector($start,$end,$self.isRow)
        }
        else {
          IndexVector(densevector_fromarray(indexvector_raw_data($self), $self.isRow).slice($start,$end))
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

      infix ("toDense") (Nil :: DenseVector(MInt)) implements composite ${ $self.map(e => e) }

      direct ("__equal") (IndexVector :: MBoolean) implements composite ${ $self.toDense == $1 }
      direct ("__equal") (DenseVector(MInt) :: MBoolean) implements composite ${ $1 == $self }

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
      parallelize as ParallelCollection(MInt, lookupOp("indexvector_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",4), lookupOp("indexvector_illegalupdate"))
    }

    // allows us to perform operations without converting to a DenseVector first
    addVectorCommonOps(IndexVector,MInt)
  }
}

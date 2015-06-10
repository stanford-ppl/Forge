package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait MultiArrayConstructors {
	this: OptiLADSL =>
	
	def importConstructors() {
    val IndexVector = lookupTpe("IndexVector")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val DenseTensor3 = lookupTpe("DenseTensor3")
    val DenseTensor4 = lookupTpe("DenseTensor4")
    val DenseTensor5 = lookupTpe("DenseTensor5")
    val T = tpePar("T")
    
    // should add apply directly to IndexVector, otherwise we have issues with ambiguous implicit conversions

 	 	// Vector Constructor
	  infix (IndexVector) ("apply", T, (IndexVector, (MInt ==> T)) :: DenseVector(T)) implements composite ${ 
      fassert(!isWild($0), "DenseVector constructor is not defined for Wildcard")
      $0.map($1)
    }

		// Matrix Constructors
    infix (IndexVector) ("apply", T, (CTuple2(IndexVector, IndexVector), ((MInt, MInt) ==> T) ) :: DenseMatrix(T)) implements composite ${ 
    	val (rowIndices, colIndices) = $0
      fassert(!isWild(rowIndices) && !isWild(colIndices), "Element-wise DenseMatrix constructor is not defined for Wildcards")

      val data = Array2D.fromFunction(rowIndices.length, colIndices.length){(i,j) => $1(rowIndices(i),colIndices(j)) }
      densematrix_fromarray2d(data)
    }

    // FIXME: the effectful implementation here prevents these operations from being fused, hoisted, or distributed
    //        distribution is prevented by the output matrix allocation happening on the master
    //        (even though the foreach afterwards could be distributed)

		// Assumes vector size is pure across iterations to maintain normal loop size    
    /*infix (IndexVector) ("apply", T, (CTuple2(IndexVector, IndexVector), MInt ==> DenseVector(T)) :: DenseMatrix(T)) implements composite ${
      val (rowIndices, colIndices) = $0
      fassert(!isWild(rowIndices) || !isWild(colIndices), "Vector-based DenseMatrix constructor is not defined for (Wildcard, Wildcard)")
      fassert(isWild(rowIndices) || isWild(colIndices), "Vector-based DenseMatrix constructor is not defined for (IndexVector, IndexVector)")
			fwarn("Use of vector-based constructor is discouraged - use element-wise constructor if possible", 0)
      
      val indices = if (isWild(rowIndices)) colIndices else rowIndices
      val first = $1(indices(0))
      val nrows = if (isWild(rowIndices)) first.length else rowIndices.length
      val ncols = if (isWild(colIndices)) first.length else colIndices.length
      
      val out = DenseMatrix[T](nrows, ncols)
      (0::indices.length) foreach { x => 
      	if (isWild(rowIndices)) out(*, x) = $1(indices(x))	// col based constructor
      	if (isWild(colIndices)) out(x, *) = $1(indices(x)) 	// row based constructor
      }
      out.unsafeImmutable
    }*/

    // Experimental version - no preallocation but requires a transpose for column-wise building
    // Also requires keeping a full copy of the flattened data?
    infix (IndexVector) ("apply", T, (CTuple2(IndexVector, IndexVector), MInt ==> DenseVector(T)) :: DenseMatrix(T)) implements composite ${
      val (rowIndices, colIndices) = $0
      fassert(!isWild(rowIndices) || !isWild(colIndices), "Vector-based DenseMatrix constructor is not defined for (Wildcard, Wildcard)")
      fassert(isWild(rowIndices) || isWild(colIndices), "Vector-based DenseMatrix constructor is not defined for (IndexVector, IndexVector)")
      fwarn("Use of vector-based constructor is discouraged - use element-wise constructor if possible", 0)
      
      val indices = if (isWild(rowIndices)) colIndices else rowIndices
      val vec = indices.flatMap($1)
      val nrows = if (isWild(rowIndices)) vec.length / indices.length else indices.length
      val ncols = if (isWild(colIndices)) vec.length / indices.length else indices.length
      if (isWild(colIndices)) vec.reshape(nrows, ncols)
      else                    vec.reshape(nrows, ncols).t
    }

    // TODO: Update Tensor Constructors
    /*
    // 3D MultiArray Constructors
    infix (IndexVector) ("apply", T, (CTuple3(IndexVector, IndexVector, IndexVector), ((MInt, MInt, MInt) ==> T) ) :: MultiArray(T)) implements composite ${ 
    	cassert(!isWild($0._1) && !isWild($0._2) && !isWild($0._3), "Element-wise 3D MultiArray constructor is not defined for Wildcards")
    	fmultia_fromfunc[T](Seq($0._1.length, $0._2.length, $0._3.length), mutable = false, {d: Seq[Rep[Int]] => 
    		$1($0._1(d(0)), $0._2(d(1)), $0._3(d(2)))
    	})
    }

		infix (IndexVector) ("apply", T, (CTuple3(IndexVector, IndexVector, IndexVector), (MInt, MInt) ==> DenseVector(T)) :: MultiArray(T)) implements composite ${
			val seq = Seq($0._1, $0._2, $0._3)
			val wilds = seq.count{i => isWild(i)}
			val errmsg = seq.map{i => if (isWild(i)) "Wildcard" else "IndexVector"}.mkString("(",",",")")
			cassert(wilds == 1, "Vector based MultiArray constructor is not defined for " + errmsg)
			cwarn("Use of vector-based constructor is discouraged - use element-wise constructor if possible", 0)

			val indices1 = if (!isWild($0._1)) $0._1 else $0._2
			val indices2 = if (!isWild($0._3)) $0._3 else $0._2
			val first = $1(indices1(0), indices2(0))
			val nrows  = if (isWild($0._1)) first.length else $0._1.length
			val ncols  = if (isWild($0._2)) first.length else $0._2.length
			val npages = if (isWild($0._3)) first.length else $0._3.length
			val out = MultiArray[T](nrows, ncols, npages)
			(0::indices1.length) foreach {x =>
				IndexVector(0, indices2.length) foreach {y =>
					if (isWild($0._1)) out(*, idx(x), idx(y)) = $1(indices1(x), indices2(y))
					if (isWild($0._2)) out(idx(x), *, idx(y)) = $1(indices1(x), indices2(y))
					if (isWild($0._3)) out(idx(x), idx(y), *) = $1(indices1(x), indices2(y))
			}}
			out.unsafeImmutable
		}	

		infix (IndexVector) ("apply", T, (CTuple3(IndexVector, IndexVector, IndexVector), MInt ==> DenseMatrix(T)) :: MultiArray(T)) implements composite ${
			val seq = Seq($0._1, $0._2, $0._3)
			val wilds = seq.count{i => isWild(i)}
			val errmsg = seq.map{i => if (isWild(i)) "Wildcard" else "IndexVector"}.mkString("(",",",")")
			cassert(wilds == 2, "Matrix based MultiArray constructor is not defined for " + errmsg)
			cwarn("Use of matrix-based constructor is discouraged - use element-wise constructor if possible", 0)

			val indices = if (!isWild($0._1)) $0._1 else if (!isWild($0._2)) $0._2 else $0._3
			val first = $1(indices(0))
			val nrows  = if (!isWild($0._1)) $0._1.length else first.numRows
			val ncols  = if (!isWild($0._2)) $0._2.length else if (!isWild($0._1)) first.numRows else first.numCols
			val npages = if (!isWild($0._3)) $0._3.length else first.numCols
			val out = MultiArray[T](nrows, ncols, npages)
			(0::indices.length) foreach {x =>
				if (!isWild($0._1)) out(idx(x), *, *) = $1(indices(x))
				if (!isWild($0._2)) out(*, idx(x), *) = $1(indices(x))
				if (!isWild($0._3)) out(*, *, idx(x)) = $1(indices(x))
			}
			out.unsafeImmutable
		}

    // 4D MultiArray Constructors
  	infix (IndexVector) ("apply", T, (CTuple4(IndexVector, IndexVector, IndexVector, IndexVector), ((MInt, MInt, MInt, MInt) ==> T)) :: MultiArray(T)) implements composite ${ 
  		cassert(!isWild($0._1) && !isWild($0._2) && !isWild($0._3) && !isWild($0._4), "Element-wise 4D MultiArray constructor is not defined for Wildcards")
  		fmultia_fromfunc[T](Seq($0._1.length, $0._2.length, $0._3.length, $0._4.length), mutable = false, {d: Seq[Rep[Int]] => 
  			$1($0._1(d(0)), $0._2(d(1)), $0._3(d(2)), $0._4(d(3)))
  		})
  	}

  	infix (IndexVector) ("apply", T, (CTuple4(IndexVector, IndexVector, IndexVector, IndexVector), ((MInt, MInt, MInt) ==> DenseVector(T))) :: MultiArray(T)) implements composite ${
  		val seq = Seq($0._1, $0._2, $0._3, $0._4)
  		val wilds = seq.map{i => isWild(i)}
  		val errmsg = seq.map{i => if (isWild(i)) "Wildcard" else "IndexVector"}.mkString("(",",",")")
  		cassert(wilds == 1, "Vector-based MultiArray constructor is not defined for " + errmsg)
  		cwarn("Use of vector-based constructor is discouraged - use element-wise constructor if possible", 0)

  		val indices1 = if (isWild($0._1)) $0._2 else $0._1
  		val indices2 = if (isWild($0._1) || isWild($0._2)) $0._3 else $0._2
  		val indices3 = if (isWild($0._4)) $0._3 else $0._4
  		val first = $1(indices1(0),indices2(0),indices3(0))
  		val nrows  = if (isWild($0._1)) first.length else $0._1.length
  		val ncols  = if (isWild($0._2)) first.length else $0._2.length
  		val npages = if (isWild($0._3)) first.length else $0._3.length
  		val nbanks = if (isWild($0._4)) first.length else $0._4.length
  		val out = MultiArray[T](nrows, ncols, npages, nbanks)
  		(0::indices1.length) foreach {x =>
  			IndexVector(0, indices2.length) foreach {y => 
  			  IndexVector(0, indices3.length) foreach {z =>
  					if (isWild($0._1)) out(*, idx(x), idx(y), idx(z)) = $1(indices1(x),indices2(y),indices3(z))
  					if (isWild($0._2)) out(idx(x), *, idx(y), idx(z)) = $1(indices1(x),indices2(y),indices3(z))
  					if (isWild($0._3)) out(idx(x), idx(y), *, idx(z)) = $1(indices1(x),indices2(y),indices3(z))
  					if (isWild($0._4)) out(idx(x), idx(y), idx(z), *) = $1(indices1(x),indices2(y),indices3(z))
  		}}}
  		out.unsafeImmutable
  	}

  	infix (IndexVector) ("apply", T, (CTuple4(IndexVector, IndexVector, IndexVector, IndexVector), ((MInt, MInt) ==> DenseMatrix(T))) :: MultiArray(T)) implements composite ${
  		val seq = Seq($0._1, $0._2, $0._3, $0._4)
  		val wilds = seq.map{i => isWild(i)}
  		val errmsg = seq.map{i => if (isWild(i)) "Wildcard" else "IndexVector"}.mkString("(",",",")")
  		cassert(wilds == 2, "Matrix-based MultiArray constructor is not defined for " + errmsg)
  		cwarn("Use of matrix-based constructor is discouraged - use element-wise constructor if possible", 0)

  		val indices1 = if (!isWild($0._1)) $0._1 else if (!isWild($0._2)) $0._2 else $0._3
  		val indices2 = if (!isWild($0._4)) $0._4 else if (!isWild($0._3)) $0._3 else $0._2 
  		val first = $1(indices1(0),indices2(0))
  		val nrows  = if (!isWild($0._1)) $0._1.length else first.numRows
  		val ncols  = if (!isWild($0._2)) $0._2.length else if (!isWild($0._1)) first.numRows else first.numCols
  		val npages = if (!isWild($0._3)) $0._3.length else if (isWild($0._4)) first.numRows else first.numCols
  		val nbanks = if (!isWild($0._4)) $0._4.length else first.numCols
  		val out = MultiArray[T](nrows, ncols, npages, nbanks)
  		(0::indices1.length) foreach {x =>
  			IndexVector(0, indices2.length) foreach {y =>
  				if (isWild($0._1) && isWild($0._2)) out(*, *, idx(x), idx(y)) = $1(indices1(x),indices2(y))
  				if (isWild($0._1) && isWild($0._3)) out(*, idx(x), *, idx(y)) = $1(indices1(x),indices2(y))
  				if (isWild($0._1) && isWild($0._4)) out(*, idx(x), idx(y), *) = $1(indices1(x),indices2(y))
  				if (isWild($0._2) && isWild($0._3)) out(idx(x), *, *, idx(y)) = $1(indices1(x),indices2(y))
  				if (isWild($0._2) && isWild($0._4)) out(idx(x), *, idx(y), *) = $1(indices1(x),indices2(y))
  				if (isWild($0._3) && isWild($0._4)) out(idx(x), idx(y), *, *) = $1(indices1(x),indices2(y))
  		}}
  		out.unsafeImmutable
  	}

  	// is this needed? will this even work?
  	// is there a way to make sure that the function results in a 3D multiarray?
  	infix (IndexVector) ("apply", T, (CTuple4(IndexVector, IndexVector, IndexVector, IndexVector), (MInt ==> MultiArray(T))) :: MultiArray(T)) implements composite ${
  		val seq = Seq($0._1, $0._2, $0._3, $0._4)
  		val wilds = seq.map{i => isWild(i)}
  		val errmsg = seq.map{i => if (isWild(i)) "Wildcard" else "IndexVector"}.mkString("(",",",")")
  		cassert(wilds == 3, "3D-based MultiArray constructor is not defined for " + errmsg)
  		cwarn("Use of 3D-based constructor is discouraged - use element-wise constructor if possible", 0)
	
			val indices = if (!isWild($0._1)) $0._1 else if (!isWild($0._2)) $0._2 else if (!isWild($0._3)) $0._3 else $0._4
			val first = $1(indices(0))
			val nrows  = if (!isWild($0._1)) $0._1.length else first.numRows
			val ncols  = if (!isWild($0._2)) $0._2.length else if (!isWild($0._1)) first.numRows else first.numCols
			val npages = if (!isWild($0._3)) $0._3.length else if (!isWild($0._4)) first.numPages else first.numCols
			val nbanks = if (!isWild($0._4)) $0._4.length else first.numPages
			val out = MultiArray[T](nrows, ncols, npages, nbanks)
			(0::indices.length) foreach {x =>
				if (!isWild($0._1)) out(idx(x), *, *, *) = $1(indices(x))
				if (!isWild($0._2)) out(*, idx(x), *, *) = $1(indices(x))
				if (!isWild($0._3)) out(*, *, idx(x), *) = $1(indices(x))
				if (!isWild($0._4)) out(*, *, *, idx(x)) = $1(indices(x))
			}
			out.unsafeImmutable
   	}*/

   	// General constructors for N-D arrays (not quite as nice looking app-side)
  	/*compiler (MultiArray) ("multia_constructN", T, (ISeq(IndexVector), (ISeq(MInt) ==> T)) :: MultiArray(T)) implements composite ${
  		cassert($0.map{i => !isWild(i)}.reduce{(a,b)=>a&&b}, "MultiArray constructor is not defined for Wildcard")
    	val dims = $0.map{idx => idx.length}
    	fmultia_fromfunc[T](dims, mutable = false, {d:Seq[Rep[Int]] => 
    		val indices = $0.zip(d).map{i => i._1.apply(i._2) }
    		$1( indices ) 
    	})
  	}*/

  	// Doesn't work right now - can't add an infix operation to Seq[IndexVector], apparently
  	/*infix (IndexVector) ("apply", T, (ISeq(IndexVector), (ISeq(MInt) ==> T)) :: MultiArray(T)) implements composite ${ 
    	multia_constructN($0, $1)
 	 	}*/

  }
}
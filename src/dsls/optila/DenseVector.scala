package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait DenseVectorOps {
  this: OptiLADSL =>

	def importDenseVectorOps() {
    val MArray1D = lookupTpe("Array1D")
    val MMap = lookupTpe("ForgeMap")
    val IndexVector = lookupTpe("IndexVector")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val SparseVector = lookupTpe("SparseVector")
    val Tuple2 = lookupTpe("Tup2")
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")

    // Data fields
    data(DenseVector, ("_data", MArray1D(T)), ("_isRow", MBoolean))
    compiler (DenseVector) ("densevector_raw_data", T, DenseVector(T) :: MArray1D(T)) implements getter(0, "_data")
    compiler (DenseVector) ("densevector_set_raw_data", T, (DenseVector(T), MArray1D(T)) :: MUnit) implements setter(0, "_data", ${$1})
    compiler (DenseVector) ("densevector_set_is_row", T, (DenseVector(T), MBoolean) :: MUnit) implements setter(0, "_isRow", ${$1})
    compiler (DenseVector) ("isWild", T, DenseVector(T) :: MBoolean) implements composite ${ unit(false) }
    compiler (DenseVector) ("isRange", T, DenseVector(T) :: MBoolean) implements composite ${ unit(false) }

    // --- Vector Constructors
    compiler (DenseVector) ("densevector_fromarray1d", T, (MArray1D(T), MBoolean) :: DenseVector(T)) implements allocates(DenseVector, ${$0}, ${$1})
    compiler (DenseVector) ("densevector_fromfunc", T, (MInt, (MInt ==> T)) :: DenseVector(T)) implements redirect ${ densevector_fromfunc($0, $1, unit(true)) }
    compiler (DenseVector) ("densevector_fromfunc", T, (MInt, (MInt ==> T), MBoolean) :: DenseVector(T)) implements composite ${
      val data = Array1D.fromFunction($1){i => $2(i)}
      densevector_fromarray1D(data, $3)
    }
    
    // Tuples to DenseVector
    for (arity <- (2 until 23)) {
      // We use "Reppable" to allow heterogeneous tuples (e.g. T, Rep[T], Var[T]) to still be converted
      val pars = tpePar("T") :: (1 until arity).map(i => tpePar(('T'.toInt + i).toChar.toString, Nil)).toList
      val impls = (1 until arity).map(i => TReppable(pars(i), pars(0))).toList
      
      // we need a version where T (the return type) is T, Rep[T], and Var[T] in order to get type inference to "always" work
      // using a different type parameter to specify the return type (like RR) almost works, but isn't always inferred
      val elems = ((2 to arity).map(i => implicitOpArgPrefix + (i-2) + ".view(t._"+i+")").toList).mkString(",")
      val TT = tpe("Tuple" + arity, pars, stage = compile)
      fimplicit (DenseVector) ("tupleToDense" + arity, pars, (("t",TT) :: DenseVector(pars(0))), impls) implements redirect ${ DenseVector[T](unit(t._1),\$elems) }

      // we hack the first argument to be Rep[T] or Var[T] as needed here.
      val TR = tpe("Tuple" + arity, tpePar("Rep[T]") :: pars.drop(1), stage = compile)
      fimplicit (DenseVector) ("repTupleToDense" + arity, pars, (("t",TR) :: DenseVector(pars(0))), impls) implements redirect ${ DenseVector[T](t._1,\$elems) }

      val TV = tpe("Tuple" + arity, tpePar("Var[T]") :: pars.drop(1), stage = compile)
      fimplicit (DenseVector) ("varTupleToDense" + arity, pars, (("t",TV) :: DenseVector(pars(0))), impls) implements redirect ${ DenseVector[T](readVar(t._1),\$elems) }
    }

    // DenseVector static methods
    static (DenseVector) ("apply", T, (MInt, MBoolean) :: DenseVector(T), effect = mutable) implements composite ${ densevector_fromarray1d(Array1D[T]($0), $1) }
    static (DenseVector) ("apply", T, varArgs(T) :: DenseVector(T)) implements composite ${ densevector_fromarray1d(fmultia_fromseq($0), unit(true)) }

    // Changed from a sequential loop with updates to a flatMap on the outer vector
    static (DenseVector) ("flatten", T, DenseVector(DenseVector(T)) :: DenseVector(T)) implements composite ${
      densevector_raw_data($0).flatMap{x => densevector_raw_data(x)}
    }

    // --- DenseVector File Read
    // This is different than the other file reads to maintain the format used in the previous version of OptiLA
    // TODO: for fusion and cluster execution, reads should be pure. however, in that case we need a different way to order them with respect to writes / deletes.
    // one solution would be to implicitly convert strings to mutable file objects, and (manually) CSE future conversions to return the original mutable object.
    static (DenseVector) ("fromFile", Nil, ("path", MString) :: DenseVector(MDouble)) implements composite ${
      DenseVector.fromFile($path, "\"\\s+\"", v => parse_string_todouble(v(0)) )
    }
    static (DenseVector) ("fromFile", Nil, (("path", MString), ("delim", MString)) :: DenseVector(MDouble)) implements composite ${ 
      DenseVector.fromFile($path, $delim, v => parse_string_todouble(v(0)) )
    }
    static (DenseVector) ("fromFile", T, (("path", MString), ("schemaBldr", DenseVector(MString) ==> T)) :: DenseVector(T)) implements composite ${
      DenseVector.fromFile($path, "\"\\s+\"", $schemaBldr)
    }
    static (DenseVector) ("fromFile", T, (("path", MString), ("delim", MString), ("schemaBldr", DenseVector(MString) ==> T)) :: DenseVector(T)) implements composite ${
      val data = Array1D.fromFile($path){elem =>
        val tokens = Array1D.splitString(elem.trim, $delim)
        $schemaBldr(densevector_fromarray1d(tokens))
      }
      densevector_fromarray1d(data, true)
    }

    // --- Direct methods
    val Ctr = lookupGrp("Constructors")
    direct (Ctr) ("uniform", Nil, MethodSignature(List(("start", MDouble), ("step_size", MDouble), ("end", MDouble), ("isRow", MBoolean, "unit(true)")), DenseVector(MDouble))) implements composite ${
      fassert($end > $start + $step_size, "end <= start+step_size in uniform vector constructor")
      val length = ceil(($end-$start)/$step_size)
      val data = Array1D.fromFunction(length){i => $step_size*i + $start}
      densevector_fromarray1d(data, $isRow) // fixed bug where the result was always a row vector
    }    

    infix (DenseVector) ("toVector", (T,R), MMap(T, R) :: DenseVector(R)) implements composite ${
      densevector_fromarray1d(fmulmap_values($0), true)  
    }
    
    val DenseVectorOps = withTpe(DenseVector)
    DenseVectorOps {
      // --- Accessors
      infix ("length") (Nil :: MInt) implements composite ${ densevector_raw_data($self).length } 
      infix ("isRow") (Nil :: MBoolean) implements getter(0, "_isRow")
      infix ("isCol") (Nil :: MBoolean) implements composite ${ !self.isRow }

      // --- Single element access
      infix ("apply") (MInt :: T) implements composite ${ densevector_raw_data($self).apply($1) }

      // --- Continuous accesses (view)
      infix ("slice") (IndexVector :: DenseVector(T)) implements composite ${
      	fassert(isRange($1), "Vector slice is not defined for discontinuous IndexVectors")
      	val inds = if (isWild($1)) $self.indices else $1
        densevector_raw_data($self).slice(inds.start, inds.length)
      }

      // --- Discontinuous accesses (copy)
      infix ("dice") (IndexVector :: DenseVector(T)) implements composite ${ 
      	if (isWild($1))        // full vector copy
          $self.Clone                 
      	else if (isRange($1))	 // continuous copy
          $self.slice($1).Clone	
      	else 								   // gather
          densevector_fromfunc($1.length, {i => $self($1(i))}, $self.isRow)
      }

     	// --- Single element update
      infix ("update") ((MInt, T) :: MUnit, effect = write(0)) implements composite ${
        densevector_raw_data($self).update($1, $2)
      }

      // --- Updates
      // guaranteed to be parallel for range IndexVectors
      // potential issues with race conditions for non-range IndexVectors
      // v( Inds(0,1,2,1) ) = (1,2,3,4)
      // User shouldn't expect any specific value for v(1) here, except that it'll be 2 or 4.

      // v(*) = x, v(0::n) = x
      infix ("update") ((IndexVector, T) :: MUnit, effect = write(0)) implements composite ${
        if (isWild($1)) 						// broadcast overwrite
        	$self.mmap(e => $2)	
        else 
          $1.forIndices{i => $self($1(i)) = $2}
      }
      // v(*) = v2, v(0::n) = v2
      infix ("update") ((IndexVector, DenseVector(T)) :: MUnit, effect = write(0)) implements composite ${
        if (isWild($1)) { 				// v(*) = v2
        	fassert($self.length == $2.length, "Dimension mismatch in DenseVector copy (" + $self.length + " != " + $2.length + ")")
        	$self.mzip($2){(a,b) => b}
        }
        else { 
	        fassert($1.length == $2.length, "Dimension mismatch between source length and IndexVector length in DenseVector copy (" + $1.length + " != " + $2.length + ")")
	        $1.forIndices{i => $self($1(i)) = $2(i)}
	      } 
      }
      infix ("copyFrom") ((MInt, DenseVector(T)) :: MUnit, effect = write(0)) implements composite ${
        $self($1::($1 + $2.length)) = $2
      }

      // --- Vector joins
      infix ("<<") (T :: DenseVector(T)) implements composite ${
        val out = DenseVector[T]($self.length + 1, $self.isRow)
        out.copyFrom(0, $self)
        out($self.length) = $1
        out.unsafeImmutable
      }
      infix ("<<") (DenseVector(T) :: DenseVector(T)) implements composite ${
        val out = vector_with_order[T]($self.length + $1.length, $self.isRow)
        out.copyFrom(0, $self)
        out.copyFrom($self.length, $1)
        out.unsafeImmutable
      }

      // --- Inserts/Removals
      noInfixList :::= List("<<=", "<<|=")

      infix ("<<=") (T :: MUnit, effect = write(0)) implements composite ${ $self.append($1) }
      infix ("<<=") (DenseVector(T) :: MUnit, effect = write(0)) implements composite ${ $self.appendAll($1) }

      infix ("append") (T :: MUnit, effect = write(0)) implements composite ${
        densevector_raw_data($self).append($1)
      }
      infix ("insert") ((MInt, T) :: MUnit, effect = write(0)) implements composite ${
        densevector_raw_data($self).insert($1,$2)
      }
      infix ("appendAll") (DenseVector(T) :: MUnit, effect = write(0)) implements composite ${
        densevector_raw_data($self).appendAll(densevector_raw_data($1))
      }
      infix ("insertAll") ((MInt, DenseVector(T)) :: MUnit, effect = write(0)) implements composite ${
        densevector_raw_data($self).insertAll($1, densevector_raw_data($2))
      }
      infix ("remove") (MInt :: MUnit, effect = write(0)) implements composite ${
        densevector_raw_data($self).remove($1)
      }
      infix ("removeAll") ((MInt, MInt) :: MUnit, effect = write(0)) implements composite ${
        densevector_raw_data($self).remove($1, $2)
      }

      // TODO: Fix - should only copy when necessary
      infix ("trim") (Nil :: MUnit, effect = write(0)) implements composite ${
        val data = densevector_raw_data($self)
        val trimmed = data.map{x => x}.mutable
        densevector_set_raw_data($self, trimmed)
      }
      infix ("clear") (Nil :: MUnit, effect = write(0)) implements composite ${
        densevector_set_raw_data($self, Array1D[T](0).unsafeImmutable)
      }

      // --- Ordering
      // TODO: In-place sort for mutable vectors? 

      // TODO: remove one of these sort methods whenever I figure out which one is faster
      // uses original version of .sort with TOrdering(T) restriction
      infix ("oldSort") (Nil :: DenseVector(T), TOrdering(T)) implements composite ${
        val a = densevector_raw_data($self).sort
        densevector_fromarray1d(a, $self.isRow)
      }
      infix ("sort") (Nil :: DenseVector(T), TOrder(T)) implements composite ${
        val data = densevector_raw_data($self).sortWith{(a,b) => implicitly[Order[T]].compare(a,b) }
        densevector_fromarray1d(data, $self.isRow)
      }
      infix ("sortWith") (((T,T) ==> MInt) :: DenseVector(T)) implements composite ${
        val data = densevector_raw_data($self).sortWith{(a,b) => $1(a,b)}
        densevector_fromarray1d(data, $self.isRow)
      } 

      // essentially a sortWith + dice
      infix ("sortWithIndex") (Nil :: CTuple2(DenseVector(T), IndexVector), TOrder(T)) implements composite ${
        val indsRaw = Array1D.sortIndices($self.length){(a,b) => implicitly[Order[T]].compare($self(a), $self(b)) }
        val inds = indexvector_fromarray(inds, $self.isRow)
        ($self(inds), inds)
      }

      // Outputs double, not T. e.g. Vector(1, 2) is still expected to have median of 1.5, not 1
      // TODO: this doesn't work right now on anything but Doubles (in original OptiML either)
      infix ("median") (Nil :: MDouble, (TNumeric(T), TOrder(T))) implements composite ${
        val x = $self.sort
        val mid = x.length / 2
        if (x.length % 2 == 0) {
          (x(mid).AsInstanceOf[Double] + x(mid - 1).AsInstanceOf[Double]) / 2
        }
        else x(mid).AsInstanceOf[Double]
      }
      direct ("median") (Nil :: MDouble, (TNumeric(T), TOrder(T))) implements composite ${ $0.median }


      // --- List-like operations
      // Has the advantage of early stopping, but sequential
      infix ("contains") (T :: MBoolean) implements composite ${ 
        var found = false
        var i = 0
        while (i < $0.length && !found) {
          if ($self(i) == $1) { found = true }
          i += 1
        }
        found
      }
      // contains starting from tail of list
      infix ("tailContains") (T :: MBoolean) implements composite ${
        var found = false
        var i = $self.length
        while (i > 0 && !found) {
          i -= 1
          if ($self(i) == $1) { found = true }
        }
        found
      }
      // Full reduction, probably best for less likely values
      infix ("redContains") (T :: MBoolean) implements composite ${
        $self.map{e => e == $1}.fold(unit(false)){(a,b) => a || b}
      }

      infix ("distinct") (Nil :: DenseVector(T)) implements composite ${
        val set = SHashMap[T, Boolean]()
        val out = DenseVector[T](0, $self.isRow)
        var index = 0
        for (i <- 0 until $self.length) {
          if (!set.contains($self(i))) {
          	set($self(i)) = true
            out <<= $self(i)
          }
        }
        out.unsafeImmutable
   		}

      // --- Comparisons
      // filterMapReduce versus zipReduce - both should be O(N) after fusion?
      // Moved to MultiArray
      /*direct ("__equal") (DenseVector(T) :: MBoolean) implements composite ${
        if ($self.length != $1.length || $self.isRow != $1.isRow) unit(false)
        else {
          $self.zip($1){(a,b) => a == b}.fold(true){(a,b) => a && b}
        }
      }*/
      direct ("__equal") (IndexVector :: MBoolean) implements composite ${
        if (isWild($1)) { unit(false) }
        else if ($self.length != $1.length || $self.isRow != $1.isRow) unit(false)
        else {
          val c = $self.indices.count{i: Rep[Int] => $self(i) != $1(i)}
          (c == 0)
        }
      }
      direct ("__equal") (SparseVector(T) :: MBoolean) implements composite ${ $self == $1.toDense }

      // --- Math
      infix ("*") (DenseVector(T) :: DenseVector(T), TArith(T)) implements composite ${
      	$self.zip($1){(a,b) => a * b}
      }

      // dot product
      infix ("*:*") (DenseVector(T) :: T, TArith(T)) implements composite ${
        fassert($self.length == $1.length, "Dimension mismatch in vector dot product (" + $self.length + " != " + $1.length + ")")
        sum($self * $1)
      }

      // vector outer product
      infix ("**") (DenseVector(T) :: DenseMatrix(T), TArith(T)) implements composite ${
        fassert($self.isCol && $1.isRow, "Dimension mismatch in vector outer product (Expected column * row)")
        ($self.indices, $1.indices) {(i,j) => $self(i) * $1(j) }
      }

      infix ("*") (DenseMatrix(T) :: DenseVector(T), TArith(T)) implements composite ${ 
        fassert($self.isRow, "Dimension mismatch in vector-matrix product (Expected row vector)")
        fassert($self.length == $1.numRows, "Dimension mismatch in vector-matrix product")
        $1.mapColsToVector{col => col *:* $self}
      }
 
      // TODO: Assumes non-empty (at least one element)
      infix ("prefixSum") (Nil :: DenseVector(T), TArith(T)) implements composite ${
        $self.scan(implicitly[Arith[T]].zero($self(0))){(a,b) => implicitly[Arith[T]] + (a,b) }
      }

      infix ("minIndex") (Nil :: MInt, TOrder(T)) implements composite ${
        $self.indices.reduce {(a,b) => if (implicitly[Order[T]].lt($self(a), $self(b))) a else b }
      }
      infix ("maxIndex") (Nil :: MInt, TOrder(T)) implements composite ${
        $self.indices.reduce {(a,b) => if (implicitly[Order[T]].gt($self(a), $self(b))) a else b }
      }

      // --- Permuting / Reshaping
      // 3 Different types of permute: copying, mutating, and viewing
      infix ("t") (Nil :: DenseVector(T)) implements composite ${ 
        densevector_fromarray1d(densevector_raw_data($self).Clone, !$self.isRow) 
      }
      infix ("mt") (Nil :: MUnit, effect = write(0)) implements composite ${
        densevector_set_is_row($self, !$self.isRow)
      }
      infix ("vt") (Nil :: MUnit) implements composite ${
        densevector_fromarray1d(densevector_raw_data($self), !$self.isRow)
      }

      // Copy conversion to DenseMatrix
      infix ("toMat") (Nil :: DenseMatrix(T)) implements composite ${
      	if ($self.isRow) densematrix_fromarray2d(densevector_raw_data($self).reshape($self.length, 1))
        else densematrix_fromarray2d(densevector_raw_data($self).reshape(1, $self.length))
      }

      // --- Vector parallel ops
      infix ("filter") ((T ==> MBoolean) :: DenseVector(T)) implements composite ${ 
        val data = densevector_raw_data($self).filter{x => $1(x)}
        densevector_fromarray1d(data, $self.isRow)
      }

      infix ("flatMap") ((T ==> DenseVector(R)) :: DenseVector(R), addTpePars = R) implements composite ${
        densevector_fromarray1d(densevector_raw_data($self).flatMap{i => densevector_raw_data($1(i))})
      }

      // --- DenseVector File Writing
      infix ("writeFile") (("path", MString) :: MUnit, TStringable(T), effect = simple) implements composite ${
        $self.writeFile($path, e => e.makStr)
      }
      infix ("writeFile") ( (("path", MString), ("bldr", T ==> MString)) :: MUnit, effect = simple) implements composite ${
        densevector_raw_data($self).writeFile($path)($bldr) 
      }

    } /* End of DenseVectorOps */

    label(lookupOverloaded("DenseVector","*",1), "densevector_vecmatmult")
    
    CastHelp.pairs{ (A, B, R) => 
    	infix (DenseVector) ("*", Nil, (DenseVector(A), DenseVector(B)) :: DenseVector(R)) implements redirect {"densevector_mul[" + R.name + "]" + CastHelp.casting(A,B) }
      infix (DenseVector) ("*", Nil, (DenseVector(A), DenseMatrix(B)) :: DenseVector(R)) implements redirect {"densevector_vecmatmult[" + R.name + "]" + CastHelp.casting(A,B) }
      infix (DenseVector) ("*:*", Nil, (DenseVector(A), DenseVector(B)) :: R) implements redirect {"densevector_mulclnmul[" + R.name + "]" + CastHelp.casting(A,B) }
      infix (DenseVector) ("**", Nil, (DenseVector(A), DenseVector(B)) :: DenseMatrix(R)) implements redirect {"densevector_mulmul[" + R.name + "]" + CastHelp.casting(A,B) }
    }

    val wrapper1D: String => OpType = data => composite ${ densevector_fromarray1d{\$data.as1D} }

    compiler (DenseVector) ("raw_data", T, DenseVector(T) :: MArray1D(T)) implements composite ${ densevector_raw_data($self) }
    addMultiArrayCommonOps(DenseVector, 1, "mul", wrapper1D)
    addVectorCommonOps(DenseVector, T)

  }
}
package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait DenseMatrixOps {
  this: OptiLADSL =>

  def importDenseMatrixOps() {
    val IndexVector = lookupTpe("IndexVector")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val Tuple2 = lookupTpe("Tup2")	// overrides Scala's Tuple2
    val R = tpePar("R")
    val A = tpePar("A")

    // --- Data fields
    data(DenseMatrix, ("_data", MArray2D(A)))
    compiler (DenseMatrix) ("densematrix_raw_data", A, DenseMatrix(A) :: MArray2D(A)) implements getter(0, "_data")
    compiler (DenseMatrix) ("densematrix_set_raw_data", A, (DenseMatrix(A), MArray2D(A)) :: MUnit) implements setter(0, "_data", ${$1})

    // --- Matrix Constructors
    compiler (DenseMatrix) ("densematrix_fromarray2d", A, MArray2D(A) :: DenseMatrix(A)) implements allocates(DenseMatrix, ${$0})
    compiler (DenseMatrix) ("densematrix_fromfunc", A, (MInt, MInt, (MInt,MInt) ==> A) :: DenseMatrix(A)) implements composite ${
      val data = Array2D.fromFunction($0, $1){(i,j) => $2(i,j)}
      densematrix_fromarray2d(data)
    }

    static (DenseMatrix) ("apply", A, (MInt, MInt) :: DenseMatrix(A), effect = mutable) implements composite ${ 
      densematrix_fromarray2d(Array2D[A]($0, $1))
    }
    static (DenseMatrix) ("flattenRows", A, DenseVector(DenseVector(A)) :: DenseMatrix(A)) implements composite ${
      (0::$0.length, 0::$0(0).length) {(i,j) => $0(i).apply(j) }
    }
    static (DenseMatrix) ("flattenCols", A, DenseVector(DenseVector(A)) :: DenseMatrix(A)) implements composite ${
      (0::$0(0).length, 0::$0.length) {(i,j) => $0(j).apply(i) } 
    }

    // Matrix from vector of vectors
    // TODO: Old version doesn't have this sensitive to orientation of outer vector
    static (DenseMatrix) ("apply", A, DenseVector(DenseVector(A)) :: DenseMatrix(A)) implements composite ${
      (0::$0.length, 0::$0(0).length) {(i,j) => $0(i).apply(j) }
    }

    // Matrix from variable number of vectors (rows)
    static (DenseMatrix) ("apply", A, varArgs(DenseVector(A)) :: DenseMatrix(A)) implements composite ${
      val numRows = $0.length
      val numCols = $0.map{e => e.length}.max
      
      // Need preallocation since vectors are not guaranteed to be all same size
      val out = DenseMatrix[A](numRows, numCols)
      // outer foreach should unroll
      for (i: Int <- scala.collection.immutable.Range(0, $0.length)) {
      	out(i, *) = $0(i)
      }
      out.unsafeImmutable
    } 

    // Other matrix constructors
    direct (DenseMatrix) ("block", A, varArgs(DenseVector(DenseMatrix(A))) :: DenseMatrix(A)) implements composite ${
      var totalRows = 0
      var totalCols = 0
      val mats = array_fromseq($0)
      val numargs = array_length(mats)
      for (i <- 0 until numargs) {
        val subMatRow = array_apply(mats, i)
        val numRows = subMatRow(0).numRows
        var numCols = subMatRow(0).numCols
        for (j <- unit(1) until subMatRow.length) {
          fassert(subMatRow(j).numRows == numRows, "Dimension mismatch: block matrix constructor: " + subMatRow(j).numRows + " != " + numRows)
          numCols += subMatRow(j).numCols
        }
        if (i == 0) { totalCols += numCols }
        else { fassert(numCols == totalCols, "Dimension mismatch: block matrix constructor: row " + i + " has wrong number of cols " + numCols + "(expected " + totalCols + ")")}
        totalRows += numRows
      }

      // write blocks
      val out = DenseMatrix[A](totalRows, totalCols)
      var row = 0
      var col = 0
      for (i <- 0 until numargs) {
        val subMatRow = array_apply(mats, i)
        col = 0
        for (j <- 0 until subMatRow.length) {
          val subMat = subMatRow(j)
          out(row::row+subMat.numRows, col::col+subMat.numCols) = subMat
          col += subMat.numCols
        }
        row += subMatRow(0).numRows
      }

      out.unsafeImmutable
    }

    // Creates a square diagonal matrix with the elements of the given vector on the matrix's diagonal
    // TODO: generalize to the kth diagonal
    direct (DenseMatrix) ("diag", A, DenseVector(A) :: DenseMatrix(A), TArith(A)) implements composite ${ 
      val arith = implicitly[Arith[A]]
      densematrix_fromfunc($0.length, $0.length, {(i,j) => 
        if (i == j) $0(i) else arith.empty
      })
    }
    direct (DenseMatrix) ("identity", Nil, (MInt, MInt) :: DenseMatrix(MDouble)) implements composite ${ 
      densematrix_fromfunc($0, $1, {(i,j) => 
        if (i == j) 1.0
        else 0.0
      })
    }
    direct (DenseMatrix) ("identity", Nil, MInt :: DenseMatrix(A)) implements redirect ${ identity($0, $0) }

    direct (DenseMatrix) ("diag", A, DenseMatrix(A) :: DenseVector(A)) implements redirect ${ $0.diag }
    direct (DenseMatrix) ("triu", A, DenseMatrix(A) :: DenseMatrix(A), TArith(A)) implements redirect ${ $0.triu }
    direct (DenseMatrix) ("tril", A, DenseMatrix(A) :: DenseMatrix(A), TArith(A)) implements redirect ${ $0.tril }


    val DenseMatrixOps = withTpe(DenseMatrix)
    DenseMatrixOps {
      // --- Miscellaneous infix operations
      // TODO: change this to a matrix view? technically a strided access here (stride = nCols + 1)
      // TODO: generalize to the kth diagonal
      infix ("diag") (Nil :: DenseVector(A)) implements composite ${
        val length = min($self.numRows, $self.numCols)
        (0::length) {i => $self(i, i) }
      }
      infix ("triu") (Nil :: DenseMatrix(A), TArith(A)) implements composite ${ 
        densematrix_fromfunc($self.numRows, $self.numCols, {(i,j) => 
          if (i <= j) $self(i,j) else implicitly[Arith[A]].empty
        })
      }
      infix ("tril") (Nil :: DenseMatrix(A), TArith(A)) implements composite ${ 
        densematrix_fromfunc($self.numRows, $self.numCols, {(i,j) => 
          if (i >= j) $self(i,j) else implicitly[Arith[A]].empty
        })
      }

      infix ("replicate") ((MInt, MInt) :: DenseMatrix(A)) implements composite ${
        densematrix_fromfunc($self.numRows*$1, $self.numCols*$2, {(i,j) => 
          $self(i % $self.numRows, j % $self.numCols)
        })
      }

      // --- Single element access
      // Moved to TensorCommonOps
      //infix ("apply") ((MInt, MInt) :: A) implements composite ${ densematrix_raw_data($self).apply($1, $2) }

      // --- Continuous slice
      // orientation of IndexVector in apply does not matter - use getCols or 2d apply to slice cols. This is so we can use n::m syntax
      // to slice rows, while still retaining our convention of row vectors being the default (e.g. for matrix construction)
      
      // Moved to TensorCommonOps
      /*infix ("slice") ((IndexVector, IndexVector) :: DenseMatrix(A)) implements composite ${
        fassert(isRange($1) && isRange($2), "DenseMatrix slice is not defined on discontinuous IndexVectors")
        val rInds = if (isWild($1)) $self.rowIndices else $1
        val cInds = if (isWild($2)) $self.colIndices else $2
        densematrix_fromarray2d(densematrix_raw_data($self).slice(rInds.start, rInds.length, cInds.start, cInds.length))
      }*/
    
      // Aliases for continuous slices
      // TODO: should we have a copy version of getRow and getCol as well (i.e. change these to slice and add a dice?)
      infix ("slice") ((MInt, MInt, MInt, MInt) :: DenseMatrix(A)) implements redirect ${ $self.slice($1::$2,$3::$4) }
      
      infix ("getRow") (MInt :: DenseVector(A)) implements composite ${ 
        densevector_fromarray1d(densematrix_raw_data($0).sliceRow($1), true)
      }
      infix ("getCol") (MInt :: DenseVector(A)) implements composite ${ 
        densevector_fromarray1d(densematrix_raw_data($0).sliceCol($1), false)
      }
      infix ("apply") (MInt :: DenseVector(A)) implements composite ${ $self.getRow($1) }
      infix ("sliceRows") (IndexVector :: DenseMatrix(A)) implements composite ${ $self.slice($1, *) }
      infix ("sliceCols") (IndexVector :: DenseMatrix(A)) implements composite ${ $self.slice(*, $1) }
      infix ("sliceRows") ((("start",MInt),("end",MInt)) :: DenseMatrix(A)) implements composite ${ $self.slice($start::$end, *) }
      infix ("sliceCols") ((("start",MInt),("end",MInt)) :: DenseMatrix(A)) implements composite ${ $self.slice(*, $start::$end) }

      // --- Discontinuous dice
      // TODO: Row-wise and column-wise dices still look like full gathering operations
      // Mark diceRows and diceCols as row/column based accesses.. but coding that explicitly 
      // requires mutations, which means no fusion...
      
      // Moved to TensorCommonOps
      /*infix ("dice") ((IndexVector, IndexVector) :: DenseMatrix(A)) implements composite ${
      	if (isWild($1) && isWild($2)) $self.Clone
      	else if (isRange($1) && isRange($2)) $self.slice($1, $2).Clone
      	else {
      		val rInds = if (isWild($1)) $self.rowIndices else $1
      		val cInds = if (isWild($2)) $self.colIndices else $2
      		(rInds, cInds) { (i,j) => $self(i,j) }
      	}
      }*/
      // Aliases for discontinuous accesses
      // TODO: May be able to do these as some form of flatMap on IndexVector
      infix ("diceRows") (IndexVector :: DenseMatrix(A)) implements composite ${ $self.dice($1, *) }
      infix ("diceCols") (IndexVector :: DenseMatrix(A)) implements composite ${ $self.dice(*, $1) }

      // --- Ambiguous methods
      // Moved to TensorCommonOps
      /*infix ("apply") ((IndexVector, IndexVector) :: DenseMatrix(A)) implements composite ${ 
        if (isRange($1) && isRange($2)) $self.slice($1, $2)
        else $self.dice($1, $2) 
      }*/
      infix ("apply") (IndexVector :: DenseMatrix(A)) implements composite ${ 
      	if (isRange($1)) $self.slice($1, *)
      	else $self.dice($1, *) 
      }

      // e.g. m(1, 0::3)
      infix ("apply") ((MInt, IndexVector) :: DenseVector(A)) implements composite ${
        if (isWild($2)) $self.getRow($1)
        else if (isRange($2)) $self.getRow($1).slice($2)
        else $self.getRow($1).dice($2)
      }
      infix ("apply") ((IndexVector, MInt) :: DenseVector(A)) implements composite ${
        if (isWild($1)) $self.getCol($2)
        else if (isRange($1)) $self.getCol($2).slice($1)
        else $self.getCol($2).dice($1)
      }

      // --- Updates
      // Moved to TensorCommonOps
      /*infix ("update") ((MInt, MInt, A) :: MUnit, effect = write(0)) implements composite ${
        densematrix_raw_data($self).update($1, $2, $3)
      }
      
      // Should still be able to figure out windowed update vs. broadcast from here
			infix ("update") ((IndexVector, IndexVector, A) :: MUnit, effect = write(0)) implements composite ${
      	if (isWild($1) && isWild($2))  $self.mmap{e => $3}
      	else {  // Can't use slice.mmap - slices are immutable
          val rInds = if (isWild($1)) $self.rowIndices else $1
          val cInds = if (isWild($2)) $self.colIndices else $2
	        rInds.foreach{i => cInds.foreach{j => $self(i,j) = $3 }}
	      }
      }

      infix ("update") ((IndexVector, IndexVector, DenseMatrix(A)) :: MUnit, effect = write(0)) implements composite ${
        if (isWild($1) && isWild($2)) { 				// Full matrix copy
          fassert($self.numRows == $3.numRows && $self.numCols == $3.numCols, "Dimension mismatch in DenseMatrix copy: (" + $self.numRows + "x" + $self.numCols + ") != (" + $3.numRows + "x" + $3.numCols + ")")
          $self.mzip($3){(a,b) => b}
        }
        else {
          val rInds = if (isWild($1)) $self.rowIndices else $1
          val cInds = if (isWild($2)) $self.colIndices else $2
          rInds.forIndices{i => cInds.forIndices{j => $self(rInds(i),cInds(j)) = $3(i,j) }}
        }
      }*/
			
			// aliases for updates
      // multi row/column updates
			infix ("update") ((IndexVector, DenseMatrix(A)) :: MUnit, effect = write(0)) implements composite ${ $self($1, *) = $2 }
      infix ("updateRows") ((IndexVector, DenseMatrix(A)) :: MUnit, effect = write(0)) implements composite ${ $self($1, *) = $2 }
      infix ("updateCols") ((IndexVector, DenseMatrix(A)) :: MUnit, effect = write(0)) implements composite ${ $self(*, $1) = $2 }
     	
      // full row/column updates
      infix ("updateRow") ((MInt, DenseVector(A)) :: MUnit, effect = write(0)) implements composite ${ 
        (0::$2.length) foreach {j => $self($1, j) = $2(j) }
      }
      infix ("updateCol") ((MInt, DenseVector(A)) :: MUnit, effect = write(0)) implements composite ${ 
        (0::$2.length) foreach {i => $self(i, $1) = $2(i) }
      }
			infix ("update") ((MInt, DenseVector(A)) :: MUnit, effect = write(0)) implements composite ${ $self.updateRow($1, $2) }
      
      // partial row/column updates
      infix ("update") ((MInt, IndexVector, DenseVector(A)) :: MUnit, effect = write(0)) implements composite ${
        val cInds = if (isWild($2)) $self.colIndices else $2
        cInds.forIndices{j => $self($1, cInds(j)) = $3(j) }
      }
      infix ("update") ((IndexVector, MInt, DenseVector(A)) :: MUnit, effect = write(0)) implements composite ${
        val rInds = if (isWild($1)) $self.colIndices else $1
        rInds.forIndices{i => $self(rInds(i), $2) = $3(i) }
      }     

      
      // --- Matrix Joins
      // insert row (at end)
      infix ("<<") (DenseVector(A) :: DenseMatrix(A)) implements composite ${
        fassert($1.length == $self.numCols, "Dimension mismatch between DenseVector length and DenseMatrix columns in vertical Matrix-Vector join (" + $1.length + " != " + $self.numCols + ")")
        val out = DenseMatrix[A]($self.numRows + 1, $self.numCols)
        out($self.rowIndices, *) = $self
        out($self.numRows) = $1
        out.unsafeImmutable
      }

      // vertically join matrices
      infix ("<<") (DenseMatrix(A) :: DenseMatrix(A)) implements composite ${
        fassert($1.numCols == $self.numCols, "Dimension mismatch in vertical Matrix-Matrix join (" + $self.numCols + " != " + $1.numCols + ")")
        val out = DenseMatrix[A]($self.numRows + $1.numRows, $self.numCols)
        out($self.rowIndices, *) = $self
        out($self.numRows :: ($self.numRows + $1.numRows), *) = $1
        out.unsafeImmutable
      }

      infix ("<<|") (DenseVector(A) :: DenseMatrix(A)) implements composite ${
        fassert($1.length == $self.numRows, "Dimension mismatch between DenseVector length and DenseMatrix rows in horizontal Matrix-Vector join (" + $1.length + " != " + $self.numRows + ")")
        val out = DenseMatrix[A]($self.numRows, $self.numCols + 1)
        out(*, $self.colIndices) = $self
        out(*, $self.numCols) = $1
        out.unsafeImmutable
      }

      // horizontally join matrices
      infix ("<<|") (DenseMatrix(A) :: DenseMatrix(A)) implements composite ${
        fassert($1.numRows == $self.numRows, "Dimension mismatch in horizontal Matrix-Matrix join (" + $self.numRows + " != " + $1.numRows + ")")
        val out = DenseMatrix[A]($self.numRows, $self.numCols + $1.numCols)
        out(*, $self.colIndices) = $self
        out(*, $self.numCols :: ($self.numCols + $1.numCols)) = $1
        out.unsafeImmutable
      }

      // --- Inserts / Removals
      infix ("<<=") (DenseVector(A) :: MUnit, effect = write(0)) implements composite ${ $self.insertRow($self.numRows, $1) }
      infix ("<<=") (DenseMatrix(A) :: MUnit, effect = write(0)) implements composite ${ $self.insertRows($self.numRows, $1) }
      infix ("<<|=") (DenseVector(A) :: MUnit, effect = write(0)) implements composite ${ $self.insertCol($self.numCols, $1) }
      infix ("<<|=") (DenseMatrix(A) :: MUnit, effect = write(0)) implements composite ${ $self.insertCols($self.numCols, $1) }
      infix ("insertAllRows") ((MInt, DenseMatrix(A)) :: MUnit, effect = write(0)) implements composite ${ $self.insertRows($1, $2) }
      infix ("insertAllCols") ((MInt, DenseMatrix(A)) :: MUnit, effect = write(0)) implements composite ${ $self.insertCols($1, $2) }

      infix ("insertRow") ((MInt, DenseVector(A)) :: MUnit, effect = write(0)) implements composite ${
        densematrix_raw_data($self).insertRow($1, densevector_raw_data($2))
      }
      infix ("insertCol") ((MInt, DenseVector(A)) :: MUnit, effect = write(0)) implements composite ${
        densematrix_raw_data($self).insertCol($1, densevector_raw_data($2))
      }
      infix ("insertRows") ((MInt, DenseMatrix(A)) :: MUnit, effect = write(0)) implements composite ${
        densematrix_raw_data($self).insertRows($1, densevector_raw_data($2))
      }
      infix ("insertCols") ((MInt, DenseMatrix(A)) :: MUnit, effect = write(0)) implements composite ${
        densematrix_raw_data($self).insertCols($1, densevector_raw_data($2))
      }

      // TODO: Fix trim method - should only copy if necessary
      // TODO: Add trim method to MultiArray support?
      infix ("trim") (Nil :: MUnit, effect = write(0)) implements composite ${
        val data = densematrix_raw_data($self)
        val trimmed = data.map{x => x}.mutable
        densematrix_set_raw_data($self, trimmed)
      }
      infix ("clear") (Nil :: MUnit, effect = write(0)) implements composite ${
        fwarn(unit(false), "User, y u do dis? User stahp")
        densematrix_set_raw_data($self, Array2D[A](0,0).unsafeImmutable)
      }

      infix ("removeRow") (MInt :: MUnit, effect = write(0)) implements composite ${
        densematrix_raw_data($self).removeRow($1)
      }
      infix ("removeCol") (MInt :: MUnit, effect = write(0)) implements composite ${
        densematrix_raw_data($self).removeCol($1)
      }
      infix ("removeRows") ((MInt, MInt) :: MUnit, effect = write(0)) implements composite ${
        densematrix_raw_data($self).removeRows($1, $2)
      }
      infix ("removeCols") ((MInt, MInt) :: MUnit, effect = write(0)) implements composite ${
        densematrix_raw_data($self).removeCols($1, $2)
      }

      infix ("removeRows") (IndexVector :: MUnit, effect = write(0)) implements composite ${
        if (isWild($1)) $self.clear
        else if (isRange($1)) $self.removeRows($1.start, $1.length)
        else {
          for (i <- unit(0) until $1.length) { $self.removeRow($1(i)) }
        }
      }
      infix ("removeCols") (IndexVector :: MUnit, effect = write(0)) implements composite ${
        if (isWild($1)) $self.clear
        else if (isRange($1)) $self.removeCols($1.start, $1.length)
        else {
          for (i <- unit(0) until $1.length) { $self.removeCol($1(i)) }
        }
      }

      // --- Math
      // element-wise multiply
      infix ("*:*") (DenseMatrix(A) :: DenseMatrix(A), TArith(A)) implements composite ${
        $0.zip($1){(a,b) => implicitly[Arith[A]] * (a,b) }
      }

      // Matrix multiply
      infix ("*") (DenseMatrix(A) :: DenseMatrix(A), TArith(A)) implements composite ${
        fassert($0.numCols == $1.numRows, "Dimension mismatch in matrix multiply (" + $0.numCols + " != " + $1.numRows + ")")
        val lhs = densematrix_raw_data($0)
        val rhs = densematrix_raw_data($1)    
        densematrix_fromarray2d(fmultia_matmult(lhs, rhs))
      }

      // Matrix-Vector multiply
      infix ("*") (DenseVector(A) :: DenseVector(A), TArith(A)) implements composite ${
        fassert($1.isCol, "Dimension mismatch in matrix-vector product (Expected column vector)")
        fassert($0.numCols == $1.length, "Dimension mismatch in matrix-vector product (" + $self.numCols + " != " + $1.length + ")")
        val lhs = densematrix_raw_data($0)
        val rhs = densevector_raw_data($1)
        densevector_fromarray1d(fmultia_matvecmult(lhs, rhs), unit(false))
      }

      // --- Ordering
      // Possible alternatives for parallel implementations of min/maxIndex..

      // minIndex as zipTupleReduce
      // pros: should fuse, sort of extensible to ND, layout independent (?)
      // cons: kinda gross looking
      infix ("minIndexX1") (Nil :: Tuple2(MInt, MInt), TOrder(A)) implements composite ${
        val inds2D = Array2D.fromFunction($self.numRows, $self.numCols){(i,j) => pack((i,j)) }
        densematrix_raw_data($self).zip(inds2D){(a,b) => pack((a,b)) }.reduce{ (a,b) =>
          if (implicitly[Order[A]].lt(a._1, b._1)) a._2 else b._2 
        }
      }
      infix ("maxIndexX1") (Nil :: Tuple2(MInt, MInt), TOrder(A)) implements composite ${
        val inds2D = Array2D.fromFunction($self.numRows, $self.numCols){(i,j) => pack((i,j)) }
        densematrix_raw_data($self).zip(inds2D){(a,b) => pack((a,b)) }.reduce{ (a,b) =>
          if (implicitly[Order[A]].gt(a._1, b._1)) a._2 else b._2 
        }
      }
      // minIndex as 1D mapReduce with following unflatten 
      // pros: copy should fuse out(?), easily extensible up to 6D
      // cons: may have performance hits for non-flat layouts?
      infix ("minIndexX2") (Nil :: Tuple2(MInt, MInt), TOrder(A)) implements composite ${
        val flatIndex = $self.reshape($self.size).minIndex
        unflatten(flatIndex, pack(($self.numRows, $self.numCols)))
      }
      infix ("maxIndexX2") (Nil :: Tuple2(MInt, MInt), TOrder(A)) implements composite ${
        val flatIndex = $self.reshape($self.size).maxIndex
        unflatten(flatIndex, pack(($self.numRows, $self.numCols)))
      }

      infix ("minIndex") (Nil :: Tuple2(MInt, MInt), TOrder(A)) implements composite ${
        var min = $self(0, 0)
        var minRow = 0
        var minCol = 0
        val i = 0
        val j = 0
        for (i <- 0 until $self.numRows) {
          for (j <- 0 until $self.numCols) {
            if (implicitly[Order[A]].lt($self(i, j), min)) {
              min = $self(i, j)
              minRow = i
              minCol = j
            }
          }
        }  
        pack((readVar(minRow), readVar(minCol)))
      }
      infix ("maxIndex") (Nil :: Tuple2(MInt, MInt), TOrder(A)) implements composite ${
        var max = $self(0, 0)
        var maxRow = 0
        var maxCol = 0
        val i = 0
        val j = 0
        for (i <- 0 until $self.numRows) {
          for (j <- 0 until $self.numCols) {
            if (implicitly[Order[A]].gt($self(i, j), max)) {
              max = $self(i, j)
              maxRow = i
              maxCol = j
            }
          }
        }
        pack((readVar(maxRow), readVar(maxCol)))
      }

      // --- Permuting
      infix ("t") (Nil :: DenseMatrix(A)) implements composite ${ densematrix_fromarray2d(densematrix_raw_data($self).t) }
      infix ("vt") (Nil :: DenseMatrix(A)) implements composite ${ densematrix_fromarray2d(densematrix_raw_data($self).vt) }

      // --- Reshaping
      infix ("flattenToVector") (MBoolean :: DenseVector(A)) implements composite ${
        densevector_fromarray1d(densematrix_raw_data($self).reshape($self.size), $1)
      }
      infix ("flattenToVector") (Nil :: DenseVector(A)) implements composite ${ $self.flattenToVector(true) }


      // --- Row/Column-wise ops
      infix ("sumRows") (Nil :: DenseVector(A), TArith(A)) implements composite ${ $self.mapRowsToVector{row => sum(row) }}
      infix ("sumCols") (Nil :: DenseVector(A), TArith(A)) implements composite ${ $self.mapColsToVector{col => sum(col) }}
      infix ("minRows") (Nil :: DenseVector(A), TOrder(A)) implements composite ${ $self.mapRowsToVector{row => min(row) }}
      infix ("minCols") (Nil :: DenseVector(A), TOrder(A)) implements composite ${ $self.mapColsToVector{col => min(col) }}
      infix ("maxRows") (Nil :: DenseVector(A), TOrder(A)) implements composite ${ $self.mapRowsToVector{row => max(row) }}
      infix ("maxCols") (Nil :: DenseVector(A), TOrder(A)) implements composite ${ $self.mapColsToVector{col => max(col) }}

      infix ("mapRowsToVector") ((DenseVector(A) ==> R) :: DenseVector(R), addTpePars = R) implements composite ${
        $self.rowIndices.map{i => $1( $self.getRow(i) )}
      }
      infix ("mapColsToVector") ((DenseVector(A) ==> R) :: DenseVector(R), addTpePars = R) implements composite ${
        $self.colIndices.map{j => $1( $self.getCol(j) )}
      }

      infix ("findRows") ((DenseVector(A) ==> MBoolean) :: IndexVector) implements composite ${
        $self.rowIndices.filter{i => $1( $self(i, *) )}
      }
      infix ("findCols") ((DenseVector(A) ==> MBoolean) :: IndexVector) implements composite ${
        $self.colIndices.filter{j => $1( $self(*, j) )}
      }

      infix ("filterRows") ((DenseVector(A) ==> MBoolean) :: DenseMatrix(A)) implements composite ${
        $self($self.findRows($1))
      }
      infix ("filterCols") ((DenseVector(A) ==> MBoolean) :: DenseMatrix(A)) implements composite ${
        $self(*, $self.findCols($1))
      }

      // row/col foreach
      infix ("foreachRow") ((DenseVector(A) ==> MUnit) :: MUnit, effect = simple) implements composite ${
        $self.rowIndices foreach {i => $1($self(i, *)) }
      }
      infix ("foreachCol") ((DenseVector(A) ==> MUnit) :: MUnit, effect = simple) implements composite ${
        $self.colIndices foreach {j => $1($self(*, j))}
      }

      // size preserving maps
      // The wrapping/unwrapping to apply the lambda function is unfortunate, but is hidden from the user
      infix ("mapRows") ((DenseVector(A) ==> DenseVector(R)) :: DenseMatrix(R), addTpePars = R) implements composite ${
        val data = densematrix_raw_data($self).mapRows{row => densevector_raw_data($1(densevector_fromarray1d(row, true))) }
        densematrix_fromarray2d(data)
      }
      infix ("mapCols") ((DenseVector(A) ==> DenseVector(R)) :: DenseMatrix(R), addTpePars = R) implements composite ${
        val data = densematrix_raw_data($self).mapCols{col => densevector_raw_data($1(densevector_fromarray1d(col, false))) }
        densematrix_fromarray2d(data)
      }

      // row/column views, so the fusion should definitely happen here without any data copying
      // TODO: There may still be another, better way to express this
      infix ("reduceRows") (((DenseVector(A), DenseVector(A)) ==> DenseVector(A)) :: DenseVector(A), TArith(A)) implements composite ${
        $self.rowIndices.map{i => $self.getRow(i)}.reduce{(a,b) => $1(a,b)}
      }
      infix ("reduceCols") (((DenseVector(A), DenseVector(A)) ==> DenseVector(A)) :: DenseVector(A), TArith(A)) implements composite ${
        $self.colIndices.map{i => $self.getCol(i)}.reduce{(a,b) => $1(a,b)}
      }

      infix ("groupRowsBy") ((DenseVector(A) ==> K) :: MMap(K, DenseMatrix(A)), addTpePars = K) implements composite ${
        val grps = $self.rowIndices.groupBy(i => $1($self.getRow(i)), i => $self.getRow(i))
        val vals = fmulmap_values(grps)
        val submats = vals.map(rows => DenseMatrix.flattenRows(rows))
        fmulmap_from_1d_arrays(fmulmap_keys(grps), submats)
      }

      infix ("groupColsBy") ((DenseVector(A) ==> K) :: MMap(K, DenseMatrix(A)), addTpePars = K) implements composite ${
        val grps = $self.colIndices.groupBy(i => $1($self.getCol(i)), i => $self.getCol(i))
        val vals = fmulmap_values(grps)
        val submats = vals.map(cols => DenseMatrix.flattenCols(cols))
        fmulmap_from_1d_arrays(fmulmap_keys(grps), submats)
      }
    } /* End DenseMatrixOps */

    // -- Add MultiArray ops
    val wrapper2D: String => OpType = data => composite ${ densematrix_fromarray2d{\$data.as2D} }

    compiler (DenseMatrix) ("raw_data", A, DenseMatrix(A) :: MArray2D(A)) implements composite ${ densematrix_raw_data($0) }
    addMultiArrayCommonOps(DenseMatrix, 2, "mulclnmul", wrapper2D)

    // Type casting for element-wise, matrix-matrix, and matrix-vector products
    CastHelp.pairs{ (A, B, R) => 
  		infix (DenseMatrix) ("*", Nil, (DenseMatrix(A), DenseMatrix(B)) :: DenseMatrix(R)) implements redirect {"densematrix_matmult[" + R.name + "]" + CastHelp.casting(A,B)}
  		infix (DenseMatrix) ("*", Nil, (DenseMatrix(A), DenseVector(B)) :: DenseVector(R)) implements redirect {"densematrix_matvecmult[" + R.name + "]" + CastHelp.casting(A,B)}
  		infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(A), DenseMatrix(B)) :: DenseMatrix(R)) implements redirect {"densematrix_mulclnmul[" + R.name + "]" + CastHelp.casting(A,B)}
  	}
  }

}
package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait MatrixOps {
  this: OptiLADSL =>

  /**
   * This interface represents a convenient set of matrix accessor functions.
   * They require m to define numRows, numCols, size, getRow, getCol, slice and apply,
   * and m must be a ParallelCollection (which is checked at Forge stage-time).
   */
  def addMatrixCommonOps(m: Rep[DSLType], T: Rep[DSLType]) {
    val IndexVector = lookupTpe("IndexVector")
    val IndexWildcard = lookupTpe("IndexWildcard", stage = compile)
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val DenseMatrixView = lookupTpe("DenseMatrixView")
    val SparseMatrix = lookupTpe("SparseMatrix")
    val SparseVector = lookupTpe("SparseVector")
    val SparseVectorView = lookupTpe("SparseVectorView")
    val Tuple2 = lookupTpe("Tup2")
    val B = tpePar("B")
    val R = tpePar("R")

    // have to be careful about the type argument name we use in single and composite since T is being passed in
    // We splice this name into blocks using the escaped \$ to inform the preprocessor that the value already exists.
    val TT = T.name

    // we can also perform bulk operations generically, returning a DenseVector result for each operation
    // Arith is only required if T is actually a tpePar here, so we need to be careful.
    if (!isTpePar(T)) compiler (m) ("zeroT", Nil, Nil :: T) implements composite ${ 0.asInstanceOf[\$TT] }

    val AZ = if (isTpePar(T)) (List(TArith(asTpePar(T))), "implicitly[Arith[T]].empty") else (Nil, "zeroT")
    val A = AZ._1; val Z = AZ._2; // can't use capital letters with tuple return pattern matching
    val O = if (isTpePar(T)) List(TOrdering(asTpePar(T))) else Nil
    val S = if (isTpePar(T)) List(TStringable(asTpePar(T))) else Nil
    val M = if (isTpePar(T)) m(T) else m

    val MatrixCommonOps = withTpe(m)
    MatrixCommonOps {
      /**
       * Conversions
       */
      infix ("toBoolean") (Nil :: DenseMatrix(MBoolean), ("conv",T ==> MBoolean)) implements composite ${ $self.map(conv) }
      infix ("toDouble") (Nil :: DenseMatrix(MDouble), ("conv",T ==> MDouble)) implements composite ${ $self.map(conv) }
      infix ("toFloat") (Nil :: DenseMatrix(MFloat), ("conv",T ==> MFloat)) implements composite ${ $self.map(conv) }
      infix ("toInt") (Nil :: DenseMatrix(MInt), ("conv",T ==> MInt)) implements composite ${ $self.map(conv) }

      /**
       * Accessors
       */
      infix ("apply") (MInt :: DenseVectorView(T)) implements redirect ${ $self.getRow($1) }

      // orientation of IndexVector in apply does not matter - use getCols or 2d apply to slice cols. This is so we can use n::m syntax
      // to slice rows, while still retaining our convention of row vectors being the default (e.g. for matrix construction).
      // TODO: if the IndexVector is continuous, we should slice (and return a view) instead of copy.
      infix ("apply") (IndexVector :: DenseMatrix(T)) implements redirect ${ $self.getRows($1) }
      infix ("apply") ((IndexVector, IndexWildcard) :: DenseMatrix(T)) implements redirect ${ $self.getRows($1) }
      infix ("apply") ((("rows", IndexVector), ("cols", IndexVector)) :: DenseMatrix(T)) implements composite ${
        (rows, cols) { (i,j) => $self(i,j) }
      }
      infix ("apply") ((IndexWildcard, IndexVector) :: DenseMatrix(T)) implements redirect ${ $self.getCols($2) }

      infix ("indices") (Nil :: IndexVector) implements composite ${ IndexVector(0, $self.size) }
      infix ("rowIndices") (Nil :: IndexVector) implements composite ${ IndexVector(0, $self.numRows, false) }
      infix ("colIndices") (Nil :: IndexVector) implements composite ${ IndexVector(0, $self.numCols) }

      infix ("getRows") (IndexVector :: DenseMatrix(T)) implements composite ${
        // ($1, *) { i => $self(i) }
        if ($1.length == 0) densematrix_fromarray[T](array_empty_imm[T](0), 0, $self.numCols)
        else {
          var z = $self // manual guard against code motion
          ($1, *) { i => z(i) }
        }
      }
      infix ("getCols") (IndexVector :: DenseMatrix(T)) implements composite ${
        // (*, $1) { j => $self.getCol(j) }
        if ($1.length == 0) densematrix_fromarray[T](array_empty_imm[T](0), $self.numRows, 0)
        else {
          var z = $self // manual guard against code motion
          (*, $1) { j => z.getCol(j) }
        }

      }

      infix ("sliceRows") ((("start",MInt),("end",MInt)) :: DenseMatrixView(T)) implements composite ${ $self.slice(start, end, 0, $self.numCols) }
      infix ("sliceCols") ((("start",MInt),("end",MInt)) :: DenseMatrixView(T)) implements composite ${ $self.slice(0, $self.numRows, start, end) }

      /**
       * Miscellaneous
       */
      // $self.toString doesn't work in Delite, since there is no 'self' instance
      infix ("pprint") (Nil :: MUnit, S, effect = simple) implements composite ${ println($self.makeStr + "\\n") }

      infix ("makeDimsStr") (Nil :: MString) implements single ${
        $self.numRows + " x " + $self.numCols
      }

      infix ("makeString") (Nil :: MString, S) implements single ${
        var s = ""
        if ($self == null) {
          s = "null"
        }
        else if ($self.numRows == 0) {
          s = "[ ]"
        }
        else {
          for (i <- 0 until $self.numRows-1) {
            s = s + $self(i).makeStr + "\\n"
          }
          s = s + $self($self.numRows-1).makeStr
        }
        s
      }

      infix ("toString") (Nil :: MString) implements single ${
        var s = ""
        if ($self == null) {
          s = "null"
        }
        else if ($self.numRows == 0) {
          s = "[ ]"
        }
        else {
          for (i <- 0 until $self.numRows-1) {
            s = s + densevectorview_tostring($self(i)) + "\\n"
          }
          s = s + densevectorview_tostring($self($self.numRows-1))
        }
        s
      }

      infix ("t") (Nil :: DenseMatrix(T)) implements composite ${ (0::$self.numCols, 0::$self.numRows) { (i,j) => $self(j, i) } }

      infix ("mutable") (Nil :: DenseMatrix(T), effect = mutable, aliasHint = copies(0)) implements composite ${
         val out = DenseMatrix[\$TT]($self.numRows, $self.numCols)
         for (i <- 0 until $self.numRows) {
           for (j <- 0 until $self.numCols) {
             out(i,j) = $self(i,j)
           }
         }
         out
       }

     infix ("replicate") ((MInt,MInt) :: DenseMatrix(T)) implements composite ${
       val out = DenseMatrix[\$TT]($1*$self.numRows, $2*$self.numCols)
       for (ii <- 0 until $1) {
         for (i <- 0 until $self.numRows) {
           for (jj <- 0 until $2) {
             for (j <- 0 until $self.numCols) {
               out(ii*$self.numRows+i, jj*$self.numCols+j) = $self(i,j)
             }
           }
         }
       }
       out.unsafeImmutable
     }


     /**
      * Math
      */
     // TODO: inverse
     infix ("+") (DenseMatrix(T) :: DenseMatrix(T), A) implements composite ${ $self.zip($1) { (a,b) => a+b } }
     infix ("+") (T :: DenseMatrix(T), A) implements composite ${ $self.map(e => e+$1) }
     // infix ("+") (DenseMatrix(B) :: DenseMatrix(T), (A, B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a+b })

     infix ("-") (DenseMatrix(T) :: DenseMatrix(T), A) implements composite ${ $self.zip($1) { (a,b) => a-b } }
     infix ("-") (T :: DenseMatrix(T), A) implements composite ${ $self.map(e => e-$1) }
     // infix ("-") (DenseMatrix(B) :: DenseMatrix(T), (A, B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a-b })

     infix ("*:*") (DenseMatrix(T) :: DenseMatrix(T), A) implements composite ${ $self.zip($1) { (a,b) => a*b } }
     // infix ("*:*") (DenseMatrix(B) :: DenseMatrix(T), (A, B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a*b })
     infix ("*") (T :: DenseMatrix(T), A) implements composite ${ $self.map(e => e*$1) }

     infix ("*") (DenseMatrix(T) :: DenseMatrix(T), A) implements composite ${
       fassert($self.numCols == $1.numRows, "dimension mismatch: matrix multiply (lhs: " + $self.makeDimsStr + ", rhs: " + $1.makeDimsStr + ")")
       // naive
       if ($self.numRows == 0) DenseMatrix[T]()
       else {
         var z = $self // manual guard against code motion
         (0::z.numRows, *) { i =>
           $1.mapColsToVector { c => z(i) *:* c }
         }
       }
     }

     infix ("*") (SparseMatrix(T) :: DenseMatrix(T), A) implements composite ${
       fassert($self.numCols == $1.numRows, "dimension mismatch: matrix multiply (lhs: " + $self.makeDimsStr + ", rhs: " + $1.makeDimsStr + ")")
       // Compute (rhs.t*lhs.t).t == lhs*rhs to reformulate as sparse*dense, which is supported by sparse BLAS
       ($1.t*$self.t).t
     }

     for (rhs <- List(DenseVector(T), SparseVector(T))) {
       infix ("*") (rhs :: DenseVector(T), A) implements composite ${
         fassert($self.numCols == $1.length && !$1.isRow, "dimension mismatch: matrix * vector")
         val out = (0::$self.numRows) { i => $self(i) *:* $1 }
         out.t
       }
     }

     infix ("/") (DenseMatrix(T) :: DenseMatrix(T), A) implements composite ${ $self.zip($1) { (a,b) => a/b } }
     infix ("/") (T :: DenseMatrix(T), A) implements composite ${ $self.map(e => e/$1) }
     // infix ("/") (DenseMatrix(B) :: DenseMatrix(T), (A, B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a/b })

     // Dense-sparse point-wise math
     infix ("+") (SparseMatrix(T) :: DenseMatrix(T), A) implements composite ${ $self + $1.toDense }
     infix ("-") (SparseMatrix(T) :: DenseMatrix(T), A) implements composite ${ $self - $1.toDense }
     infix ("*:*") (SparseMatrix(T) :: DenseMatrix(T), A) implements composite ${ $self *:* $1.toDense }
     infix ("/") (SparseMatrix(T) :: DenseMatrix(T), A) implements composite ${ $self / $1.toDense }

     infix ("sum") (Nil :: T, A) implements composite ${ self.reduce((a,b) => a+b ) }
     infix ("prod") (Nil :: T, A) implements reduce(T, 0, ${ unit(1.asInstanceOf[\$TT]) }, ${ (a,b) => a*b })
     infix ("mean") (Nil :: MDouble, ("conv",T ==> MDouble)) implements composite ${ $self.map(conv).sum / $self.size }
     infix ("abs") (Nil :: DenseMatrix(T), A) implements composite ${ $self.map(e => e.abs) }
     infix ("exp") (Nil :: DenseMatrix(T), A) implements composite ${ $self.map(e => e.exp) }
     infix ("log") (Nil :: DenseMatrix(T), A) implements composite ${ $self.map(e => e.log) }
     infix ("variance") (Nil :: MDouble, ("conv",T ==> MDouble)) implements composite ${ $self.flattenToVector.variance }
     infix ("stddev") (Nil :: MDouble, ("conv",T ==> MDouble)) implements composite ${ sqrt($0.variance) }

     infix ("sumRows") (Nil :: DenseVector(T), A) implements composite ${ $self.mapRowsToVector { row => sum(row) }}
     infix ("sumCols") (Nil :: DenseVector(T), A) implements composite ${ $self.mapColsToVector { col => sum(col) }}


     /**
      * Ordering
      */
     val H = if (isTpePar(T)) List(THasMinMax(asTpePar(T))) else Nil
     val MinT = if (isTpePar(T)) "implicitly[HasMinMax[T]].min" else "implicitly[HasMinMax["+TT+"]].min"
     val MaxT = if (isTpePar(T)) "implicitly[HasMinMax[T]].max" else "implicitly[HasMinMax["+TT+"]].max"

     infix ("minRows") (Nil :: DenseVector(T), O ::: H) implements composite ${ $self.mapRowsToVector { row => min(row) }}
     infix ("minCols") (Nil :: DenseVector(T), O ::: H) implements composite ${ $self.mapColsToVector { col => min(col) }}
     infix ("maxRows") (Nil :: DenseVector(T), O ::: H) implements composite ${ $self.mapRowsToVector { row => max(row) }}
     infix ("maxCols") (Nil :: DenseVector(T), O ::: H) implements composite ${ $self.mapColsToVector { col => max(col) }}

     infix ("min") (Nil :: T, O ::: H) implements reduce(T, 0, MaxT, ${ (a,b) => if (a < b) a else b })
     infix ("max") (Nil :: T, O ::: H) implements reduce(T, 0, MinT, ${ (a,b) => if (a > b) a else b })

     // TODO: switch to reduce when TupleReduce is generalized
     infix ("minIndex") (Nil :: Tuple2(MInt,MInt), O) implements composite ${
       var min = $self(0,0)
       var minRow = 0
       var minCol = 0
       for (i <- 0 until $self.numRows) {
         for (j <- 0 until $self.numCols) {
           if ($self(i,j) < min) {
             min = $self(i,j)
             minRow = i
             minCol = j
           }
         }
       }
       pack((minRow,minCol))
     }

     infix ("maxIndex") (Nil :: Tuple2(MInt,MInt), O) implements composite ${
       var max = $self(0,0)
       var maxRow = 0
       var maxCol = 0
       for (i <- 0 until $self.numRows) {
         for (j <- 0 until $self.numCols) {
           if ($self(i,j) > max) {
             max = $self(i,j)
             maxRow = i
             maxCol = j
           }
         }
       }
       pack((maxRow,maxCol))
     }

     /**
      * Bulk
      */
      infix ("map") ((T ==> R) :: DenseMatrix(R), addTpePars = R) implements map((T,R), 0, ${ e => $1(e) })
      infix ("reduce") (((T,T) ==> T) :: T, A) implements reduce(T, 0, Z, ${ (a,b) => $1(a,b) })
      infix ("foreach") ((T ==> MUnit) :: MUnit) implements foreach(T, 0, ${ e => $1(e) })
      infix ("zip") (CurriedMethodSignature(List(List(DenseMatrix(B)), List((T,B) ==> R)), DenseMatrix(R)), addTpePars = (B,R)) implements zip((T,B,R), (0,1), ${ (a,b) => $2(a,b) })
      infix ("count") ((T ==> MBoolean) :: MInt) implements mapReduce((T,MInt), 0, ${ e => 1 }, ${ 0 }, ${ (a,b) => a+b }, Some(${ e => $1(e)}))

      infix ("mapRowsToVector") ((DenseVectorView(T) ==> R) :: DenseVector(R), addTpePars = R) implements composite ${
        $self.rowIndices.map(i => $1($self(i)))
      }
      infix ("mapColsToVector") ((DenseVectorView(T) ==> R) :: DenseVector(R), addTpePars = R) implements composite ${
        $self.colIndices.map(i => $1($self.getCol(i)))
      }

      infix ("findRows") ((DenseVectorView(T) ==> MBoolean) :: IndexVector) implements composite ${
        $self.rowIndices.filter(i => $1($self(i)))
      }
      infix ("findCols") ((DenseVectorView(T) ==> MBoolean) :: IndexVector) implements composite ${
        $self.colIndices.filter(i => $1($self.getCol(i)))
      }

      infix ("filterRows") ((DenseVectorView(T) ==> MBoolean) :: DenseMatrix(T)) implements composite ${
        $self($self.findRows($1))
      }
      infix ("filterCols") ((DenseVectorView(T) ==> MBoolean) :: DenseMatrix(T)) implements composite ${
        $self.getCols($self.findCols($1))
      }

      infix ("foreachRow") ((DenseVectorView(T) ==> MUnit) :: MUnit, effect = simple) implements composite ${
        $self.rowIndices foreach { i => $1($self(i)) }
      }
      infix ("foreachCol") ((DenseVectorView(T) ==> MUnit) :: MUnit, effect = simple) implements composite ${
        $self.colIndices foreach { i => $1($self.getCol(i)) }
      }

      infix ("mapRows") ((DenseVectorView(T) ==> DenseVector(R)) :: DenseMatrix(R), addTpePars = R) implements composite ${
        ($self.rowIndices, *) { i =>
          $1($self(i))
        }
      }
      infix ("mapCols") ((DenseVectorView(T) ==> DenseVector(R)) :: DenseMatrix(R), addTpePars = R) implements composite ${
        (*, $self.colIndices) { j =>
          $1($self.getCol(j))
        }
      }

      // in order to express this with the current Delite ops, we have to convert the matrix to a vector of vectors,
      // which is unfortunate. A vector of vectorviews would be somewhat better, but since Delite reduce requires
      // (A,A) => A, we cannot yet express that operation in parallel with converting each vectors.
      // however, the map and reduce here should fuse, eliminating the overhead in the Delite version.
      infix ("reduceRows") (((DenseVector(T),DenseVector(T)) ==> DenseVector(T)) :: DenseVector(T), A) implements composite ${
        val vv = $self.rowIndices.map(i => $self(i).toDense)
        vv.reduce((a,b) => $1(a,b))
      }

      infix ("reduceCols") (((DenseVector(T),DenseVector(T)) ==> DenseVector(T)) :: DenseVector(T), A) implements composite ${
        val vv = $self.colIndices.map(i => $self.getCol(i).toDense)
        vv.reduce((a,b) => $1(a,b))
      }

    }
  }
}


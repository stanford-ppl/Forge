package ppl.dsl.forge
package examples
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait DenseMatrixOps {
  this: OptiLADSL =>

  def importDenseMatrixOps() {
    val T = tpePar("T")
    val R = tpePar("R")
    val B = tpePar("B")

    val DenseVector = lookupTpe("DenseVector")
    val IndexVector = lookupTpe("IndexVector")
    val IndexWildcard = lookupTpe("IndexWildcard", stage = compile)
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")

    // data fields
    data(DenseMatrix, ("_numRows", MInt), ("_numCols", MInt), ("_data", MArray(T)))

    // static methods
    static (DenseMatrix) ("apply", T, (MInt, MInt) :: DenseMatrix(T), effect = mutable) implements allocates(DenseMatrix, ${$0}, ${$1}, ${array_empty[T]($0*$1)})

    // matrix from vector of vectors
    for (v <- List(DenseVector(T),DenseVectorView(T))) {
      static (DenseMatrix) ("apply", T, (DenseVector(v)) :: DenseMatrix(T)) implements composite ${
        val numRows = $0.length
        val numCols = $0(0).length
        (0::numRows, 0::numCols) { (i,j) => $0(i).apply(j) }
      }
    }

    // matrix from variable number of vectors (rows)
    static (DenseMatrix) ("apply", T, varArgs(DenseVector(T)) :: DenseMatrix(T)) implements single ${
      val out = DenseMatrix[T](0, 0)
      // don't lift the range over the current stage Seq[DenseVector[T]]
      for (i: Int <- scala.collection.immutable.Range(0,$0.length)) {
        out <<= $0(i)
      }
      out.unsafeImmutable
    }

    // block matrix constructor
    // we can't reuse "apply", because it is ambiguous with the row constructor above
    static (DenseMatrix) ("block", T, varArgs(DenseVector(DenseMatrix(T))) :: DenseMatrix(T)) implements single ${
      // precompute output size
      var totalNumRows = 0
      var totalNumCols = 0
      val seq = array_fromseq($0)
      for (i <- 0 until array_length(seq)) {
        val subMatRow = array_apply(seq,i)
        val numRows = subMatRow(0).numRows
        var numCols = subMatRow(0).numCols
        for (j <- 1 until subMatRow.length) {
          if (subMatRow(j).numRows != numRows) fatal("dimension mismatch in block matrix constructor: " + subMatRow(j).numRows + " != " + numRows)
          numCols += subMatRow(j).numCols
        }
        totalNumRows += numRows
        if (i == 0) {
          totalNumCols = numCols
        }
        else if (numCols != totalNumCols) {
          fatal("dimension mismatch in block matrix constructor: row " + i + " has wrong number of cols " + numCols + " (expected " + totalNumCols + ")")
          () // FIXME: these extra ()s are needed to convince the embedding that the branch returns a Rep[Unit]. why?
        }
        ()
      }

      // write blocks
      val out = DenseMatrix[T](totalNumRows, totalNumCols)
      var rowIdx = 0
      var colIdx = 0
      for (i <- 0 until array_length(seq)) {
        val subMatRow = array_apply(seq,i)
        colIdx = 0
        for (j <- 0 until subMatRow.length) {
          for (k <- 0 until subMatRow(j).numRows) {
            for (l <- 0 until subMatRow(j).numCols) {
              out(rowIdx+k, colIdx+l) = subMatRow(j).apply(k,l)
            }
          }
          colIdx += subMatRow(j).numCols
        }
        rowIdx += subMatRow(0).numRows
      }

      out.unsafeImmutable
    }

    static (DenseMatrix) ("diag", T withBound TArith, (MInt, DenseVector(T)) :: DenseMatrix(T)) implements single ${ densematrix_fromfunc($0, $0, (i,j) =>
      if (i == j) $1(i)
      else implicitly[Arith[T]].empty
    )}
    static (DenseMatrix) ("identity", Nil, (MInt,MInt) :: DenseMatrix(MDouble)) implements single ${ densematrix_fromfunc($0, $1, (i,j) =>
      if (i == j) 1.0
      else 0.0
    )}
    static (DenseMatrix) ("identity", Nil, MInt :: DenseMatrix(MDouble)) implements redirect ${ DenseMatrix.identity($0,$0) }

    // helper
    compiler (DenseMatrix) ("densematrix_fromarray", T, (MArray(T), MInt, MInt) :: DenseMatrix(T)) implements allocates(DenseMatrix, ${$1}, ${$2}, ${$0})
    compiler (DenseMatrix) ("densematrix_fromfunc", T, (MInt, MInt, (MInt,MInt) ==> T) :: DenseMatrix(T)) implements composite ${
      (0::$0, 0::$1) { (i,j) => $2(i,j) }
    }
    static (DenseMatrix) ("zeros", Nil, (MInt,MInt) :: DenseMatrix(MDouble)) implements composite ${ densematrix_fromfunc($0, $1, (i,j) => 0.0 )}
    static (DenseMatrix) ("zerosf", Nil, (MInt,MInt) :: DenseMatrix(MFloat)) implements composite ${ densematrix_fromfunc($0, $1, (i,j) => 0f )}
    static (DenseMatrix) ("ones", Nil, (MInt,MInt) :: DenseMatrix(MDouble)) implements composite ${ densematrix_fromfunc($0, $1, (i,j) => 1.0 )}
    static (DenseMatrix) ("onesf", Nil, (MInt,MInt) :: DenseMatrix(MFloat)) implements composite ${ densematrix_fromfunc($0, $1, (i,j) => 1f )}
    static (DenseMatrix) ("rand", Nil, (MInt,MInt) :: DenseMatrix(MDouble)) implements composite ${ densematrix_fromfunc($0, $1, (i,j) => random[Double] )}
    static (DenseMatrix) ("randf", Nil, (MInt,MInt) :: DenseMatrix(MFloat)) implements composite ${ densematrix_fromfunc($0, $1, (i,j) => random[Float] )}
    static (DenseMatrix) ("randn", Nil, (MInt,MInt) :: DenseMatrix(MDouble)) implements composite ${ densematrix_fromfunc($0, $1, (i,j) => randomGaussian )}
    static (DenseMatrix) ("randnf", Nil, (MInt,MInt) :: DenseMatrix(MFloat)) implements composite ${ densematrix_fromfunc($0, $1, (i,j) => randomGaussian.toFloat )}

    // these are provided for convenience for MATLAB-ians
    direct (DenseMatrix) ("diag", T, DenseMatrix(T) :: DenseVector(T)) implements redirect ${ $0.diag }
    direct (DenseMatrix) ("triu", T withBound TArith, DenseMatrix(T) :: DenseMatrix(T)) implements redirect ${ $0.triu }
    direct (DenseMatrix) ("tril", T withBound TArith, DenseMatrix(T) :: DenseMatrix(T)) implements redirect ${ $0.tril }

    val DenseMatrixOps = withTpe (DenseMatrix)
    DenseMatrixOps {
      /**
       * Conversions
       */
      infix ("toBoolean") (Nil :: DenseMatrix(MBoolean), ("conv",T ==> MBoolean)) implements map((T,MBoolean), 0, ${$conv})
      infix ("toDouble") (Nil :: DenseMatrix(MDouble), ("conv",T ==> MDouble)) implements map((T,MDouble), 0, ${$conv})
      infix ("toFloat") (Nil :: DenseMatrix(MFloat), ("conv",T ==> MFloat)) implements map((T,MFloat), 0, ${$conv})
      infix ("toInt") (Nil :: DenseMatrix(MInt), ("conv",T ==> MInt)) implements map((T,MInt), 0, ${$conv})


      /**
       * Accessors
       */
      infix ("numRows") (Nil :: MInt) implements getter(0, "_numRows")
      infix ("numCols") (Nil :: MInt) implements getter(0, "_numCols")
      infix ("size") (Nil :: MInt) implements composite ${ $self.numRows*$self.numCols }

      compiler ("densematrix_index") ((MInt,MInt) :: MInt) implements composite ${ $1*$self.numCols+ $2 }
      infix ("apply") ((MInt,MInt) :: T) implements composite ${ array_apply(densematrix_raw_data($self), densematrix_index($self,$1,$2)) }
      infix ("apply") (MInt :: DenseVectorView(T)) implements redirect ${ $self.getRow($1) }

      // TODO: we should have DenseMatrixViews instead of copying here

      // orientation of IndexVector in apply does not matter - use getCols or 2d apply to slice cols. This is so we can use n::m syntax
      // to slice rows, while still retaining our convention of row vectors being the default (e.g. for matrix construction)
      infix ("apply") (IndexVector :: DenseMatrix(T)) implements redirect ${ $self.getRows($1) }
      infix ("apply") ((IndexVector, IndexWildcard) :: DenseMatrix(T)) implements redirect ${ $self.getRows($1) }
      infix ("apply") ((("rows", IndexVector), ("cols", IndexVector)) :: DenseMatrix(T)) implements composite ${
        // if (rows.length != cols.length) fatal("dimension mismatch in bulk apply: rows.length " + rows.length + " != cols.length " + cols.length)
        (rows,cols) { (i,j) => $self(i,j) }
      }
      infix ("apply") ((IndexWildcard, IndexVector) :: DenseMatrix(T)) implements redirect ${ $self.getCols($2) }

      infix ("rowIndices") (Nil :: IndexVector) implements composite ${ IndexVector(0, $self.numRows, false) }
      infix ("colIndices") (Nil :: IndexVector) implements composite ${ IndexVector(0, $self.numCols) }

      // vview is a "contains" because the view points-to the matrix; dereferencing the view returns the matrix.
      // it is not "extracts", because it is not returning any matrix elements.
      infix ("vview") ((MInt, MInt, MInt, MBoolean) :: DenseVectorView(T), aliasHint = contains(0)) implements single ${ DenseVectorView[T](densematrix_raw_data($self), $1, $2, $3, $4) } // read-only right now
      infix ("getRow") (MInt :: DenseVectorView(T)) implements composite ${ $self.vview($1*$self.numCols, 1, $self.numCols, true) }
      infix ("getRows") (IndexVector :: DenseMatrix(T)) implements composite ${
        // DenseMatrix($1.map(i => $self(i)))
        ($1, *) { i => $self(i) }
      }
      infix ("getCol") (MInt :: DenseVectorView(T)) implements composite ${ $self.vview($1, $self.numCols, $self.numRows, false) }
      infix ("getCols") (IndexVector :: DenseMatrix(T)) implements composite ${
        // DenseMatrix($1.map(i => $self.getCol(i))).t
        (*, $1) { j => $self.getCol(j) }
      }

      infix ("slice") ((("startRow",MInt),("endRow",MInt),("startCol",MInt),("endCol",MInt)) :: DenseMatrix(T)) implements redirect ${ $self(startRow::endRow, startCol::endCol) }
      infix ("sliceRows") ((("start",MInt),("end",MInt)) :: DenseMatrix(T)) implements redirect ${ $self.getRows(start::end) }
      infix ("sliceCols") ((("start",MInt),("end",MInt)) :: DenseMatrix(T)) implements redirect ${ $self.getCols(start::end) }

      // TODO: generalize the following (and the static diag above) to kth diagonal
      // infix ("diag") (MethodSignature(List(("x",DenseMatrix(T)),("k",MInt,"0")), DenseVector(T)) implements composite ${
      infix ("diag") (Nil :: DenseVector(T)) implements composite ${
        val indices = (0::$self.numRows) { i => i + i*$self.numCols }
        indices.t map { i => densematrix_raw_apply($self,i) }
       }
      infix ("triu") (Nil :: DenseMatrix(T), TArith(T)) implements composite ${
        (0::$self.numRows, 0::$self.numCols) { (i,j) =>
          if (i <= j) $self(i,j) else implicitly[Arith[T]].empty
        }
      }
      infix ("tril") (Nil :: DenseMatrix(T), TArith(T)) implements composite ${
        (0::$self.numRows, 0::$self.numCols) { (i,j) =>
          if (i >= j) $self(i,j) else implicitly[Arith[T]].empty
        }
      }


      /**
       * Miscellaneous
       */
       // $self.toString doesn't work in Delite, since there is no 'self' instance
       infix ("pprint") (Nil :: MUnit, TStringable(T), effect = simple) implements composite ${ println($self.makeStr + "\\n") }
       infix ("makeString") (Nil :: MString, TStringable(T)) implements single ${
         var s = ""
         for (i <- 0 until $self.numRows-1) {
           s = s + $self(i).makeStr + "\\n"
         }
         if ($self.numRows > 0)
           s + $self($self.numRows-1).makeStr
         else "[ ]"
       }
       infix ("toString") (Nil :: MString) implements single ${
         var s = ""
         for (i <- 0 until $self.numRows-1) {
           s = s + densevectorview_tostring($self(i)) + "\\n"
         }
         if ($self.numRows > 0)
           s + densevectorview_tostring($self($self.numRows-1))
         else "[ ]"
       }

       infix ("t") (Nil :: DenseMatrix(T)) implements single ${
         // naive, should block
         val out = DenseMatrix[T]($self.numCols, $self.numRows)
         for (i <- 0 until $self.numCols){
           for (j <- 0 until $self.numRows){
             out(i,j) = $self(j,i)
           }
         }
         out.unsafeImmutable
       }

       infix ("Clone") (Nil :: DenseMatrix(T), aliasHint = copies(0)) implements map((T,T), 0, "e => e")
       infix ("mutable") (Nil :: DenseMatrix(T), effect = mutable, aliasHint = copies(0)) implements single ${
         val out = DenseMatrix[T]($self.numRows, $self.numCols)
         for (i <- 0 until $self.numRows) {
           for (j <- 0 until $self.numCols) {
             out(i,j) = $self(i,j)
           }
         }
         out
       }
       infix ("replicate") ((MInt,MInt) :: DenseMatrix(T)) implements single ${
         val out = DenseMatrix[T]($1*$self.numRows, $2*$self.numCols)
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
       * Data operations
       */
      compiler ("densematrix_raw_data") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("densematrix_set_raw_data") (MArray(T) :: MUnit, effect = write(0)) implements setter(0, "_data", ${$1})
      compiler ("densematrix_set_numrows") (MInt :: MUnit, effect = write(0)) implements setter(0, "_numRows", ${$1})
      compiler ("densematrix_set_numcols") (MInt :: MUnit, effect = write(0)) implements setter(0, "_numCols", ${$1})

      infix ("update") ((MInt,MInt,T) :: MUnit, effect = write(0)) implements composite ${ array_update(densematrix_raw_data($self), densematrix_index($self,$1,$2), $3) }

      for (rhs <- List(DenseVector(T),DenseVectorView(T))) {
        infix ("update") ((MInt,rhs) :: MUnit, effect = write(0)) implements composite ${
          $self.updateRow($1, $2)
        }
        infix ("updateRow") ((MInt,rhs) :: MUnit, effect=write(0)) implements composite ${
          (0::$2.length) foreach { j => $self($1,j) = $2(j) }
        }
        infix ("updateCol") ((MInt,rhs) :: MUnit, effect=write(0)) implements composite ${
          (0::$2.length) foreach { i => $self(i,$1) = $2(i) }
        }
      }

      infix ("<<") (DenseVector(T) :: DenseMatrix(T)) implements single ${
        val out = DenseMatrix[T](0, 0)
        out <<= $self
        out <<= $1
        out.unsafeImmutable
      }
      infix ("<<") (DenseMatrix(T) :: DenseMatrix(T)) implements single ${
        val out = DenseMatrix[T](0, 0)
        out <<= $self
        out <<= $1
        out.unsafeImmutable
      }
      infix ("<<|") (DenseVector(T) :: DenseMatrix(T)) implements single ${
        val out = DenseMatrix[T](0, 0)
        out.insertAllCols(0, $self)
        out.insertCol($self.numCols, $1)
        out.unsafeImmutable
      }
      infix ("<<|") (DenseMatrix(T) :: DenseMatrix(T)) implements single ${
        val out = DenseMatrix[T](0, 0)
        out.insertAllCols(0, $self)
        out.insertAllCols($self.numCols, $1)
        out.unsafeImmutable
      }
      infix ("<<=") (DenseVector(T) :: MUnit, effect = write(0)) implements composite ${ $self.insertRow($self.numRows, $1) }
      infix ("<<=") (DenseMatrix(T) :: MUnit, effect = write(0)) implements composite ${ $self.insertAllRows($self.numRows, $1) }
      infix ("<<|=") (DenseVector(T) :: MUnit, effect = write(0)) implements composite ${ $self.insertCol($self.numCols, $1) }
      infix ("<<|=") (DenseMatrix(T) :: MUnit, effect = write(0)) implements composite ${ $self.insertAllCols($self.numCols, $1) }

      infix ("insertRow") ((("pos",MInt),("y",DenseVector(T))) :: MUnit, effect=write(0)) implements single ${
        val idx = $pos*$self.numCols
        if ($self.size == 0) densematrix_set_numcols($self, $y.length)
        densematrix_insertspace($self, idx, $self.numCols)
        val data = densematrix_raw_data($self)
        for (i <- idx until idx+$self.numCols){
          array_update(data,i,$y(i-idx))
        }
        densematrix_set_numrows($self, $self.numRows+1)
      }
      infix ("insertAllRows") ((("pos",MInt),("xs",DenseMatrix(T))) :: MUnit, effect=write(0)) implements single ${
        val idx = $pos*$self.numCols
        if ($self.size == 0) densematrix_set_numcols($self, $xs.numCols)
        val sz = $self.numCols*xs.numRows
        densematrix_insertspace($self, idx, sz)
        val data = densematrix_raw_data($self)
        for (i <- idx until idx+sz){
          array_update(data,i,densematrix_raw_apply($xs, i-idx))
        }
        densematrix_set_numrows($self, $self.numRows+$xs.numRows)
      }
      infix ("insertCol") ((("pos",MInt),("y",DenseVector(T))) :: MUnit, effect=write(0)) implements single ${
        val newCols = $self.numCols+1
        if ($self.size == 0) densematrix_set_numrows($self, $y.length)
        val outData = array_empty[T]($self.numRows*newCols)
        for (i <- 0 until $self.numRows){
          var col = 0
          for (j <- 0 until newCols) {
            if (j == $pos){
              outData(i*newCols+j) = $y(i)
            }
            else{
              outData(i*newCols+j) = $self(i,col)
              col += 1
            }
          }
        }
        densematrix_set_raw_data($self, outData.unsafeImmutable)
        densematrix_set_numcols($self, newCols)
      }
      infix ("insertAllCols") ((("pos",MInt),("xs",DenseMatrix(T))) :: MUnit, effect=write(0)) implements single ${
        val newCols = $self.numCols+$xs.numCols
        if ($self.size == 0) densematrix_set_numrows($self, $xs.numRows)
        val outData = array_empty[T]($self.numRows*newCols)
        for (i <- 0 until $self.numRows){
          var col = 0
          for (j <- 0 until newCols){
            if (j < $1 || j >= $pos+$xs.numCols){
              outData(i*newCols+j) = $self(i,col)
              col += 1
            }
            else{
              outData(i*newCols+j) = $xs(i,j-$pos)
            }
          }
        }
        densematrix_set_raw_data($self, outData.unsafeImmutable)
        densematrix_set_numcols($self, newCols)
      }

      infix ("trim") (Nil :: MUnit, effect = write(0)) implements single ${
        val data = densematrix_raw_data($self)
        if ($self.size < array_length(data)) {
          val d = array_empty[T]($self.size)
          array_copy(data, 0, d, 0, $self.size)
          densematrix_set_raw_data($self, d.unsafeImmutable)
        }
      }

      compiler ("densematrix_insertspace") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements single ${
        if ($pos < 0 || $pos > $self.size) fatal("DenseMatrix IndexOutOfBounds")
        densematrix_ensureextra($self,$len)
        val d = densematrix_raw_data($self)
        array_copy(d, $pos, d, $pos + $len, $self.size - $pos)
      }
      compiler ("densematrix_ensureextra") (("extra",MInt) :: MUnit, effect = write(0)) implements single ${
        val data = densematrix_raw_data($self)
        if (array_length(data) - $self.size < $extra) {
          densematrix_realloc($self, $self.size+$extra)
        }
      }
      compiler ("densematrix_realloc") (("minLen",MInt) :: MUnit, effect = write(0)) implements single ${
        val data = densematrix_raw_data($self)
        var n = max(4, array_length(data) * 2)
        while (n < minLen) n = n*2
        val d = array_empty[T](n)
        array_copy(data, 0, d, 0, $self.size)
        densematrix_set_raw_data($self, d.unsafeImmutable)
      }

      infix ("removeRow") (("pos",MInt) :: MUnit, effect = write(0)) implements composite ${ $self.removeRows($pos, 1) }
      infix ("removeCol") (("pos",MInt) :: MUnit, effect = write(0)) implements composite ${ $self.removeCols($pos, 1) }
      infix ("removeRows") ((("pos",MInt),("num",MInt)) :: MUnit, effect=write(0)) implements single ${
        val idx = $pos*$self.numCols
        val len = $num*$self.numCols
        val data = densematrix_raw_data($self)
        array_copy(data, idx + len, data, idx, $self.size - (idx + len))
        densematrix_set_numrows($self, $self.numRows - $num)
      }
      infix ("removeCols") ((("pos",MInt),("num",MInt)) :: MUnit, effect=write(0)) implements single ${
        val newCols = $self.numCols-$num
        val outData = array_empty[T]($self.numRows*newCols)
        for (i <- 0 until $self.numRows){
          var col = 0
          for (j <- 0 until $self.numCols){
            if (j < $pos || j >= $pos+$num){
              outData(i*newCols+col) = $self(i,j)
              col += 1
            }
          }
        }
        densematrix_set_raw_data($self, outData.unsafeImmutable)
        densematrix_set_numcols($self, newCols)
      }


      /**
       * Math
       */

       // TODO: inverse
       infix ("+") (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements zip((T,T,T), (0,1), ${ (a,b) => a+b })
       infix ("+") (T :: DenseMatrix(T), TArith(T)) implements map((T,T), 0, ${ e => e+$1 })
         // infix ("+") (DenseMatrix(B) :: DenseMatrix(T), (TArith(T), B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a+b })
       infix ("+=") (DenseMatrix(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
         val indices = IndexVector(0,$self.size)
         indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)+densematrix_raw_apply($1,i)) }
       }
       infix ("+=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${
         val indices = IndexVector(0,$self.size)
         indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)+$1) }
       }

       infix ("-") (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements zip((T,T,T), (0,1), ${ (a,b) => a-b })
       infix ("-") (T :: DenseMatrix(T), TArith(T)) implements map((T,T), 0, ${ e => e-$1 })
         // infix ("-") (DenseMatrix(B) :: DenseMatrix(T), (TArith(T), B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a-b })
       infix ("-=") (DenseMatrix(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
         val indices = IndexVector(0,$self.size)
         indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)-densematrix_raw_apply($1,i)) }
       }
       infix ("-=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${
         val indices = IndexVector(0,$self.size)
         indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)-$1) }
       }

       infix ("*:*") (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements zip((T,T,T), (0,1), ${ (a,b) => a*b })
       infix ("*") (T :: DenseMatrix(T), TArith(T)) implements map((T,T), 0, ${ e => e*$1 })
         // infix ("*") (DenseMatrix(B) :: DenseMatrix(T), (TArith(T), B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a*b })
       infix ("*=") (DenseMatrix(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
         val indices = IndexVector(0,$self.size)
         indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)*densematrix_raw_apply($1,i)) }
       }
       infix ("*=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${
         val indices = IndexVector(0,$self.size)
         indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)*$1) }
       }
       infix ("*") (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements single ${
         if ($self.numCols != $1.numRows) fatal("dimension mismatch: matrix multiply")
         // naive
         val yT = $1.t
         val out = DenseMatrix[T]($self.numRows, $1.numCols)
         for (rowIdx <- 0 until $self.numRows) {
           for (i <- 0 until $1.numCols) {
             var acc = $self(rowIdx, 0) * yT(i, 0)
             for (j <- 1 until yT.numCols) {
               acc += $self(rowIdx, j) * yT(i, j)
             }
             out(rowIdx, i) = acc
           }
         }
         out.unsafeImmutable
       }
       infix ("*") (DenseVector(T) :: DenseVector(T), TArith(T)) implements single ${
        if ($self.numCols != $1.length || $1.isRow) fatal("dimension mismatch: matrix * vector")
        val out = DenseVector[T]($self.numRows, false)
        for (rowIdx <- 0 until $self.numRows) {
          out(rowIdx) = $self(rowIdx) *:* $1
        }
        out.unsafeImmutable
       }

       infix ("/") (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements zip((T,T,T), (0,1), ${ (a,b) => a/b })
       infix ("/") (T :: DenseMatrix(T), TArith(T)) implements map((T,T), 0, ${ e => e/$1 })
         // infix ("/") (DenseMatrix(B) :: DenseMatrix(T), (TArith(T), B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a/b })
       infix ("/=") (DenseMatrix(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
         val indices = IndexVector(0,$self.size)
         indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)/densematrix_raw_apply($1,i)) }
       }
       infix ("/=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${
         val indices = IndexVector(0,$self.size)
         indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)/$1) }
       }

       infix ("sum") (Nil :: T, TArith(T)) implements reduce(T, 0, ${ implicitly[Arith[T]].empty }, ${ (a,b) => a+b })
       infix ("mean") (Nil :: MDouble, ("conv",T ==> MDouble)) implements composite ${ $self.map(conv).sum / $self.size }
       infix ("abs") (Nil :: DenseMatrix(T), TArith(T)) implements map((T,T), 0, ${ e => e.abs })
       infix ("exp") (Nil :: DenseMatrix(T), TArith(T)) implements map((T,T), 0, ${ e => e.exp })
       infix ("log") (Nil :: DenseMatrix(T), TArith(T)) implements map((T,T), 0, ${ e => e.log })

       infix ("sumRows") (Nil :: DenseVector(T), TArith(T)) implements composite ${ $self.mapRowsToVector { row => sum(row) }}
       infix ("sumCols") (Nil :: DenseVector(T), TArith(T)) implements composite ${ $self.mapColsToVector { col => sum(col) }}
       infix ("minRows") (Nil :: DenseVector(T), TOrdering(T)) implements composite ${ $self.mapRowsToVector { row => min(row) }}
       infix ("minCols") (Nil :: DenseVector(T), TOrdering(T)) implements composite ${ $self.mapColsToVector { col => min(col) }}
       infix ("maxRows") (Nil :: DenseVector(T), TOrdering(T)) implements composite ${ $self.mapRowsToVector { row => max(row) }}
       infix ("maxCols") (Nil :: DenseVector(T), TOrdering(T)) implements composite ${ $self.mapColsToVector { col => max(col) }}

       /**
        * Ordering
        */

       infix ("min") (Nil :: T, TOrdering(T)) implements reduce(T, 0, ${$self(0,0)}, ${ (a,b) => if (a < b) a else b })
       infix ("max") (Nil :: T, TOrdering(T)) implements reduce(T, 0, ${$self(0,0)}, ${ (a,b) => if (a > b) a else b })

       // TODO: switch to reduce when TupleReduce is generalized
       val Tuple2 = lookupTpe("Tup2")
       infix ("minIndex") (Nil :: Tuple2(MInt,MInt), TOrdering(T)) implements composite ${
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
       infix ("maxIndex") (Nil :: Tuple2(MInt,MInt), TOrdering(T)) implements composite ${
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

       infix (":>") (DenseMatrix(T) :: DenseMatrix(MBoolean), TOrdering(T)) implements zip((T,T,MBoolean), (0,1), ${ (a,b) => a > b })
       infix (":<") (DenseMatrix(T) :: DenseMatrix(MBoolean), TOrdering(T)) implements zip((T,T,MBoolean), (0,1), ${ (a,b) => a < b })

       direct ("__equal") (DenseMatrix(T) :: MBoolean) implements composite ${
        if ($self.numRows != $1.numRows || $self.numCols != $1.numCols) false
          else {
            val c = sum($self.zip($1) { (a,b) => if (a == b) 0 else 1})
            c == 0
          }
       }


       /**
        *  Bulk
        */
       infix ("map") ((T ==> R) :: DenseMatrix(R), addTpePars = R) implements map((T,R), 0, ${ e => $1(e) })
       infix ("mapRowsToVector") ((DenseVectorView(T) ==> R) :: DenseVector(R), addTpePars = R) implements composite ${
         $self.rowIndices.map(i => $1($self(i)))
       }
       infix ("mapColsToVector") ((DenseVectorView(T) ==> R) :: DenseVector(R), addTpePars = R) implements composite ${
         $self.colIndices.map(i => $1($self.getCol(i)))
       }
       infix ("zip") (CurriedMethodSignature(List(List(DenseMatrix(B)), List((T,B) ==> R)), DenseMatrix(R)), addTpePars = (B,R)) implements zip((T,B,R), (0,1), ${ (a,b) => $2(a,b) })
       infix ("foreach") ((T ==> MUnit) :: MUnit) implements foreach(T, 0, ${ e => $1(e) })
       infix ("count") ((T ==> MBoolean) :: MInt) implements mapReduce((T,MInt), 0, ${ e => 1 }, ${ 0 }, ${ (a,b) => a+b }, Some(${ e => $1(e)}))

       infix ("findRows") ((DenseVectorView(T) ==> MBoolean) :: IndexVector) implements composite ${
         IndexVector($self.rowIndices.filter(i => $1($self(i))))
       }
       infix ("findCols") ((DenseVectorView(T) ==> MBoolean) :: IndexVector) implements composite ${
         IndexVector($self.colIndices.filter(i => $1($self.getCol(i))))
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
         val out = DenseMatrix[R]($self.numRows, $self.numCols)
         (0::$self.numRows) foreach { i =>
           out(i) = $1($self(i))
         }
         out.unsafeImmutable
       }
       infix ("mapCols") ((DenseVectorView(T) ==> DenseVector(R)) :: DenseMatrix(R), addTpePars = R) implements composite ${
         val out = DenseMatrix[R]($self.numRows, $self.numCols)
         (0::$self.numCols) foreach { j =>
           out.updateCol(j, $1($self.getCol(j)))
         }
         out.unsafeImmutable
       }

       // in order to express this with the current Delite ops, we have to convert the matrix to a vector of vectors,
       // which is unfortunate. A vector of vectorviews would be somewhat better, but since Delite reduce requires
       // (A,A) => A, we cannot yet express that operation in parallel with converting each vectors.
       // however, the map and reduce here should fuse, eliminating the overhead in the Delite version.
       infix ("reduceRows") (((DenseVector(T),DenseVector(T)) ==> DenseVector(T)) :: DenseVector(T), TArith(T)) implements composite ${
         val vv = $self.rowIndices.map(i => $self(i).toDense)
         vv.reduce((a,b) => $1(a,b))
       }

       infix ("reduceCols") (((DenseVector(T),DenseVector(T)) ==> DenseVector(T)) :: DenseVector(T), TArith(T)) implements composite ${
         val vv = $self.colIndices.map(i => $self.getCol(i).toDense)
         vv.reduce((a,b) => $1(a,b))
       }

       // TODO: waiting for DeliteHashMap to implement these.
       // infix ("groupRowsBy") (((T ==> K)) :: DenseVector(DenseMatrixT)))
       // infix ("groupColsBy") (((T ==> K)) :: DenseVector(DenseMatrixT)))

      /**
       * Required for parallel collection
       */
      compiler ("densematrix_raw_alloc") (MInt :: DenseMatrix(R), addTpePars = R) implements composite ${
        // assert($1 == $self.size) // any reason this would not be true? <-- assert fails because it is staging time reference equality
        DenseMatrix[R]($self.numRows, $self.numCols)
      }
      direct ("densematrix_raw_apply") (MInt :: T) implements composite ${ array_apply(densematrix_raw_data($self), $1) }
      direct ("densematrix_raw_update") ((MInt,T) :: MUnit, effect = write(0)) implements composite ${ array_update(densematrix_raw_data($self), $1, $2) }

      parallelize as ParallelCollection(T, lookupOp("densematrix_raw_alloc"), lookupOp("size"), lookupOp("densematrix_raw_apply"), lookupOp("densematrix_raw_update"))
    }

    // label lets you give a precise name to a particular variant of an overloaded op (typically so that you can refer to it in external code)
    // we use the following labels to optionally override the ops by calling BLAS
    label(lookupOverloaded("DenseMatrix","*",1), "densematrix_matmult")
    label(lookupOverloaded("DenseMatrix","*",2), "densematrix_matvecmult")

    // Add DenseMatrix to Arith
    val Arith = lookupGrp("Arith").asInstanceOf[Rep[DSLTypeClass]]
    val DenseMatrixArith = tpeClassInst("ArithDenseMatrix", T withBound TArith, Arith(DenseMatrix(T)))
    infix (DenseMatrixArith) ("zero", T withBound TArith, DenseMatrix(T) :: DenseMatrix(T)) implements composite ${ DenseMatrix[T]($0.numRows,$0.numCols).unsafeImmutable }
    infix (DenseMatrixArith) ("empty", T withBound TArith, Nil :: DenseMatrix(T)) implements composite ${ DenseMatrix[T](unit(0),unit(0)).unsafeImmutable }
    infix (DenseMatrixArith) ("+", T withBound TArith, (DenseMatrix(T),DenseMatrix(T)) :: DenseMatrix(T)) implements composite ${ densematrix_pl($0,$1) }
    infix (DenseMatrixArith) ("-", T withBound TArith, (DenseMatrix(T),DenseMatrix(T)) :: DenseMatrix(T)) implements composite ${ densematrix_sub($0,$1) }
    infix (DenseMatrixArith) ("*", T withBound TArith, (DenseMatrix(T),DenseMatrix(T)) :: DenseMatrix(T)) implements composite ${ densematrix_mulclnmul($0,$1) }
    infix (DenseMatrixArith) ("/", T withBound TArith, (DenseMatrix(T),DenseMatrix(T)) :: DenseMatrix(T)) implements composite ${ densematrix_div($0,$1) }
    infix (DenseMatrixArith) ("abs", T withBound TArith, DenseMatrix(T) :: DenseMatrix(T)) implements composite ${ densematrix_abs($0) }
    infix (DenseMatrixArith) ("exp", T withBound TArith, DenseMatrix(T) :: DenseMatrix(T)) implements composite ${ densematrix_exp($0) }
    infix (DenseMatrixArith) ("log", T withBound TArith, DenseMatrix(T) :: DenseMatrix(T)) implements composite ${ densematrix_log($0) }

    importDenseMatrixPrimitiveOps()
  }

  /**
   * Special cases for DenseMatrix primitive arithmetic. This is annoying, so let's hide it at the bottom.
   */
  def importDenseMatrixPrimitiveOps() {
    val DenseMatrix = lookupTpe("DenseMatrix")
    val DenseVector = lookupTpe("DenseVector")

    // the conversions here will be costly unless things fuse. alternatively, we could convert element by element.

    infix (DenseMatrix) ("+", Nil, (MInt,DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect ${ densematrix_pl[Int]($1,$0) }
    infix (DenseMatrix) ("+", Nil, (MInt,DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($1,$0.toFloat) }
    infix (DenseMatrix) ("+", Nil, (MInt,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($1,$0.toDouble) }
    infix (DenseMatrix) ("+", Nil, (MFloat,DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($1.toFloat,$0) }
    infix (DenseMatrix) ("+", Nil, (MFloat,DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($1,$0) }
    infix (DenseMatrix) ("+", Nil, (MFloat,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($1,$0.toDouble) }
    infix (DenseMatrix) ("+", Nil, (MDouble,DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($1.toDouble,$0) }
    infix (DenseMatrix) ("+", Nil, (MDouble,DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($1.toDouble,$0) }
    infix (DenseMatrix) ("+", Nil, (MDouble,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($1,$0) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),MInt) :: DenseMatrix(MInt)) implements redirect ${ densematrix_pl[Int]($0,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),MFloat) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),MInt) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),MFloat) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($0,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),MInt) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),MFloat) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect ${ densematrix_pl[Int]($0,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MInt),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_pl[Float]($0,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MFloat),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("+", Nil, (DenseMatrix(MDouble),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_pl[Double]($0,$1) }

    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),MInt) :: DenseMatrix(MInt)) implements redirect ${ densematrix_sub[Int]($0,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),MFloat) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_sub[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),MInt) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_sub[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),MFloat) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_sub[Float]($0,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),MInt) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),MFloat) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect ${ densematrix_sub[Int]($0,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_sub[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MInt),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_sub[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_sub[Float]($0,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MFloat),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("-", Nil, (DenseMatrix(MDouble),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_sub[Double]($0,$1) }

    infix (DenseMatrix) ("unary_-", Nil, (DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect ${ densematrix_mul[Int]($0,unit(-1)) }
    infix (DenseMatrix) ("unary_-", Nil, (DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mul[Float]($0,unit(-1f)) }
    infix (DenseMatrix) ("unary_-", Nil, (DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($0,unit(-1.0)) }
    infix (DenseMatrix) ("*", Nil, (MInt,DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect ${ densematrix_mul[Int]($1,$0) }
    infix (DenseMatrix) ("*", Nil, (MInt,DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mul[Float]($1,$0.toFloat) }
    infix (DenseMatrix) ("*", Nil, (MInt,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($1,$0.toDouble) }
    infix (DenseMatrix) ("*", Nil, (MFloat,DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mul[Float]($1.toFloat,$0) }
    infix (DenseMatrix) ("*", Nil, (MFloat,DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mul[Float]($1,$0) }
    infix (DenseMatrix) ("*", Nil, (MFloat,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($1,$0.toDouble) }
    infix (DenseMatrix) ("*", Nil, (MDouble,DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($1.toDouble,$0) }
    infix (DenseMatrix) ("*", Nil, (MDouble,DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($1.toDouble,$0) }
    infix (DenseMatrix) ("*", Nil, (MDouble,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($1,$0) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),MInt) :: DenseMatrix(MInt)) implements redirect ${ densematrix_mul[Int]($0,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),MFloat) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mul[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),MInt) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mul[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),MFloat) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mul[Float]($0,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),MInt) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),MFloat) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mul[Double]($0,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect ${ densematrix_matmult[Int]($0,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_matmult[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_matmult[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_matmult[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_matmult[Float]($0,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_matmult[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_matmult[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_matmult[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_matmult[Double]($0,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseVector(MInt)) :: DenseVector(MInt)) implements redirect ${ densematrix_matvecmult[Int]($0,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densematrix_matvecmult[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MInt),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densematrix_matvecmult[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseVector(MInt)) :: DenseVector(MFloat)) implements redirect ${ densematrix_matvecmult[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseVector(MFloat)) :: DenseVector(MFloat)) implements redirect ${ densematrix_matvecmult[Float]($0,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MFloat),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densematrix_matvecmult[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseVector(MInt)) :: DenseVector(MDouble)) implements redirect ${ densematrix_matvecmult[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseVector(MFloat)) :: DenseVector(MDouble)) implements redirect ${ densematrix_matvecmult[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("*", Nil, (DenseMatrix(MDouble),DenseVector(MDouble)) :: DenseVector(MDouble)) implements redirect ${ densematrix_matvecmult[Double]($0,$1) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MInt),DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect ${ densematrix_mulclnmul[Int]($0,$1) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MInt),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mulclnmul[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MInt),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mulclnmul[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MFloat),DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mulclnmul[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MFloat),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_mulclnmul[Float]($0,$1) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MFloat),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mulclnmul[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MDouble),DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mulclnmul[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MDouble),DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mulclnmul[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("*:*", Nil, (DenseMatrix(MDouble),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_mulclnmul[Double]($0,$1) }

    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),MInt) :: DenseMatrix(MInt)) implements redirect ${ densematrix_div[Int]($0,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),MFloat) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_div[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),MInt) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_div[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),MFloat) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_div[Float]($0,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),MInt) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),MFloat) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),MDouble) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect ${ densematrix_div[Int]($0,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_div[Float]($0.toFloat,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MInt),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_div[Float]($0,$1.toFloat) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_div[Float]($0,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MFloat),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0.toDouble,$1) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0,$1.toDouble) }
    infix (DenseMatrix) ("/", Nil, (DenseMatrix(MDouble),DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_div[Double]($0,$1) }
  }
}

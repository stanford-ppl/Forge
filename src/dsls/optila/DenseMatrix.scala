package ppl.dsl.forge
package dsls
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
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val DenseMatrixView = lookupTpe("DenseMatrixView")
    val SparseMatrix = lookupTpe("SparseMatrix")
    val Tuple2 = lookupTpe("Tup2")

    // data fields
    data(DenseMatrix, ("_numRows", MInt), ("_numCols", MInt), ("_data", MArray(T)))

    // static methods
    static (DenseMatrix) ("apply", T, (MInt, MInt) :: DenseMatrix(T), effect = mutable) implements allocates(DenseMatrix, ${$0}, ${$1}, ${array_empty[T]($0*$1)})
    static (DenseMatrix) ("apply", T, (MArray(T), MInt, MInt) :: DenseMatrix(T)) implements composite ${ densematrix_fromarray($0, $1, $2) }

    // matrix from vector of vectors
    for (v <- List(DenseVector(T),DenseVectorView(T))) {
      static (DenseMatrix) ("apply", T, (DenseVector(v)) :: DenseMatrix(T)) implements composite ${
        // (0::$0.length, *) { i => $0(i) }
        if ($0.length == 0) densematrix_fromarray[T](array_empty_imm[T](0), 0, 0)
        else {
          var z = $0 // manual guard against code motion
          (0::z.length, *) { i => z(i) }
        }
      }
    }

    // matrix from variable number of vectors (rows)
    static (DenseMatrix) ("apply", T, varArgs(DenseVector(T)) :: DenseMatrix(T)) implements composite ${
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
          fassert(subMatRow(j).numRows == numRows, "dimension mismatch in block matrix constructor: " + subMatRow(j).numRows + " != " + numRows)
          numCols += subMatRow(j).numCols
        }
        totalNumRows += numRows
        if (i == 0) {
          totalNumCols = numCols
        }
        else {
          fassert(numCols == totalNumCols, "dimension mismatch in block matrix constructor: row " + i + " has wrong number of cols " + numCols + " (expected " + totalNumCols + ")")
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

    static (DenseMatrix) ("diag", T withBound TArith, (MInt, DenseVector(T)) :: DenseMatrix(T)) implements composite ${ densematrix_fromfunc($0, $0, (i,j) =>
      if (i == j) $1(i)
      else implicitly[Arith[T]].empty
    )}
    static (DenseMatrix) ("identity", Nil, (MInt,MInt) :: DenseMatrix(MDouble)) implements composite ${ densematrix_fromfunc($0, $1, (i,j) =>
      if (i == j) 1.0
      else 0.0
    )}
    static (DenseMatrix) ("identity", Nil, MInt :: DenseMatrix(MDouble)) implements redirect ${ DenseMatrix.identity($0,$0) }

    // helpers
    direct (DenseMatrix) ("densematrix_fromarray", T, (MArray(T), MInt, MInt) :: DenseMatrix(T)) implements allocates(DenseMatrix, ${$1}, ${$2}, ${$0})
    direct (DenseMatrix) ("densematrix_fromfunc", T, (MInt, MInt, (MInt,MInt) ==> T) :: DenseMatrix(T)) implements composite ${
      (0::$0, 0::$1) { (i,j) => $2(i,j) }
    }
    compiler (DenseMatrix) ("matrix_shapeindex", Nil, (("idx",MLong),("numCols",MLong)) :: Tuple2(MInt,MInt)) implements composite ${
      val rowIndex = idx / numCols
      val colIndex = idx % numCols
      pack(rowIndex.toInt,colIndex.toInt)
    }

    val K = tpePar("K")
    val V = tpePar("V")

    compiler (DenseMatrix) ("densematrix_grouprowsby_helper", (T,K,V), (IndexVector, DenseMatrix(T), DenseVectorView(T) ==> K, DenseVectorView(T) ==> V) :: MHashMap(K, MArrayBuffer(V))) implements groupBy((MInt,K,V), 0, ${i => $2($1.getRow(i))}, ${i => $3($1.getRow(i)) })
    compiler (DenseMatrix) ("densematrix_groupcolsby_helper", (T,K,V), (IndexVector, DenseMatrix(T), DenseVectorView(T) ==> K, DenseVectorView(T) ==> V) :: MHashMap(K, MArrayBuffer(V))) implements groupBy((MInt,K,V), 0, ${i => $2($1.getCol(i))}, ${i => $3($1.getCol(i)) })

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

    // a non-type-safe way of passing the metadata required to allocate a DenseMatrix in a parallel op
    // ideally we would encode this is as a type class, but it's not clear we would get an instance of this type class in dc_alloc
    val CR = tpePar("CR")
    compiler (DenseMatrix) ("densematrix_dc_alloc", (R,CR), (CR,MInt) :: DenseMatrix(R)) implements composite ${
      val simpleName = manifest[CR].erasure.getSimpleName
      val (numRows, numCols) = simpleName match {
        case s if s.startsWith("DenseMatrixView") =>
          (densematrixview_numrows($0.asInstanceOf[Rep[DenseMatrixView[Any]]]), densematrixview_numcols($0.asInstanceOf[Rep[DenseMatrixView[Any]]]))
        case s if s.startsWith("DenseMatrix") =>
          (densematrix_numrows($0.asInstanceOf[Rep[DenseMatrix[Any]]]), densematrix_numcols($0.asInstanceOf[Rep[DenseMatrix[Any]]]))
      }
      DenseMatrix[R](numRows, numCols)
    }

    val DenseMatrixOps = withTpe (DenseMatrix)
    DenseMatrixOps {
      /**
       * Conversions
       */
      // This workaround is required for 2.11 for some reason (the matrix conversion implicit fails to
      // resolve for vector * matrix).
      mustInfixList :::= List("toFloat", "toDouble")

      // But on the other hand, infix is not working for these in 2.11.
      noInfixList :::= List("slice", "vview", "getRow", "getCol")

      infix ("flattenToVector") (Nil :: DenseVector(T)) implements composite ${
        (0::$self.size) { i => densematrix_raw_apply($self, i) }
      }

      /**
       * Accessors
       */
      infix ("numRows") (Nil :: MInt) implements getter(0, "_numRows")
      infix ("numCols") (Nil :: MInt) implements getter(0, "_numCols")
      infix ("size") (Nil :: MInt) implements composite ${ $self.numRows*$self.numCols }

      compiler ("densematrix_index") ((MInt,MInt) :: MInt) implements composite ${ $1*$self.numCols + $2 }
      infix ("apply") ((MInt,MInt) :: T) implements composite ${ array_apply(densematrix_raw_data($self), densematrix_index($self,$1,$2)) }

      // vview, mview are "contains" because the view points-to the matrix; dereferencing the view returns the matrix.
      // it is not "extracts", because it is not returning any matrix elements.
      infix ("vview") ((("start", MInt), ("stride", MInt), ("length", MInt), ("isRow", MBoolean)) :: DenseVectorView(T), aliasHint = contains(0)) implements single ${
        // read-only right now
        DenseVectorView[T](densematrix_raw_data($self), $1, $2, $3, $4)
      }
      label(lookupOp("vview"), "densematrix_vectorview")

      infix ("mview") ((("startRow", MInt), ("endRow", MInt), ("startCol", MInt), ("endCol", MInt)) :: DenseMatrixView(T), aliasHint = contains(0)) implements single ${
        // read-only right now
        DenseMatrixView[T](densematrix_raw_data($self), $1, $2, $3, $4, $self.numRows, $self.numCols)
      }

      infix ("getRow") (MInt :: DenseVectorView(T)) implements composite ${ $self.vview($1*$self.numCols, 1, $self.numCols, true) }
      infix ("getCol") (MInt :: DenseVectorView(T)) implements composite ${ $self.vview($1, $self.numCols, $self.numRows, false) }

      infix ("slice") ((("startRow",MInt),("endRow",MInt),("startCol",MInt),("endCol",MInt)) :: DenseMatrixView(T)) implements composite ${
        $self.mview(startRow, endRow, startCol, endCol)
      }

      // TODO: generalize the following (and the static diag above) to kth diagonal
      // infix ("diag") (MethodSignature(List(("x",DenseMatrix(T)),("k",MInt,"unit(0)")), DenseVector(T)) implements composite ${
      infix ("diag") (Nil :: DenseVector(T)) implements composite ${
        val dim = min($self.numRows, $self.numCols)
        val indices = (0::dim) { i => i + i*$self.numCols }
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
       infix ("Clone") (Nil :: DenseMatrix(T), aliasHint = copies(0)) implements composite ${
         densematrix_fromarray(array_clone(densematrix_raw_data($self)), $self.numRows, $self.numCols)
       }

       infix ("toSparse") (Nil :: SparseMatrix(T)) implements composite ${
          val sparseElements = $self.indices.map { i =>
            pack((densematrix_raw_apply($self, i), i / $self.numCols, i % $self.numCols))
          }

          SparseMatrix.fromElements(
            $self.numRows,
            $self.numCols,
            sparseElements.map(_._1),
            sparseElements.map(_._2),
            sparseElements.map(_._3)
          )
       }


      /**
       * Data operations
       */
      compiler ("densematrix_raw_data") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("densematrix_set_raw_data") (MArray(T) :: MUnit, effect = write(0)) implements setter(0, "_data", ${$1})
      compiler ("densematrix_set_numrows") (MInt :: MUnit, effect = write(0)) implements setter(0, "_numRows", ${$1})
      compiler ("densematrix_set_numcols") (MInt :: MUnit, effect = write(0)) implements setter(0, "_numCols", ${$1})

      infix ("toArray") (Nil :: MArray(T)) implements composite ${ densematrix_raw_data($self.map(e => e)) }

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

      infix ("update") ((IndexVector,DenseMatrix(T)) :: MUnit, effect=write(0)) implements composite ${
        $self.updateRows($1, $2)
      }

      infix ("updateRows") ((IndexVector,DenseMatrix(T)) :: MUnit, effect=write(0)) implements composite ${
        fassert($1.length == $2.numRows && $self.numCols == $2.numCols, "dimension mismatch in updateRows")
        (0::$1.length) foreach { (i: Rep[Int]) =>
          val row: Rep[Int] = $1(i)
          $self.updateRow(row, $2(i))
        }
      }

      infix ("updateCols") ((IndexVector,DenseMatrix(T)) :: MUnit, effect=write(0)) implements composite ${
        fassert($1.length == $2.numCols && $self.numRows == $2.numRows, "dimension mismatch in updateCols")
        (0::$1.length) foreach { (i: Rep[Int]) =>
          val col: Rep[Int] = $1(i)
          $self.updateCol(col, $2.getCol(i))
        }
      }

      infix ("<<") (DenseVector(T) :: DenseMatrix(T)) implements composite ${
        val out = DenseMatrix[T](0, 0)
        out <<= $self
        out <<= $1
        out.unsafeImmutable
      }
      infix ("<<") (DenseMatrix(T) :: DenseMatrix(T)) implements composite ${
        val out = DenseMatrix[T](0, 0)
        out <<= $self
        out <<= $1
        out.unsafeImmutable
      }
      infix ("<<|") (DenseVector(T) :: DenseMatrix(T)) implements composite ${
        val out = DenseMatrix[T](0, 0)
        out.insertAllCols(0, $self)
        out.insertCol($self.numCols, $1)
        out.unsafeImmutable
      }
      infix ("<<|") (DenseMatrix(T) :: DenseMatrix(T)) implements composite ${
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

      infix ("removeRow") (("pos",MInt) :: MUnit, effect = write(0)) implements composite ${ $self.removeRows($pos, 1) }
      infix ("removeCol") (("pos",MInt) :: MUnit, effect = write(0)) implements composite ${ $self.removeCols($pos, 1) }
      infix ("removeRows") ((("pos",MInt),("num",MInt)) :: MUnit, effect=write(0)) implements composite ${
        val idx = $pos*$self.numCols
        val len = $num*$self.numCols
        val data = densematrix_raw_data($self)
        array_copy(data, idx + len, data, idx, $self.size - (idx + len))
        densematrix_set_numrows($self, $self.numRows - $num)
      }
      infix ("removeCols") ((("pos",MInt),("num",MInt)) :: MUnit, effect=write(0)) implements composite ${
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

      compiler ("densematrix_insertspace") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements single ${
        fassert($pos >= 0 && $pos <= $self.size, "densematrix_insertspace: index out of bounds")
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


      /**
       * Math
       */

       infix ("+=") (DenseMatrix(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
         $self.indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)+densematrix_raw_apply($1,i)) }
       }
       infix ("+=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${
         $self.indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)+$1) }
       }

       infix ("-=") (DenseMatrix(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
         $self.indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)-densematrix_raw_apply($1,i)) }
       }
       infix ("-=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${
         $self.indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)-$1) }
       }

       infix ("*=") (DenseMatrix(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
         $self.indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)*densematrix_raw_apply($1,i)) }
       }
       infix ("*=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${
         $self.indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)*$1) }
       }

       infix ("/=") (DenseMatrix(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
         $self.indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)/densematrix_raw_apply($1,i)) }
       }
       infix ("/=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${
         $self.indices.foreach { i => densematrix_raw_update($self,i,densematrix_raw_apply($self,i)/$1) }
       }


       /**
        * Ordering
        */

       infix ("sortRowsBy") ((DenseVectorView(T) ==> B) :: DenseMatrix(T), TOrdering(B), addTpePars = B) implements composite ${
         val sortedIndicesRaw = farray_from_sarray(densevector_sortindex_helper(0, $self.numRows, densevector_raw_data($self.mapRowsToVector($1))))
         val sortedIndices = IndexVector(densevector_fromarray(sortedIndicesRaw,true))
         $self(sortedIndices)
       }

       infix ("sortColsBy") ((DenseVectorView(T) ==> B) :: DenseMatrix(T), TOrdering(B), addTpePars = B) implements composite ${
         val sortedIndicesRaw = farray_from_sarray(densevector_sortindex_helper(0, $self.numCols, densevector_raw_data($self.mapColsToVector($1))))
         val sortedIndices = IndexVector(densevector_fromarray(sortedIndicesRaw,true))
         $self.getCols(sortedIndices)
       }

       infix (":>") (DenseMatrix(T) :: DenseMatrix(MBoolean), TOrdering(T)) implements zip((T,T,MBoolean), (0,1), ${ (a,b) => a > b })
       infix (":<") (DenseMatrix(T) :: DenseMatrix(MBoolean), TOrdering(T)) implements zip((T,T,MBoolean), (0,1), ${ (a,b) => a < b })

       for (rhs <- List(DenseMatrix(T),DenseMatrixView(T))) {
         direct ("__equal") (rhs :: MBoolean) implements composite ${
          if ($self.numRows != $1.numRows || $self.numCols != $1.numCols) false
            else {
              val c = sum($self.zip($1) { (a,b) => if (a == b) 0 else 1})
              c == 0
            }
         }
       }

       direct ("__equal") (SparseMatrix(T) :: MBoolean) implements composite ${ $self == $1.toDense }


       /**
        * Bulk
        */
       infix ("groupRowsBy") ((DenseVectorView(T) ==> K) :: MHashMap(K, DenseMatrix(T)), addTpePars = K) implements composite ${
         val grps = densematrix_grouprowsby_helper($self.rowIndices, $self, $1, (row: Rep[DenseVectorView[T]]) => row)
         val vals = fhashmap_values(grps)
         val submats = vals.map(buf => (0::array_buffer_length(buf), *) { i => array_buffer_apply(buf,i) })
         fhashmap_from_arrays(fhashmap_keys(grps), submats)
       }

       infix ("groupColsBy") ((DenseVectorView(T) ==> K) :: MHashMap(K, DenseMatrix(T)), addTpePars = K) implements composite ${
         val grps = densematrix_groupcolsby_helper($self.colIndices, $self, $1, (col: Rep[DenseVectorView[T]]) => col)
         val vals = fhashmap_values(grps)
         val submats = vals.map(buf => (*, 0::array_buffer_length(buf)) { j => array_buffer_apply(buf,j) })
         fhashmap_from_arrays(fhashmap_keys(grps), submats)
       }

      /**
       * Required for parallel collection
       */
      direct ("densematrix_raw_apply") (MInt :: T) implements composite ${ array_apply(densematrix_raw_data($self), $1) }
      direct ("densematrix_raw_update") ((MInt,T) :: MUnit, effect = write(0)) implements composite ${ array_update(densematrix_raw_data($self), $1, $2) }

      parallelize as ParallelCollection(T, lookupOp("densematrix_dc_alloc"), lookupOp("size"), lookupOp("densematrix_raw_apply"), lookupOp("densematrix_raw_update"))
    }

    // Bulk of matrix operations is imported
    addMatrixCommonOps(DenseMatrix,T)

    // label lets you give a precise name to a particular variant of an overloaded op (typically so that you can refer to it in external code)
    // we use the following labels to optionally override the ops by calling BLAS
    label(lookupOverloaded("DenseMatrix","*",1), "densematrix_matmult")
    label(lookupOverloaded("DenseMatrix","*",2), "densematrix_sparse_matmult")
    label(lookupOverloaded("DenseMatrix","*",3), "densematrix_matvecmult")
    label(lookupOverloaded("DenseMatrix","*",4), "densematrix_sparse_matvecmult")

    // Add DenseMatrix to Arith
    val Arith = lookupGrp("Arith").asInstanceOf[Rep[DSLTypeClass]]
    val DenseMatrixArith = tpeClassInst("ArithDenseMatrix", T withBound TArith, Arith(DenseMatrix(T)))
    infix (DenseMatrixArith) ("zero", T withBound TArith, DenseMatrix(T) :: DenseMatrix(T)) implements composite ${ (unit(0)::$0.numRows,unit(0)::$0.numCols) { (i,j) => implicitly[Arith[T]].empty } }
    infix (DenseMatrixArith) ("empty", T withBound TArith, Nil :: DenseMatrix(T)) implements composite ${ densematrix_fromarray[T](array_empty_imm[T](unit(0)),unit(0),unit(0)) }
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

    infix (DenseMatrix) ("-", Nil, (MInt,DenseMatrix(MInt)) :: DenseMatrix(MInt)) implements redirect ${ densematrix_map[Int,Int]($1, e => forge_int_minus($0,e)) }
    infix (DenseMatrix) ("-", Nil, (MInt,DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_map[Float,Float]($1, e => forge_float_minus($0.toFloat,e)) }
    infix (DenseMatrix) ("-", Nil, (MInt,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_map[Double,Double]($1, e => forge_double_minus($0.toDouble,e)) }
    infix (DenseMatrix) ("-", Nil, (MFloat,DenseMatrix(MInt)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_map[Int,Float]($1, e => forge_float_minus($0,e.toFloat)) }
    infix (DenseMatrix) ("-", Nil, (MFloat,DenseMatrix(MFloat)) :: DenseMatrix(MFloat)) implements redirect ${ densematrix_map[Float,Float]($1, e => forge_float_minus($0,e)) }
    infix (DenseMatrix) ("-", Nil, (MFloat,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_map[Double,Double]($1, e => forge_double_minus($0.toDouble,e)) }
    infix (DenseMatrix) ("-", Nil, (MDouble,DenseMatrix(MInt)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_map[Int,Double]($1, e => forge_double_minus($0,e.toDouble)) }
    infix (DenseMatrix) ("-", Nil, (MDouble,DenseMatrix(MFloat)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_map[Float,Double]($1, e => forge_double_minus($0,e.toDouble)) }
    infix (DenseMatrix) ("-", Nil, (MDouble,DenseMatrix(MDouble)) :: DenseMatrix(MDouble)) implements redirect ${ densematrix_map[Double,Double]($1, e => forge_double_minus($0,e)) }
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

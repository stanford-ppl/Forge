package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait SparseMatrixOps {
  this: OptiLADSL =>

  def importSparseMatrixOps() {
    importSparseMatrixBuildableOps()
    importSparseMatrixFinalOps()
  }

  def importSparseMatrixBuildableOps() {
    val T = tpePar("T")
    val R = tpePar("R")
    val B = tpePar("B")

    val SparseVector = lookupTpe("SparseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val SparseMatrix = lookupTpe("SparseMatrix")
    val SparseMatrixBuildable = lookupTpe("SparseMatrixBuildable")

    // COO format
    data(SparseMatrixBuildable, ("_numRows", MInt), ("_numCols", MInt), ("_data", MArray(T)), ("_rowIndices", MArray(MInt)), ("_colIndices", MArray(MInt)), ("_nnz", MInt))

    static (SparseMatrix) ("apply", T, (MInt, MInt) :: SparseMatrixBuildable(T), effect = mutable) implements allocates(SparseMatrixBuildable, ${$0}, ${$1}, ${array_empty[T](unit(32))}, ${array_empty[Int](unit(32))}, ${array_empty[Int](unit(32))}, ${unit(0)})

    compiler (SparseMatrix) ("sparsematrix_coo_alloc_raw", T, MethodSignature(List(
      ("numRows", MInt),
      ("numCols", MInt),
      ("nzElements", MArray(T)),
      ("rowIndices", MArray(MInt)),
      ("colIndices", MArray(MInt)),
      ("nnz", MInt)), SparseMatrixBuildable(T))) implements allocates(SparseMatrixBuildable, ${$0}, ${$1}, ${$2}, ${$3}, ${$4}, ${$5})

    val SparseMatrixBuildableOps = withTpe(SparseMatrixBuildable)
    SparseMatrixBuildableOps {
      /**
       * Accessors
       */
      infix ("numRows") (Nil :: MInt) implements getter(0, "_numRows")
      infix ("numCols") (Nil :: MInt) implements getter(0, "_numCols")
      infix ("nnz") (Nil :: MInt) implements getter(0, "_nnz")
      infix ("size") (Nil :: MInt) implements composite ${ $self.numRows*$self.numCols }

      compiler ("sparsematrix_coo_find_offset") ((("row",MInt),("col",MInt)) :: MInt) implements composite ${
        val rowIndices = sparsematrix_coo_rowindices($self)
        val colIndices = sparsematrix_coo_colindices($self)

        // linear scan, prefer elements further to the right in the case of duplicates
        var i = 0
        var foundAtIndex = -1
        while (i < $self.nnz) {
          if (rowIndices(i) == row && colIndices(i) == col)
            foundAtIndex = i
          i += 1
        }

        foundAtIndex
      }

      infix ("apply") ((("i",MInt),("j",MInt)) :: T) implements composite ${
        println("[optila warning]: possible performance problem - reading from a sparse matrix COO representation")

        val data = sparsematrix_coo_data($self)
        val offRaw = sparsematrix_coo_find_offset($self,i,j)
        if (offRaw > -1) data(offRaw)
        else defaultValue[T]
      }


      /**
       * Miscellaneous
       */
      // $self.toString doesn't work in Delite, since there is no 'self' instance
      infix ("pprint") (Nil :: MUnit, TStringable(T), effect = simple) implements composite ${ println($self.makeStr + "\\n") }

      infix ("makeString") (Nil :: MString, TStringable(T)) implements single ${
        val rowIndices = sparsematrix_coo_rowindices($self)
        val colIndices = sparsematrix_coo_colindices($self)
        val data = sparsematrix_coo_data($self)
        var s = ""

        if ($self == null) {
          s = "null"
        }
        else if ($self.nnz == 0) {
          s = "[ ]"
        }
        else {
          // COO is not stored in order, so just output coordinates as a list
          for (i <- 0 until $self.nnz-1) {
            if (rowIndices(i) > -1)
              s = s + "((" + rowIndices(i) + ", " + colIndices(i) + "), " + data(i).makeStr + ")\\n"
          }
          s = s + "((" + rowIndices($self.nnz-1) + ", " + colIndices($self.nnz-1) + "), " + data($self.nnz-1).makeStr + ")\\n"
        }
        s
      }

      infix ("toString") (Nil :: MString) implements single ${
        val rowIndices = sparsematrix_coo_rowindices($self)
        val colIndices = sparsematrix_coo_colindices($self)
        val data = sparsematrix_coo_data($self)
        var s = ""

        if ($self == null) {
          s = "null"
        }
        else if ($self.nnz == 0) {
          s = "[ ]"
        }
        else {
          // COO is not stored in order, so just output coordinates as a list
          for (i <- 0 until $self.nnz-1) {
            if (rowIndices(i) > -1)
              s = s + "((" + rowIndices(i) + ", " + colIndices(i) + "), " + optila_fmt_str(data(i)) + ")\\n"
          }
          s = s + "((" + rowIndices($self.nnz-1) + ", " + colIndices($self.nnz-1) + "), " + optila_fmt_str(data($self.nnz-1)) + ")\\n"
        }
        s
      }

      infix ("mutable") (Nil :: SparseMatrixBuildable(T), effect = mutable, aliasHint = copies(0)) implements
        allocates(SparseMatrixBuildable, ${sparsematrixbuildable_numrows($0)}, ${sparsematrixbuildable_numcols($0)}, ${array_clone(sparsematrix_coo_data($0))}, ${array_clone(sparsematrix_coo_colindices($0))}, ${array_clone(sparsematrix_coo_rowindices($0))}, ${sparsematrixbuildable_nnz($0)})

      /**
       * Data operations
       */
      compiler ("sparsematrix_coo_data") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("sparsematrix_coo_rowindices") (Nil :: MArray(MInt)) implements getter(0, "_rowIndices")
      compiler ("sparsematrix_coo_colindices") (Nil :: MArray(MInt)) implements getter(0, "_colIndices")
      compiler ("sparsematrix_coo_set_numrows") (MInt :: MUnit, effect = write(0)) implements setter(0, "_numRows", ${$1})
      compiler ("sparsematrix_coo_set_numcols") (MInt :: MUnit, effect = write(0)) implements setter(0, "_numCols", ${$1})
      compiler ("sparsematrix_coo_set_data") (MArray(T) :: MUnit, effect = write(0)) implements setter(0, "_data", ${$1})
      compiler ("sparsematrix_coo_set_rowindices") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_rowIndices", ${$1})
      compiler ("sparsematrix_coo_set_colindices") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_colIndices", ${$1})
      compiler ("sparsematrix_coo_set_nnz") (MInt :: MUnit, effect = write(0)) implements setter(0, "_nnz", ${$1})

      infix ("update") ((MInt,MInt,T) :: MUnit, effect = write(0)) implements composite ${
        // duplicates are allowed, so don't bother checking if the value already exists
        // the last value in the array (furthest to the right is the true value)

        // FIXME: if this updated an existing value, nnz is wrong...
        // without scanning, we won't know whether or not nnz should be incremented
        // one (perhaps bad) option: don't expose nnz in SparseMatrixBuildable interface
        // another option: rename nnz to internalSize, and implement nnz to do a scan when called (has to deal with dupes)
        $self.append($1,$2,$3,true)
      }

      infix ("append") (MethodSignature(List(("i",MInt),("j",MInt),("y",T),("alwaysWrite",MBoolean,"unit(true)")), MUnit), effect = write(0)) implements single ${
        val shouldAppend = alwaysWrite || (y != defaultValue[T])  // conditional evaluated at staging time
        if (shouldAppend) {
          sparsematrix_coo_ensureextra($self, 1)
          array_update(sparsematrix_coo_data($self), $self.nnz, y)
          array_update(sparsematrix_coo_rowindices($self), $self.nnz, i)
          array_update(sparsematrix_coo_colindices($self), $self.nnz, j)
          sparsematrix_coo_set_nnz($self, $self.nnz+1)
        }
      }

      infix ("<<=") (SparseVector(T) :: MUnit, effect = write(0)) implements composite ${ $self.insertRow($self.numRows, $1) }
      // infix ("<<=") (SparseMatrix(T) :: MUnit, effect = write(0)) implements composite ${ $self.insertAllRows($self.numRows, $1) }
      infix ("<<|=") (SparseVector(T) :: MUnit, effect = write(0)) implements composite ${ $self.insertCol($self.numCols, $1) }
      // infix ("<<|=") (SparseMatrix(T) :: MUnit, effect = write(0)) implements composite ${ $self.insertAllCols($self.numCols, $1) }

      infix ("insertRow") ((("pos",MInt),("y",SparseVector(T))) :: MUnit, effect=write(0)) implements single ${
        if ($self.size == 0) sparsematrix_coo_set_numcols($self, y.length)
        val rowIndices = sparsematrix_coo_rowindices($self)
        for (i <- 0 until $self.nnz) {
          if (rowIndices(i) >= pos)
            array_update(rowIndices, i, rowIndices(i)+1)
        }
        for (j <- 0 until y.nnz) {
          $self.append(pos, sparsevector_raw_indices(y).apply(j), sparsevector_raw_data(y).apply(j))
        }
        sparsematrix_coo_set_numrows($self, $self.numRows+1)
      }

      // TODO
      // infix ("insertAllRows") ((("pos",MInt),("xs",SparseMatrix(T))) :: MUnit, effect=write(0)) implements single ${
      //   if ($self.size == 0) sparsematrix_coo_set_numcols($self, xs.numCols)
      //   val rowIndices = sparsematrix_coo_rowindices($self)
      //   for (i <- 0 until $self.nnz) {
      //     if (rowIndices(i) >= pos)
      //       array_update(rowIndices, i, rowIndices(i)+xs.numRows)
      //   }
      //   // TODO: need to iterate over underlying SparseMatrix CSR arrays directly
      //   for (i <- 0 until xs.numRows) {
      //     for (j <- 0 until xs.numCols) {
      //       $self.append(pos+i, j, xs(i,j))
      //     }
      //   }
      //   sparsematrix_coo_set_numrows($self, $self.numRows+xs.numRows)
      // }

      infix ("insertCol") ((("pos",MInt),("y",SparseVector(T))) :: MUnit, effect=write(0)) implements single ${
        if ($self.size == 0) sparsematrix_coo_set_numrows($self, y.length)
        val colIndices = sparsematrix_coo_colindices($self)
        for (i <- 0 until $self.nnz) {
          if (colIndices(i) >= pos)
            array_update(colIndices, i, colIndices(i)+1)
        }
        for (i <- 0 until y.nnz) {
          $self.append(sparsevector_raw_indices(y).apply(i), pos, sparsevector_raw_data(y).apply(i))
        }
        sparsematrix_coo_set_numcols($self, $self.numCols+1)
      }

      // TODO
      // infix ("insertAllCols") ((("pos",MInt),("xs",SparseMatrix(T))) :: MUnit, effect=write(0)) implements composite ${
      //   if ($self.size == 0) sparsematrix_coo_set_numrows($self, xs.numRows)
      //   val colIndices = sparsematrix_coo_colindices($self)
      //   for (i <- 0 until $self.nnz) {
      //     if (colIndices(i) >= pos)
      //       array_update(colIndices, i, colIndices(i)+xs.numCols)
      //   }
      //   // TODO: need to iterate over underlying SparseMatrix CSR arrays directly
      //   for (i <- 0 until xs.numRows) {
      //     for (j <- 0 until xs.numCols) {
      //       $self.append(i, pos+j, xs(i,j))
      //     }
      //   }
      //   sparsematrix_coo_set_numcols($self, $self.numCols+xs.numCols)
      // }

      infix ("removeRow") (("pos",MInt) :: MUnit, effect = write(0)) implements composite ${ $self.removeRows($pos, 1) }
      infix ("removeCol") (("pos",MInt) :: MUnit, effect = write(0)) implements composite ${ $self.removeCols($pos, 1) }

      infix ("removeRows") ((("pos",MInt),("num",MInt)) :: MUnit, effect=write(0)) implements single ${
        // FIXME: this will also leave nnz in a bad state
        val rowIndices = sparsematrix_coo_rowindices($self)
        for (i <- 0 until $self.nnz) {
          if (rowIndices(i) >= pos && rowIndices(i) < pos+num) {
            // remove by setting it to invalid
            array_update(rowIndices, i, -1)
            array_update(sparsematrix_coo_colindices($self), i, -1)
            // array_update(sparsematrix_coo_data($self), i, defaultValue[T])
          }
          else if (rowIndices(i) >= pos+num)
            array_update(rowIndices, i, rowIndices(i)-num)
        }
        sparsematrix_coo_set_numrows($self, $self.numRows-num)
      }

      infix ("removeCols") ((("pos",MInt),("num",MInt)) :: MUnit, effect=write(0)) implements single ${
        // FIXME: this will also leave nnz in a bad state
        val colIndices = sparsematrix_coo_colindices($self)
        for (i <- 0 until $self.nnz) {
          if (colIndices(i) >= pos && colIndices(i) < pos+num) {
            // remove by setting it to invalid
            array_update(colIndices, i, -1)
            array_update(sparsematrix_coo_rowindices($self), i, -1)
            // array_update(sparsematrix_coo_data($self), i, defaultValue[T])
          }
          else if (colIndices(i) >= pos+num)
            array_update(colIndices, i, colIndices(i)-num)
        }
        sparsematrix_coo_set_numcols($self, $self.numCols-num)
      }

      compiler ("sparsematrix_coo_ensureextra") (("extra",MInt) :: MUnit, effect = write(0)) implements single ${
        if (array_length(sparsematrix_coo_data($self)) - $self.nnz < extra) {
          sparsematrix_coo_realloc($self, $self.nnz + extra)
        }
      }

      compiler ("sparsematrix_coo_realloc") (("minLen",MInt) :: MUnit, effect = write(0)) implements single ${
        val data = sparsematrix_coo_data($self)
        val rowIndices = sparsematrix_coo_rowindices($self)
        val colIndices = sparsematrix_coo_colindices($self)
        var n = max(4, array_length(data) * 2)
        while (n < minLen) n = n*2
        val outData = array_empty[T](n)
        val outRowIndices = array_empty[Int](n)
        val outColIndices = array_empty[Int](n)
        array_copy(data, 0, outData, 0, $self.nnz)
        array_copy(rowIndices, 0, outRowIndices, 0, $self.nnz)
        array_copy(colIndices, 0, outColIndices, 0, $self.nnz)
        sparsematrix_coo_set_data($self, outData.unsafeImmutable)
        sparsematrix_coo_set_rowindices($self, outRowIndices.unsafeImmutable)
        sparsematrix_coo_set_colindices($self, outColIndices.unsafeImmutable)
      }


      /**
       * Conversion to CSR
       */
      infix ("finish") (Nil :: SparseMatrix(T)) implements composite ${ coo_to_csr($self) }

      compiler ("coo_to_csr") (Nil :: SparseMatrix(T)) implements composite ${
        if (coo_ordered($self, $self.nnz, sparsematrix_coo_rowindices($self),sparsematrix_coo_colindices($self)))
          coo_to_csr_ordered($self)
        else
          coo_to_csr_unordered($self)
      }

      compiler ("coo_ordered") ((("nnz",MInt),("rowIndices",MArray(MInt)),("colIndices",MArray(MInt))) :: MBoolean) implements composite ${
        var i = 0
        var lastRow = 0
        var lastCol = 0
        var outOfOrder = false
        while (i < nnz && !outOfOrder) {
          if (rowIndices(i) < lastRow)
            outOfOrder = true
          if (rowIndices(i) == lastRow && colIndices(i) <= lastCol)
            outOfOrder = true
          lastRow = rowIndices(i)
          lastCol = colIndices(i)
          i += 1
        }
        !outOfOrder
      }

      compiler ("coo_to_csr_ordered") (Nil :: SparseMatrix(T)) implements composite ${
        val data = sparsematrix_coo_data($self)
        val rowIndices = sparsematrix_coo_rowindices($self)
        val colIndices = sparsematrix_coo_colindices($self)

        val outData = array_empty[T]($self.nnz)
        val outColIndices = array_empty[Int]($self.nnz)
        val outRowPtr = array_empty[Int]($self.numRows+1)

        var i = 0
        while (i < $self.nnz) {
          array_update(outColIndices, i, colIndices(i))
          array_update(outData, i, data(i))
          array_update(outRowPtr, rowIndices(i)+1, outRowPtr(rowIndices(i)+1)+1)
          i += 1
        }

        coo_to_csr_finalize($self,outData,outColIndices,outRowPtr,$self.nnz)
      }

      compiler ("coo_to_csr_unordered") (Nil :: SparseMatrix(T)) implements composite ${
        val data = sparsematrix_coo_data($self)
        val rowIndices = sparsematrix_coo_rowindices($self)
        val colIndices = sparsematrix_coo_colindices($self)

        // First, remove deleted elements, which are represented by a negative index
        val remainingElems = (0::$self.nnz).filter(i => rowIndices(i) >= 0)

        // Sort by column and then row, so that cols are sorted, but row takes precedence
        val sortedElems = remainingElems.sortBy(i => colIndices(i)).sortBy(i => rowIndices(i))
        // val sortedElems = remainingElems.sortBy(i => (rowIndices(i).toLong << 32) + colIndices(i).toLong)

        // Write to output in sorted order without duplicates (left to right, top to bottom)
        val outData = array_empty[T](sortedElems.length)
        val outColIndices = array_empty[Int](sortedElems.length)
        val outRowPtr = array_empty[Int]($self.numRows+1)

        var i = 0
        var nnz = 0
        while (i < sortedElems.length) {
          // Check for duplicates, relying on sorted order
          var newElem = false
          val next = sortedElems(i)
          val prev = if (i > 0) { var z = i; sortedElems(z-1) } else 0
          if (nnz == 0 || rowIndices(next) != rowIndices(prev) || colIndices(next) != colIndices(prev)) {
            nnz += 1
            newElem = true
          }

          array_update(outData, nnz-1, data(next))
          if (newElem) {
            array_update(outColIndices, nnz-1, colIndices(next))
            array_update(outRowPtr, rowIndices(next)+1, outRowPtr(rowIndices(next)+1)+1)
          }
          i += 1
        }

        coo_to_csr_finalize($self,outData,outColIndices,outRowPtr,nnz)
      }

      compiler ("coo_to_csr_finalize") ((("outData",MArray(T)),("outColIndices",MArray(MInt)),("outRowPtr",MArray(MInt)),("outNnz",MInt)) :: SparseMatrix(T)) implements single ${
        // finalize rowPtr
        var i = 0
        var acc = 0
        while (i < $self.numRows) {
          acc += outRowPtr(i)
          array_update(outRowPtr, i, acc)
          i += 1
        }
        array_update(outRowPtr, $self.numRows, outNnz)

        // -- debug
        // println("indices.length: " + array_length(indices))
        // println("input to coo->csr: ")
        // println("data: " + data)
        // println("colIndices: " + colIndices)
        // println("rowIndices: " + rowIndices)
        //
        // println("output of coo->csr: ")
        // println("data: " + outData)
        // println("colIndices: " + outColIndices)
        // println("rowPtr: " + outRowPtr)

        sparsematrix_csr_alloc_raw[T]($self.numRows,$self.numCols,outData.unsafeImmutable,outColIndices.unsafeImmutable,outRowPtr.unsafeImmutable,outNnz)
      }
    }
  }

  def importSparseMatrixFinalOps() {
    val T = tpePar("T")
    val R = tpePar("R")
    val B = tpePar("B")

    val DenseVector = lookupTpe("DenseVector")
    val IndexVector = lookupTpe("IndexVector")
    val IndexWildcard = lookupTpe("IndexWildcard", stage = compile)
    val DenseVectorView = lookupTpe("DenseVectorView")
    val SparseVector = lookupTpe("SparseVector")
    val SparseVectorView = lookupTpe("SparseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val SparseMatrix = lookupTpe("SparseMatrix")
    val SparseMatrixBuildable = lookupTpe("SparseMatrixBuildable")

    // data fields
    data(SparseMatrix, ("_numRows", MInt), ("_numCols", MInt), ("_data", MArray(T)), ("_colIndices",MArray(MInt)), ("_rowPtr",MArray(MInt)), ("_nnz",MInt))

    // static methods
    static (SparseMatrix) ("fromElements", T, MethodSignature(List(
      ("numRows", MInt),
      ("numCols", MInt),
      ("nzElements", DenseVector(T)),
      ("nzRowIndices", DenseVector(MInt)),
      ("nzColIndices", DenseVector(MInt))), SparseMatrix(T)), effect = mutable) implements composite ${

      val coo = sparsematrix_coo_alloc_raw($0, $1, densevector_raw_data($2), densevector_raw_data($3), densevector_raw_data($4), densevector_length($2))
      val csr = coo.finish
      csr
    }

    static (SparseMatrix) ("diag", T withBound TArith, (MInt, SparseVector(T)) :: SparseMatrix(T)) implements composite ${
      val out = SparseMatrix[T]($0,$0)
      // updates append, and must be sequential
      for (i <- 0 until $0) {
        out(i,i) = $1(i)
      }
      out.finish
    }

    static (SparseMatrix) ("identity", Nil, (MInt,MInt) :: SparseMatrix(MDouble)) implements composite ${
      val out = SparseMatrix[Double]($0,$0)
      for (i <- 0 until $0) {
        out(i,i) = 1.0
      }
      out.finish
    }

    static (SparseMatrix) ("identity", Nil, MInt :: SparseMatrix(MDouble)) implements redirect ${ SparseMatrix.identity($0,$0) }

    // helper
    compiler (SparseMatrix) ("sparsematrix_csr_alloc_raw", T, MethodSignature(List(MInt, MInt, MArray(T), MArray(MInt), MArray(MInt), MInt), SparseMatrix(T))) implements allocates(SparseMatrix, ${$0}, ${$1}, ${$2}, ${$3}, ${$4}, ${$5})

    // this is a Forge quirk -- we cannot allocate an instantiated generic type using "allocates"; instead we have to use redirect or composite
    // static (SparseMatrix) ("zeros", Nil, (MInt,MInt) :: SparseMatrix(MDouble)) implements allocates (SparseMatrix(MDouble), ${$0}, ${$1}, ${array_empty[Double](unit(0))}, ${array_empty[Int](unit(0))}, ${array_empty[Int](unit(0))}, ${unit(0)})
    static (SparseMatrix) ("zeros", Nil, (MInt,MInt) :: SparseMatrix(MDouble)) implements composite ${ sparsematrix_csr_alloc_raw($0, $1, array_empty[Double](unit(0)), array_empty[Int](unit(0)), array_empty[Int]($0+unit(1)), unit(0)) }
    static (SparseMatrix) ("zerosf", Nil, (MInt,MInt) :: SparseMatrix(MFloat)) implements composite ${ sparsematrix_csr_alloc_raw($0, $1, array_empty[Float](unit(0)), array_empty[Int](unit(0)), array_empty[Int]($0+unit(1)), unit(0)) }

    compiler (SparseMatrix) ("sparsematrix_rand", T, (("numRows",MInt),("numCols",MInt),("sparsity",MDouble),("gen",MInt ==> T)) :: SparseMatrix(T)) implements composite ${
      val density = 1.0 - sparsity
      val size = numRows.toLong*numCols.toLong
      val nnz = floor(density*size)
      val nz = (0::nnz) { i => gen(i) }

      // Select row and column indices at random, but remove duplicates and keep selecting until we reach nnz
      // This could be pretty slow if the matrix is small or sparsity is low.
      // val indices = ((0::nnz) { i => pack((randomInt(numRows), randomInt(numCols))) }).distinct
      // while (indices.length != nnz) {
      //   val append = (0::nnz-indices.length) { i => pack((randomInt(numRows), randomInt(numCols))) }
      //   indices = (indices << append).distinct
      // }
      // val rowIndices = indices.map(_._1)
      // val colIndices = indices.map(_._2)

      // Duplicates will be removed during construction; faster than above, but we may not achieve the desired
      // sparsity if the matrix is small or sparsity is low.
      val rowIndices = (0::nnz) { i => randomInt(numRows) }
      val colIndices = (0::nnz) { i => randomInt(numCols) }
      SparseMatrix.fromElements(numRows, numCols, nz, rowIndices, colIndices)
    }

    static (SparseMatrix) ("rand", Nil, (MInt,MInt,MDouble) :: SparseMatrix(MDouble)) implements composite ${ sparsematrix_rand[Double]($0, $1, $2, i => random[Double]) }
    static (SparseMatrix) ("randf", Nil, (MInt,MInt,MDouble) :: SparseMatrix(MFloat)) implements composite ${ sparsematrix_rand[Float]($0, $1, $2, i => random[Float]) }
    static (SparseMatrix) ("randn", Nil, (MInt,MInt,MDouble) :: SparseMatrix(MDouble)) implements composite ${ sparsematrix_rand[Double]($0, $1, $2, i => randomGaussian) }
    static (SparseMatrix) ("randnf", Nil, (MInt,MInt,MDouble) :: SparseMatrix(MFloat)) implements composite ${ sparsematrix_rand[Float]($0, $1, $2, i => randomGaussian.toFloat) }

    // these are provided for convenience for MATLAB-ians
    // direct (SparseMatrix) ("diag", T, SparseMatrix(T) :: SparseVector(T)) implements redirect ${ $0.diag }
    // direct (SparseMatrix) ("triu", T withBound TArith, SparseMatrix(T) :: SparseMatrix(T)) implements redirect ${ $0.triu }
    // direct (SparseMatrix) ("tril", T withBound TArith, SparseMatrix(T) :: SparseMatrix(T)) implements redirect ${ $0.tril }

    val SparseMatrixOps = withTpe (SparseMatrix)
    SparseMatrixOps {
      /**
       * Accessors
       */
      infix ("numRows") (Nil :: MInt) implements getter(0, "_numRows")
      infix ("numCols") (Nil :: MInt) implements getter(0, "_numCols")
      infix ("size") (Nil :: MInt) implements composite ${ $self.numRows*$self.numCols }
      infix ("nnz") (Nil :: MInt) implements getter(0, "_nnz")

      infix ("nz") (MethodSignature(List(("asRow",MBoolean,"unit(true)")), DenseVector(T))) implements composite ${ densevector_alloc_raw($self.nnz, $1, sparsematrix_csr_data($self)) }

      compiler ("sparsematrix_csr_find_offset") ((("row",MInt),("col",MInt)) :: MInt) implements composite ${
        val rowPtr = sparsematrix_csr_rowptr($self)
        val colIndices = sparsematrix_csr_colindices($self)
        // colIndices will be sorted between [rowPtr(row), rowPtr(row+1))
        bsearch(colIndices, rowPtr(row), rowPtr(row+1)-1, col)
      }

      infix ("apply") ((MInt,MInt) :: T) implements composite ${
        val data = sparsematrix_csr_data($self)
        val offRaw = sparsematrix_csr_find_offset($self, $1, $2)
        if (offRaw > -1) data(offRaw) else defaultValue[T]
      }

      infix ("apply") (MInt :: SparseVectorView(T)) implements redirect ${ $self.getRow($1) }

      // orientation of IndexVector in apply does not matter - use getCols or 2d apply to slice cols. This is so we can use n::m syntax
      // to slice rows, while still retaining our convention of row vectors being the default (e.g. for matrix construction)
      infix ("apply") (IndexVector :: SparseMatrix(T)) implements redirect ${ $self.getRows($1) }
      infix ("apply") ((IndexVector, IndexWildcard) :: SparseMatrix(T)) implements redirect ${ $self.getRows($1) }

      infix ("apply") ((("rows", IndexVector), ("cols", IndexVector)) :: SparseMatrix(T)) implements composite ${
        // could avoid the logical access and COO <-> CSR conversion here by slicing the underlying CSR array directly
        val out = SparseMatrix[T](rows.length, cols.length)
        for (i <- 0 until rows.length) {
          // FIXME: weird scalac errors (not found: value row) if we don't explicitly specify type here
          val row: Rep[SparseVectorView[T]] = $self(rows(i))
          for (j <- 0 until cols.length) {
            val col = cols(j)
            if (row(col) != defaultValue[T]) {
              out(i,j) = row(col)
            }
          }
        }
        out.finish
      }
      infix ("apply") ((IndexWildcard, IndexVector) :: SparseMatrix(T)) implements redirect ${ $self.getCols($2) }

      infix ("rowIndices") (Nil :: IndexVector) implements single ${
        // for each data index, compute the row associated with it

        // ex.: 8 rows
        // rowPtr: 0 0 0 1 1 5 5 10
        // colIndex 0 is row 2
        //        1-4 is row 4
        //        5-9 is row 6
        //         10 is row 7

        val rowPtr = sparsematrix_csr_rowptr($self)
        val nnz = rowPtr(array_length(rowPtr)-1)
        val rows = array_empty[Int](nnz)
        var i = 0
        var oldRow = rowPtr(0)
        while (i < array_length(rowPtr)) {
          val nextRow = rowPtr(i)
          if (nextRow != oldRow) {
            for (j <- oldRow until nextRow) {
              rows(j) = i-1
            }
          }
          oldRow = nextRow
          i += 1
        }
        indexvector_fromarray(rows.unsafeImmutable, false)
      }

      infix ("colIndices") (Nil :: IndexVector) implements composite ${
        // Make sure to trim the raw array down to nnz size before creating the IndexVector
        val rawColIndices = sparsematrix_csr_colindices($self)
        IndexVector((0::$self.nnz) { i => rawColIndices(i) })
      }

      // FIXME: more efficient way to get nzRows/nzCols?
      infix ("nzRows") (Nil :: IndexVector) implements redirect ${ IndexVector($self.rowIndices.distinct) }
      infix ("nzCols") (Nil :: IndexVector) implements redirect ${ IndexVector($self.colIndices.distinct) }

      // vview is a "contains" because the view points-to the matrix; dereferencing the view returns the matrix.
      // it is not "extracts", because it is not returning any matrix elements.
      compiler ("sparsematrix_vview") ((MLong, MInt, MInt, MBoolean) :: SparseVectorView(T), aliasHint = contains(0)) implements single ${ SparseVectorView[T]($self, $1, $2, $3, $4) } // read-only right now
      infix ("getRow") (MInt :: SparseVectorView(T)) implements composite ${ sparsematrix_vview($self, $1.toLong*$self.numCols, 1, $self.numCols, true) }

      infix ("getRows") (IndexVector :: SparseMatrix(T)) implements composite ${
        // could avoid the COO <-> CSR conversion here by slicing the underlying CSR array directly
        val out = SparseMatrix[T]($1.length, $self.numCols)
        for (i <- 0 until $1.length) {
          // FIXME: weird scalac errors (not found: value row) if we don't explicitly specify type here
          val row: Rep[SparseVectorView[T]] = $self($1(i))
          val rowData = row.nz
          val rowIndices = row.indices
          for (j <- 0 until rowData.length) {
            out(i,rowIndices(j)) = rowData(j)
          }
        }
        out.finish
      }

      infix ("getCol") (MInt :: SparseVectorView(T)) implements composite ${ sparsematrix_vview($self, $1, $self.numCols, $self.numRows, false) }

      infix ("getCols") (IndexVector :: SparseMatrix(T)) implements composite ${
        // could avoid the OO <-> CSR conversion here by slicing the underlying CSR array directly
        val out = SparseMatrix[T]($self.numRows, $1.length)
        for (j <- 0 until $1.length) {
          val col = $self.getCol($1(j))
          val colData = col.nz
          val colIndices = col.indices
          for (i <- 0 until colData.length) {
            out(colIndices(i),j) = colData(i)
          }
        }
        out.finish
      }

      infix ("slice") ((("startRow",MInt),("endRow",MInt),("startCol",MInt),("endCol",MInt)) :: SparseMatrix(T)) implements redirect ${ $self(startRow::endRow, startCol::endCol) }
      infix ("sliceRows") ((("start",MInt),("end",MInt)) :: SparseMatrix(T)) implements redirect ${ $self.getRows(start::end) }
      infix ("sliceCols") ((("start",MInt),("end",MInt)) :: SparseMatrix(T)) implements redirect ${ $self.getCols(start::end) }

      // TODO
      // TODO: generalize the following (and the static diag above) to kth diagonal
      // infix ("diag") (MethodSignature(List(("x",SparseMatrix(T)),("k",MInt,"unit(0)")), SparseVector(T)) implements composite ${
      // infix ("diag") (Nil :: SparseVector(T)) implements composite ${
      //   val indices = (0::$self.numRows) { i => i + i*$self.numCols }
      //   indices.t map { i => sparsematrix_raw_apply($self,i) }
      //  }
      // infix ("triu") (Nil :: SparseMatrix(T), TArith(T)) implements composite ${
      //   (0::$self.numRows, 0::$self.numCols) { (i,j) =>
      //     if (i <= j) $self(i,j) else implicitly[Arith[T]].empty
      //   }
      // }
      // infix ("tril") (Nil :: SparseMatrix(T), TArith(T)) implements composite ${
      //   (0::$self.numRows, 0::$self.numCols) { (i,j) =>
      //     if (i >= j) $self(i,j) else implicitly[Arith[T]].empty
      //   }
      // }


      /**
       * Miscellaneous
       */

      infix ("t") (Nil :: SparseMatrix(T)) implements composite ${
        val rowIndices = $self.rowIndices
        val colIndices = $self.colIndices

        SparseMatrix.fromElements[T](
          $self.numCols,
          $self.numRows,
          $self.nz,
          colIndices,
          rowIndices
        )
      }

      infix ("Clone") (Nil :: SparseMatrix(T), aliasHint = copies(0)) implements composite ${ $self.mapnz(e =>e) }

      infix ("mutable") (Nil :: SparseMatrixBuildable(T), effect = mutable, aliasHint = copies(0)) implements single ${
        val out = SparseMatrix[T]($self.numRows, $self.numCols)
        val data = sparsematrix_csr_data($self)
        val colIndices = sparsematrix_csr_colindices($self)
        val rowIndices = $self.rowIndices

        // must be sequential because writing to COO
        for (i <- 0 until $self.nnz) {
          out(rowIndices(i), colIndices(i)) = data(i)
        }
        out
      }

      infix ("toDense") (Nil :: DenseMatrix(T)) implements composite ${
        val out = DenseMatrix[T]($self.numRows, $self.numCols)
        val rowPtr = sparsematrix_csr_rowptr($self)
        val colIndices = sparsematrix_csr_colindices($self)
        val data = sparsematrix_csr_data($self)

        // parallel (but irregular), disjoint writes to rows
        (0::$self.numRows).foreach { i =>
          for (j <- rowPtr(i) until rowPtr(i+1)) {
            out(i,colIndices(j)) = data(j)
          }
        }

        out.unsafeImmutable
      }

      // $self.toString doesn't work in Delite, since there is no 'self' instance
      infix ("pprint") (Nil :: MUnit, TStringable(T), effect = simple) implements composite ${ println($self.makeStr + "\\n") }

      infix ("makeDimsStr") (Nil :: MString) implements single ${
        $self.numRows + " x " + $self.numCols + ", " + $self.nnz + " nnz"
      }

      infix ("makeString") (Nil :: MString, TStringable(T)) implements composite ${
        val rowPtr = sparsematrix_csr_rowptr($self)
        val colIndices = sparsematrix_csr_colindices($self)
        val data = sparsematrix_csr_data($self)
        var s = ""

        if ($self == null) {
          s = "null"
        }
        else if ($self.nnz < 1) {
          s = "[ ]"
        }
        else {
          for (i <- 0 until $self.numRows) {
            val nnz = rowPtr(i+1) - rowPtr(i)
            if (nnz > 0) {
              s = s + "(" + i + "): "
              for (j <- rowPtr(i) until rowPtr(i+1)-1) {
                s = s + "(" + colIndices(j) + ", " + data(j).makeStr + "), "
              }
              val lineEnd = if (i == $self.numRows-1) "" else "\\n"
              s = s + "(" + colIndices(rowPtr(i+1)-1) + ", " + data(rowPtr(i+1)-1).makeStr + ")" + lineEnd
            }
          }
          s
        }
        s
      }

      infix ("toString") (Nil :: MString) implements composite ${
        val rowPtr = sparsematrix_csr_rowptr($self)
        val colIndices = sparsematrix_csr_colindices($self)
        val data = sparsematrix_csr_data($self)
        var s = ""

        if ($self == null) {
          s = "null"
        }
        else if ($self.nnz < 1) {
          s = "[ ]"
        }
        else {
          for (i <- 0 until $self.numRows) {
            val nnz = rowPtr(i+1) - rowPtr(i)
            if (nnz > 0) {
              s = s + "(" + i + "): "
              for (j <- rowPtr(i) until rowPtr(i+1)-1) {
                s = s + "(" + colIndices(j) + ", " + optila_fmt_str(data(j)) + "), "
              }
              val lineEnd = if (i == $self.numRows-1) "" else "\\n"
              s = s + "(" + colIndices(rowPtr(i+1)-1) + ", " + optila_fmt_str(data(rowPtr(i+1)-1)) + ")" + lineEnd
            }
          }
        }
        s
      }


      /**
       * Data operations
       */
      compiler ("sparsematrix_csr_data") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("sparsematrix_csr_rowptr") (Nil :: MArray(MInt)) implements getter(0, "_rowPtr")
      compiler ("sparsematrix_csr_colindices") (Nil :: MArray(MInt)) implements getter(0, "_colIndices")
      compiler ("sparsematrix_csr_set_numrows") (MInt :: MUnit, effect = write(0)) implements setter(0, "_numRows", ${$1})
      compiler ("sparsematrix_csr_set_numcols") (MInt :: MUnit, effect = write(0)) implements setter(0, "_numCols", ${$1})
      compiler ("sparsematrix_csr_set_data") (MArray(T) :: MUnit, effect = write(0)) implements setter(0, "_data", ${$1})
      compiler ("sparsematrix_csr_set_rowptr") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_rowPtr", ${$1})
      compiler ("sparsematrix_csr_set_colindices") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_colIndices", ${$1})
      compiler ("sparsematrix_csr_set_nnz") (MInt :: MUnit, effect = write(0)) implements setter(0, "_nnz", ${$1})

      // -- in case we want to allow mutation in the future

      // infix ("update") ((("i",MInt), ("j",MInt), ("y",T)) :: MUnit, effect = write(0)) implements single ${
      compiler ("sparematrix_csr_update") ((("i",MInt), ("j",MInt), ("y",T)) :: MUnit, effect = write(0)) implements single ${
        if (Settings.verbose > 0) println("(performance warning): writing to a sparse matrix CSR representation")

        val offRaw = sparsematrix_csr_find_offset($self,i,j)
        if (offRaw > -1) array_update(sparsematrix_csr_data($self), offRaw, y)
        else {
          if (y != defaultValue[T]) {
            val off = ~offRaw
            sparsematrix_csr_insertspace($self, off, 1)
            array_update(sparsematrix_csr_colindices($self), off, j)
            array_update(sparsematrix_csr_data($self), off, y)
            val rowPtr = sparsematrix_csr_rowptr($self)
            // have to increment every element of rowPtr
            for (row <- i+1 until array_length(rowPtr)) {
              array_update(rowPtr,row,rowPtr(row)+1)
            }
          }
        }
      }

      compiler ("sparsematrix_csr_ensureextra") (("extra",MInt) :: MUnit, effect = write(0)) implements single ${
        if (array_length(sparsematrix_csr_data($self)) - $self.nnz < extra) {
          sparsematrix_csr_realloc($self,$self.nnz + extra)
        }
      }

      compiler ("sparsematrix_csr_realloc") (("minLen",MInt) :: MUnit, effect = write(0)) implements single ${
        val data = sparsematrix_csr_data($self)
        val colIndices = sparsematrix_csr_colindices($self)
        var n = max(4, array_length(data) * 2)
        while (n < minLen) n = n*2
        val outData = array_empty[T](n)
        val outColIndices = array_empty[Int](n)
        array_copy(data, 0, outData, 0, $self.nnz)
        array_copy(colIndices, 0, outColIndices, 0, $self.nnz)
        sparsematrix_csr_set_data($self, outData.unsafeImmutable)
        sparsematrix_csr_set_colindices($self, outColIndices.unsafeImmutable)
      }

      compiler ("sparsematrix_csr_insertspace") ((("pos",MInt), ("len", MInt)) :: MUnit, effect = write(0)) implements single ${
        sparsematrix_csr_ensureextra($self,len)
        val data = sparsematrix_csr_data($self)
        val colIndices = sparsematrix_csr_colindices($self)
        array_copy(data, pos, data, pos + len, $self.nnz - pos)
        array_copy(colIndices, pos, colIndices, pos + len, $self.nnz - pos)
        sparsematrix_csr_set_nnz($self, $self.nnz + len)
      }



      /**
       * Math
       */

       // TODO: how can we parallelize this efficiently?
       //   currently using a sequential version that build the output in one loop

      compiler ("zipMatrixUnion") ((SparseMatrix(B), (T,B) ==> R) :: SparseMatrix(R), addTpePars = (B,R)) implements single ${
        val aData = sparsematrix_csr_data($self)
        val aColIndices = sparsematrix_csr_colindices($self)
        val aRowPtr = sparsematrix_csr_rowptr($self)
        val bData = sparsematrix_csr_data($1)
        val bColIndices = sparsematrix_csr_colindices($1)
        val bRowPtr = sparsematrix_csr_rowptr($1)

        val outRowPtr = array_empty[Int]($self.numRows+1)
        val outColIndices = array_empty[Int]($self.nnz + $1.nnz) // upper bound
        val outData = array_empty[R]($self.nnz + $1.nnz)

        var aOldRow = aRowPtr(0)
        var bOldRow = bRowPtr(0)
        var nnz = 0
        var i = 0

        while (i < $self.numRows+1) {
          // union of colIndicesA and colIndicesB at row i
          if (aRowPtr(i) != aOldRow || bRowPtr(i) != bOldRow) {
            nnz = zipUnion(nnz, aOldRow, aRowPtr(i), aColIndices, aData, bOldRow, bRowPtr(i), bColIndices, bData, outColIndices, outData, $2)
          }
          outRowPtr(i) = nnz
          aOldRow = aRowPtr(i)
          bOldRow = bRowPtr(i)
          i += 1
        }

        sparsematrix_csr_alloc_raw[R]($self.numRows, $self.numCols, outData.unsafeImmutable, outColIndices.unsafeImmutable, outRowPtr.unsafeImmutable, nnz)
      }

      compiler ("zipMatrixIntersect") ((SparseMatrix(B), (T,B) ==> R) :: SparseMatrix(R), addTpePars = (B,R)) implements single ${
        val aData = sparsematrix_csr_data($self)
        val aColIndices = sparsematrix_csr_colindices($self)
        val aRowPtr = sparsematrix_csr_rowptr($self)
        val bData = sparsematrix_csr_data($1)
        val bColIndices = sparsematrix_csr_colindices($1)
        val bRowPtr = sparsematrix_csr_rowptr($1)

        val outRowPtr = array_empty[Int]($self.numRows+1)
        val outColIndices = array_empty[Int]($self.nnz + $1.nnz) // upper bound
        val outData = array_empty[R]($self.nnz + $1.nnz)

        var aOldRow = aRowPtr(0)
        var bOldRow = bRowPtr(0)
        var nnz = 0
        var i = 0

        while (i < $self.numRows+1) {
          // union of colIndicesA and colIndicesB at row i
          if (aRowPtr(i) != aOldRow || bRowPtr(i) != bOldRow) {
            nnz = zipIntersect(nnz, aOldRow, aRowPtr(i), aColIndices, aData, bOldRow, bRowPtr(i), bColIndices, bData, outColIndices, outData, $2)
          }
          outRowPtr(i) = nnz
          aOldRow = aRowPtr(i)
          bOldRow = bRowPtr(i)
          i += 1
        }

        sparsematrix_csr_alloc_raw[R]($self.numRows, $self.numCols, outData.unsafeImmutable, outColIndices.unsafeImmutable, outRowPtr.unsafeImmutable, nnz)
      }

      infix ("+") (SparseMatrix(T) :: SparseMatrix(T), TArith(T)) implements composite ${ zipMatrixUnion[T,T,T]($self, $1, (a,b) => a+b) }
      infix ("+") (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense + $1 }
      infix ("+") (T :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense + $1 }

      infix ("-") (SparseMatrix(T) :: SparseMatrix(T), TArith(T)) implements composite ${ zipMatrixUnion[T,T,T]($self, $1, (a,b) => a-b) }
      infix ("-") (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense - $1 }
      infix ("-") (T :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense - $1 }

      infix ("*:*") (SparseMatrix(T) :: SparseMatrix(T), TArith(T)) implements composite ${ zipMatrixIntersect[T,T,T]($self, $1, (a,b) => a*b) }
      infix ("*:*") (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense * $1 }
      infix ("*") (T :: SparseMatrix(T), TArith(T)) implements composite ${ $self.mapnz(e => e*$1) }
      infix ("*") (DenseVector(T) :: DenseVector(T), TArith(T)) implements composite ${
        if ($self.numCols != $1.length || $1.isRow) fatal("dimension mismatch: matrix * vector")
        $self.mapRowsToDenseVector { row => row *:* $1 } 
      }

      infix ("*") (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${
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

      infix ("*") (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${
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

      // TODO
      // infix ("*") (SparseMatrix(T) :: SparseMatrix(T), TArith(T)) implements single ${
      //   fassert($self.numCols == $1.numRows, "dimension mismatch: matrix multiply")
      // }

      // infix ("*") (SparseVector(T) :: SparseVector(T), TArith(T)) implements single ${
      //  fassert($self.numCols == $1.length && !$1.isRow, "dimension mismatch: matrix * vector")
      // }

      infix ("/") (SparseMatrix(T) :: SparseMatrix(T), TArith(T)) implements composite ${ zipMatrixIntersect[T,T,T]($self, $1, (a,b) => a/b) } // ignores x / 0 errors...
      infix ("/") (DenseMatrix(T) :: DenseMatrix(T), TArith(T)) implements composite ${ $self.toDense / $1 }
      infix ("/") (T :: SparseMatrix(T), TArith(T)) implements composite ${ $self.mapnz(e => e/$1) }

      infix ("sum") (Nil :: T, TArith(T)) implements composite ${ $self.nz.sum }
      infix ("mean") (Nil :: MDouble, ("conv",T ==> MDouble)) implements composite ${ $self.mapnz(conv).sum / $self.size }
      infix ("abs") (Nil :: SparseMatrix(T), TArith(T)) implements composite ${ $self.mapnz { e => e.abs } }

      infix ("sumRows") (Nil :: SparseVector(T), TArith(T)) implements composite ${ $self.mapRowsToVector { row => sum(row) }}
      infix ("sumCols") (Nil :: SparseVector(T), TArith(T)) implements composite ${ $self.mapColsToVector { col => sum(col) }}

      // TODO: inverse

      /**
       * Ordering
       */
      infix ("min") (Nil :: T, (TOrdering(T), THasMinMax(T))) implements composite ${
        val min = $self.nz.min
        if (min > defaultValue[T] && $self.nnz < $self.size) defaultValue[T] else min
      }

      infix ("max") (Nil :: T, (TOrdering(T), THasMinMax(T))) implements composite ${
        val max = $self.nz.max
        if (max < defaultValue[T] && $self.nnz < $self.size) defaultValue[T] else max
      }

      // FIXME: why is explicit toSparse needed?
      infix ("minRows") (Nil :: SparseVector(T), (TOrdering(T), THasMinMax(T))) implements composite ${ $self.mapRowsToVector { row => min(row.toSparse) }}
      infix ("minCols") (Nil :: SparseVector(T), (TOrdering(T), THasMinMax(T))) implements composite ${ $self.mapColsToVector { col => min(col.toSparse) }}
      infix ("maxRows") (Nil :: SparseVector(T), (TOrdering(T), THasMinMax(T))) implements composite ${ $self.mapRowsToVector { row => max(row.toSparse) }}
      infix ("maxCols") (Nil :: SparseVector(T), (TOrdering(T), THasMinMax(T))) implements composite ${ $self.mapColsToVector { col => max(col.toSparse) }}

      direct ("__equal") (SparseMatrix(T) :: MBoolean) implements composite ${
        if ($self.numRows != $1.numRows || $self.numCols != $1.numCols) false
        else {
          val dataEqual = densevector_alloc_raw($self.nnz, true, sparsematrix_csr_data($self)) == densevector_alloc_raw($1.nnz, true, sparsematrix_csr_data($1))
          val colIndexEqual = densevector_alloc_raw($self.nnz, true, sparsematrix_csr_colindices($self)) == densevector_alloc_raw($1.nnz, true, sparsematrix_csr_colindices($1))
          val rowPtrEqual = densevector_alloc_raw($self.numRows+1, true, sparsematrix_csr_rowptr($self)) == densevector_alloc_raw($1.numRows+1, true, sparsematrix_csr_rowptr($1))
          dataEqual && colIndexEqual && rowPtrEqual
        }
      }

      direct ("__equal") (DenseMatrix(T) :: MBoolean) implements composite ${ $self.toDense == $1 }


      /**
       *  Bulk
       */
      infix ("mapnz") ((T ==> R) :: SparseMatrix(R), addTpePars = R) implements composite ${
        val out = $self.nz.map($1)
        sparsematrix_csr_alloc_raw($self.numRows, $self.numCols, densevector_raw_data(out), sparsematrix_csr_colindices($self), sparsematrix_csr_rowptr($self), $self.nnz)
      }

      infix ("zipnz") (CurriedMethodSignature(List(List(DenseVector(B)), List((T,B) ==> R)), SparseMatrix(R)), addTpePars = (B,R)) implements composite ${
        val out = $self.nz.zip($1) { (a,b) => $2(a,b) }
        sparsematrix_csr_alloc_raw($self.numRows, $self.numCols, densevector_raw_data(out), sparsematrix_csr_colindices($self), sparsematrix_csr_rowptr($self), $self.nnz)
      }

      infix ("foreachnz") ((T ==> MUnit) :: MUnit) implements composite ${ $self.nz.foreach($1) }

      infix ("countnz") ((T ==> MBoolean) :: MInt) implements composite ${ $self.nz.count($1) }

      infix ("mapRowsToVector") ((SparseVectorView(T) ==> R) :: SparseVector(R), addTpePars = R) implements composite ${
        SparseVector.fromFunc($self.numRows, false, $self.nzRows, i => $1($self(i)))
      }

      infix ("mapRowsToDenseVector") ((SparseVectorView(T) ==> R) :: DenseVector(R), addTpePars = R) implements composite ${
         IndexVector(0, $self.numRows, false).map(i => $1($self.getRow(i)))
       }

      infix ("mapColsToVector") ((SparseVectorView(T) ==> R) :: SparseVector(R), addTpePars = R) implements composite ${
        SparseVector.fromFunc($self.numCols, true, $self.nzCols, i => $1($self.getCol(i)))
      }

      infix ("findRows") ((SparseVectorView(T) ==> MBoolean) :: IndexVector) implements composite ${
        IndexVector($self.nzRows.filter(i => $1($self(i))))
      }
      infix ("findCols") ((SparseVectorView(T) ==> MBoolean) :: IndexVector) implements composite ${
        IndexVector($self.nzCols.filter(i => $1($self.getCol(i))))
      }

      infix ("filterRows") ((SparseVectorView(T) ==> MBoolean) :: SparseMatrix(T)) implements composite ${
        $self($self.findRows($1))
      }
      infix ("filterCols") ((SparseVectorView(T) ==> MBoolean) :: SparseMatrix(T)) implements composite ${
        $self.getCols($self.findCols($1))
      }

      infix ("foreachRow") ((SparseVectorView(T) ==> MUnit) :: MUnit, effect = simple) implements composite ${
        $self.nzRows foreach { i => $1($self(i)) }
      }
      infix ("foreachCol") ((SparseVectorView(T) ==> MUnit) :: MUnit, effect = simple) implements composite ${
        $self.nzCols foreach { i => $1($self.getCol(i)) }
      }

      // TODO
      // infix ("mapRows") ((DenseVectorView(T) ==> SparseVector(R)) :: SparseMatrix(R), addTpePars = R) implements composite ${
      //   val out = SparseMatrix[R]($self.numRows, $self.numCols)
      //   (0::$self.numRows) foreach { i =>
      //     out(i) = $1($self(i))
      //   }
      //   out.unsafeImmutable
      // }
      // infix ("mapCols") ((DenseVectorView(T) ==> SparseVector(R)) :: SparseMatrix(R), addTpePars = R) implements composite ${
      //   val out = SparseMatrix[R]($self.numRows, $self.numCols)
      //   (0::$self.numCols) foreach { j =>
      //     out.updateCol(j, $1($self.getCol(j)))
      //   }
      //   out.unsafeImmutable
      // }

      // infix ("reduceRows") (((SparseVector(T),SparseVector(T)) ==> SparseVector(T)) :: SparseVector(T), TArith(T)) implements composite ${
      //   val vv = $self.rowIndices.distinct.map(i => $self(i).toSparse)
      //   vv.reduce((a,b) => $1(a,b))
      // }

      // infix ("reduceCols") (((SparseVector(T),SparseVector(T)) ==> SparseVector(T)) :: SparseVector(T), TArith(T)) implements composite ${
      //   val vv = $self.colIndices.distinct.map(i => $self.getCol(i).toSparse)
      //   vv.reduce((a,b) => $1(a,b))
      // }

      // TODO: waiting for DeliteHashMap to implement these.
      // infix ("groupRowsBy") (((T ==> K)) :: SparseVector(SparseMatrixT)))
      // infix ("groupColsBy") (((T ==> K)) :: SparseVector(SparseMatrixT)))
    }

    // should we add primitive sparse op combinations? the extra combinations kill our compile times, but the trade-off is the loss of flexible syntax with sparse math.
  }
}

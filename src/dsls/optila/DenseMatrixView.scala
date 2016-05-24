package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait DenseMatrixViewOps {
  this: OptiLADSL =>

  def importDenseMatrixViewOps() {
    val T = tpePar("T")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrixView = lookupTpe("DenseMatrixView") // tpe("DenseMatrixView", T)
    val DenseMatrix = lookupTpe("DenseMatrix")

    // data fields
    data(DenseMatrixView, ("_data", MArray(T)), ("_startRow", MInt), ("_endRow", MInt), ("_startCol", MInt), ("_endCol", MInt), ("_srcNumRows", MInt), ("_srcNumCols", MInt))

    // static methods
    static (DenseMatrixView) ("apply", T, (MethodSignature(List(MArray(T), MInt, MInt, MInt, MInt, MInt, MInt), DenseMatrixView))) implements allocates(DenseMatrixView, ${$0}, ${$1}, ${$2}, ${$3}, ${$4}, ${$5}, ${$6})

    val DenseMatrixViewOps = withTpe(DenseMatrixView)
    DenseMatrixViewOps {
      internal ("densematrixview_data") (Nil :: MArray(T)) implements getter(0, "_data")
      internal ("densematrixview_startrow") (Nil :: MInt) implements getter(0, "_startRow")
      internal ("densematrixview_endrow") (Nil :: MInt) implements getter(0, "_endRow")
      internal ("densematrixview_startcol") (Nil :: MInt) implements getter(0, "_startCol")
      internal ("densematrixview_endcol") (Nil :: MInt) implements getter(0, "_endCol")
      internal ("densematrixview_srcnumrows") (Nil :: MInt) implements getter(0, "_srcNumRows")
      internal ("densematrixview_srcnumcols") (Nil :: MInt) implements getter(0, "_srcNumCols")

      infix ("numRows") (Nil :: MInt) implements composite ${ densematrixview_endrow($self) - densematrixview_startrow($self) }
      infix ("numCols") (Nil :: MInt) implements composite ${ densematrixview_endcol($self) - densematrixview_startcol($self) }
      infix ("size") (Nil :: MInt) implements composite ${ $self.numRows*$self.numCols }

      infix ("apply") ((MInt,MInt) :: T) implements composite ${
        val rowOffset = densematrixview_startrow($self)+$1
        val colOffset = densematrixview_startcol($self)+$2
        val index = rowOffset*densematrixview_srcnumcols($self)+colOffset
        array_apply(densematrixview_data($self), index)
      }

      infix ("vview") ((("start", MInt), ("stride", MInt), ("length", MInt), ("isRow", MBoolean)) :: DenseVectorView(T), aliasHint = contains(0)) implements single ${
        // read-only right now
        DenseVectorView[T](densematrixview_data($self), $1, $2, $3, $4)
      }
      infix ("mview") ((("startRow", MInt), ("endRow", MInt), ("startCol", MInt), ("endCol", MInt)) :: DenseMatrixView(T), aliasHint = contains(0)) implements single ${
        // read-only right now
        DenseMatrixView[T](densematrixview_data($self), $1, $2, $3, $4, densematrixview_srcnumrows($self), densematrixview_srcnumcols($self))
      }

      infix ("getRow") (MInt :: DenseVectorView(T)) implements composite ${
        val srcRow = densematrixview_startrow($self)+$1
        $self.vview(srcRow*densematrixview_srcnumcols($self)+densematrixview_startcol($self), 1, $self.numCols, true)
      }
      infix ("getCol") (MInt :: DenseVectorView(T)) implements composite ${
        val srcCol = densematrixview_startcol($self)+$1
        $self.vview(densematrixview_startrow($self)*densematrixview_srcnumcols($self)+srcCol, densematrixview_srcnumcols($self), $self.numRows, false)
      }

      infix ("slice") ((("startRow",MInt),("endRow",MInt),("startCol",MInt),("endCol",MInt)) :: DenseMatrixView(T)) implements composite ${
        val currentStartRow = densematrixview_startrow($self)
        val currentStartCol = densematrixview_startcol($self)
        $self.mview(currentStartRow+$1, currentStartRow+$2, currentStartCol+$3, currentStartCol+$4)
      }

      // clones return a DenseMatrix, so that we do not retain the same underlying pointer
      infix ("Clone") (Nil :: DenseMatrix(T)) implements redirect ${ $self.toDense }

      infix ("toDense") (Nil :: DenseMatrix(T)) implements composite ${ $self.map(e => e) }

      direct ("__equal") (DenseMatrix(T) :: MBoolean) implements composite ${ $1 == $self }

      fimplicit ("viewToDense") (Nil :: DenseMatrix(T)) implements composite ${
        if (Settings.verbose > 0) println("(performance warning): automatic conversion from DenseMatrixView to DenseMatrix")
        // Console.println("  at " + quotePos(fresh[Nothing].withPos(List(implicitly[SourceContext]))))
        $self.toDense
      }
      val grpName = if (Config.fastCompile) "$Flat" else "DenseMatrix"
      fimplicit ("chainViewToDenseOps") (Nil :: ephemeralTpe(grpName+"DenseMatrixOpsCls[T]", stage = now)) implements composite ${
        repTo\${grpName}DenseMatrixOpsCls(viewToDense($self))
      }

      direct ("densematrixview_raw_apply") (MInt :: T) implements composite ${
        val (r,c) = unpack(matrix_shapeindex($1, $self.numCols))
        $self(r,c)
      }
      internal ("densematrixview_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("DenseMatrixViews cannot be allocated from a parallel op") }
      internal ("densematrixview_illegalupdate") ((MInt, T) :: MNothing, effect = simple) implements composite ${ fatal("DenseMatrixViews cannot be updated") }

      parallelize as ParallelCollection(T, lookupOp("densematrixview_illegalalloc"), lookupOp("size"), lookupOp("densematrixview_raw_apply"), lookupOp("densematrixview_illegalupdate"))
    }

    // allows us to perform operations without converting to a DenseMatrix first
    addMatrixCommonOps(DenseMatrixView,T)
  }
}

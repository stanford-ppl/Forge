package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait LegacySupport {
  this: OptiLADSL =>

  def importLegacySupport() {
  	val IndexVector = lookupTpe("IndexVector")
  	val DenseVector = lookupTpe("DenseVector")
  	val DenseMatrix = lookupTpe("DenseMatrix")
  	tpeAlias("DenseVectorView", DenseVector)

  	val A = tpePar("A")
  	val K = tpePar("K")
    val V = tpePar("V")

    // --- IO
    val IO = lookupGrp("LAio")
    // Old vector reading syntax
    direct (IO) ("readVector", Nil, ("path",MString) :: DenseVector(MDouble)) implements composite ${
      DenseVector.fromFile(path)
    }
    direct (IO) ("readVector", A, MethodSignature(List(("path", MString), ("schemaBldr", DenseVector(MString) ==> A), ("delim", MString, "\"\\s+\"")), DenseVector(A)), effect = simple) implements composite ${
      DenseVector.fromFile($path, $delim, $schemaBldr) 
    }
    // Old matrix reading syntax
    direct (IO) ("readMatrix", Nil, ("path",MString) :: DenseMatrix(MDouble)) implements composite ${
      DenseMatrix.fromFile(path)
    }
    direct (IO) ("readMatrix", Nil, (("path", MString), ("delim", MString)) :: DenseMatrix(MDouble)) implements composite ${
      DenseMatrix.fromFile($path, $delim)
    }
    direct (IO) ("readMatrix", A, MethodSignature(List(("path", MString), ("schemaBldr", MString ==> A), ("delim", MString, "\"\\s+\"")), DenseMatrix(A)), effect = simple) implements composite ${
      DenseMatrix.fromFile($path, $delim, $schemaBldr) 
    }
    // Old writing syntax
    direct (IO) ("writeVector", A withBound TStringable, (("v", DenseVector(A)), ("path", MString)) :: MUnit, effect = simple) implements composite ${
      $v.writeFile($path)
    }
    direct (IO) ("writeMatrix", A withBound TStringable, MethodSignature(List(("m", DenseMatrix(A)), ("path", MString), ("delim", MString, "\"   \"")), MUnit), effect = simple) implements composite ${
      $m.writeFile($path, $delim) 
    }
    // This is probably never used, but keeping it here for now
    direct (IO) ("deleteFile", Nil, MString :: MUnit, effect = simple) implements codegen($cala, ${
      val f = new java.io.File($0)
      if (f.exists) f.delete()
      ()
    })

    // --- Math
  	val Math = lookupGrp("BasicMath")
  	infix (Math)  ("~^", Nil, (MDouble,MInt) :: MDouble) implements redirect ${ Math.pow($0,$1) }
  	
  	// --- DenseVectors
  	static (DenseVector) ("zeros", Nil, MInt :: DenseVector(MDouble)) implements composite ${
  		fwarn("DenseVector.zeros(len) has been shortened to zeros(len)", 0)
  		zeros($0)
  	}
  	static (DenseVector) ("zerosf", Nil, MInt :: DenseVector(MFloat)) implements composite ${
  		fwarn("DenseVector.zerosf(len) has been shortened to zerosf(len)", 0)
  		zerosf($0)
  	}
  	static (DenseVector) ("ones", Nil, MInt :: DenseVector(MDouble)) implements composite ${
  		fwarn("DenseVector.ones(len) has been shortened to ones(len)", 0)
  		ones($0)
  	}
  	static (DenseVector) ("onesf", Nil, MInt :: DenseVector(MFloat)) implements composite ${
  		fwarn("DenseVector.onesf(len) has been shortened to onesf(len)", 0)
  		onesf($0)
  	}
  	static (DenseVector) ("rand", Nil, MInt :: DenseVector(MDouble)) implements composite ${
  		fwarn("DenseVector.rand(len) has been shortened to rands(len)", 0)
  		rand($0)
  	}
  	static (DenseVector) ("randf", Nil, MInt :: DenseVector(MFloat)) implements composite ${
  		fwarn("DenseVector.randf(len) has been shortened to randsf(len)", 0)
  		randf($0)
  	}
  	static (DenseVector) ("uniform", Nil, MethodSignature(List(("start", MInt), ("step_size", MDouble), ("end", MInt), ("isRow", MBoolean, "true")), DenseVector(MDouble))) implements composite ${
  		fwarn("DenseVector.uniform(start, stride, end, isRow) has been shortened to uniform(start, stride, end, isRow)")
  		uniform($start, $step_size, $end, $isRow)
  	}
  	static (DenseVector) ("flatten", A, ("pieces",DenseVector(DenseVector(A))) :: DenseVector(A)) implements composite ${
  		fwarn("DenseVector.flatten(v) has been shortened to flatten(v)")
  		flatten(pieces)
		}	 

		val DenseVectorLegacyOps = withTpe(DenseVector) 
		DenseVectorLegacyOps {
			infix (":>") (DenseVector(A) :: DenseVector(MBoolean), TOrder(A)) implements composite ${ $self > $1 }
			infix (":<") (DenseVector(A) :: DenseVector(MBoolean), TOrder(A)) implements composite ${ $self < $1 }

      // Added to avoid unnecessary conversions from what was previously DenseVectorView to DenseVector
			infix ("toDense") (Nil :: DenseVector(A)) implements composite ${
				fwarn("toDense call may not be necessary and will likely reduce performance. If a data copy or collation is desired, use Clone instead")
			  $self.Clone
			}
		}
    compiler (DenseVector) ("reduce_and", Nil, DenseVector(MBoolean) :: MBoolean) implements composite ${
      fwarn("reduce_and has been deprecated with the addition of the DenseVector fold operation")
      $0.fold(unit(true)){_&&_}
    }
    compiler (DenseVector) ("reduce_or", Nil, DenseVector(MBoolean) :: MBoolean) implements composite ${
      fwarn("reduce_or has been deprecated with the addition of the DenseVector fold operation")
      $0.fold(unit(false)){_||_}
    }


		// --- Matrices
		static (DenseMatrix) ("zeros", Nil, (MInt,MInt) :: DenseMatrix(MDouble)) implements composite ${
  		fwarn("DenseMatrix.zeros(rows, cols) has been shortened to zeros(rows, cols)", 0)
  		zeros($0,$1)
  	}
  	static (DenseMatrix) ("zerosf", Nil, (MInt,MInt) :: DenseMatrix(MFloat)) implements composite ${
  		fwarn("DenseMatrix.zerosf(rows, cols) has been shortened to zerosf(rows, cols)", 0)
  		zerosf($0,$1)
  	}
  	static (DenseMatrix) ("ones", Nil, (MInt,MInt) :: DenseMatrix(MDouble)) implements composite ${
  		fwarn("DenseMatrix.ones(rows, cols) has been shortened to ones(rows, cols)", 0)
  		ones($0,$1)
  	}
  	static (DenseMatrix) ("onesf", Nil, (MInt,MInt) :: DenseMatrix(MFloat)) implements composite ${
  		fwarn("DenseMatrix.onesf(rows, cols) has been shortened to onesf(rows, cols)", 0)
  		onesf($0,$1)
  	}
  	static (DenseMatrix) ("rand", Nil, (MInt,MInt) :: DenseMatrix(MDouble)) implements composite ${
  		fwarn("DenseMatrix.rand(rows, cols) has been shortened to rand(rows, cols)", 0)
  		rand($0,$1)
  	}
  	static (DenseMatrix) ("randf", Nil, (MInt,MInt) :: DenseMatrix(MFloat)) implements composite ${
  		fwarn("DenseMatrix.randf(rows, cols) has been shortened to randf(rows, cols)", 0)
  		randf($0,$1)
  	}
  	static (DenseMatrix) ("randn", Nil, (MInt,MInt) :: DenseMatrix(MDouble)) implements composite ${
  		fwarn("DenseMatrix.randn(rows, cols) has been shortened to randn(rows, cols)", 0)
  		randn($0,$1)
  	}
  	static (DenseMatrix) ("randnf", Nil, (MInt,MInt) :: DenseMatrix(MFloat)) implements composite ${
  		fwarn("DenseMatrix.randnf(rows, cols) has been shortened to randnf(rows, cols)", 0)
  		randnf($0,$1)
  	}

  	val DenseMatrixLegacyOps = withTpe(DenseMatrix)
  	DenseMatrixLegacyOps {
  		infix (":>") (DenseMatrix(A) :: DenseMatrix(MBoolean), TOrder(A)) implements composite ${ $self > $1 }
  		infix (":<") (DenseMatrix(A) :: DenseMatrix(MBoolean), TOrder(A)) implements composite ${ $self < $1 } 

  		infix ("getRows") (IndexVector :: DenseMatrix(A)) implements composite ${
  			fwarn("getRows has been deprecated. If you want a data copy, use diceRows. If you want a data view, use sliceRows")
  			if (isRange($1)) $self.sliceRows($1)
  			else $self.diceRows($1)
  		}
  		infix ("getCols") (IndexVector :: DenseMatrix(A)) implements composite ${
  			fwarn("getCols has been deprecated. If you want a data copy, use diceCols. If you want a data view, use sliceCols")
  			if (isRange($1)) $self.sliceCols($1)
  			else $self.diceCols($1)
  		}

  		infix ("vview") ((MInt, MInt, MInt, MBoolean) :: DenseVector(A)) implements composite ${ 
  			fwarn("Vector view of underlying matrix data restricts the array flattening approach. You may wish to use slice instead, which produces a matrix-view.")
  			val data = densematrix_raw_data($self).reshapeView($self.size)
        densevector_fromarray1d(data.slice($1, $2, $3), $4)
  		}
  	}

  }
}
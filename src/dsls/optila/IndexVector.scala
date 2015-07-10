package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait IndexVectorOps {
  this: OptiLADSL =>

  def importIndexVectorOps() {
    val MArray1D = lookupTpe("Array1D")
    val MMap = lookupTpe("ForgeMap")
  	val DenseMatrix = lookupTpe("DenseMatrix")
  	val DenseVector = lookupTpe("DenseVector")
    val IndexVector = lookupTpe("IndexVector")
    val Tuple2 = lookupTpe("Tup2") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")

    data(IndexVector, ("_data", MArray1D(MInt)), ("_start", MInt), ("_end", MInt), ("_isRow", MBoolean), ("_isRange", MBoolean), ("_isWild", MBoolean))

    // --- Compiler field accessors
    compiler (IndexVector) ("indexvector_get_start", Nil, IndexVector :: MInt) implements getter(0, "_start")
    compiler (IndexVector) ("indexvector_get_end", Nil, IndexVector :: MInt) implements getter(0, "_end")
    compiler (IndexVector) ("indexvector_raw_data", Nil, IndexVector :: MArray(MInt)) implements getter (0, "_data")
    compiler (IndexVector) ("isRange", Nil, IndexVector :: MBoolean) implements getter(0, "_isRange")
    compiler (IndexVector) ("isWild", Nil, IndexVector :: MBoolean) implements getter(0, "_isWild")

    // --- Compiler helper methods
    compiler (IndexVector) ("indexvector_allocate", Nil, (MArray1D(MInt), MInt, MInt, MBoolean, MBoolean, MBoolean) :: IndexVector) implements
      allocates(IndexVector, quotedArg(0), quotedArg(1), quotedArg(2), quotedArg(3), quotedArg(4), quotedArg(5))

    compiler (IndexVector) ("indexvector_copydata", Nil, (DenseVector(MInt)) :: MArray1D(MInt)) implements composite ${
      densevector_raw_data($0).map{x => x}
    }
    // array must be dead or immutable when this is called
    compiler (IndexVector) ("indexvector_fromarray1d", Nil, (MArray1D(MInt), MBoolean) :: IndexVector) implements composite ${
      indexvector_allocate($0, unit(0), unit(0), $1, unit(false), unit(false))
    }

    // Add various common Vector method aliases
    addVectorCommonOps(IndexVector, MInt)

    // --- Index helpers
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

    // --- Static methods
    static (IndexVector) ("apply", Nil, (DenseVector(MInt), MBoolean) :: IndexVector) implements composite ${
      indexvector_allocate(indexvector_copydata($0), unit(0), unit(0), $1, unit(false), unit(false))
    }
    static (IndexVector) ("apply", Nil, DenseVector(MInt) :: IndexVector) implements redirect ${ IndexVector($0, $0.isRow) }

    static (IndexVector) ("apply", Nil, (MInt, MInt, MBoolean) :: IndexVector) implements composite ${
      fassert($0 >= unit(0), unit("IndexVector start must be non-negative"))
      fassert($1 >= $0, unit("Cannot create an IndexVector with negative length"))
      indexvector_allocate(MArray1D[Int](unit(0)), $0, $1, $2, unit(true), unit(false))
    }
    static (IndexVector) ("apply", Nil, (MInt,MInt) :: IndexVector) implements redirect ${ IndexVector($0, $1, unit(true)) }
     
    // --- Range vector constructor (e.g. 0::10)
    noSourceContextList ::= "::" // surpress SourceContext implicit because it interferes with the 'apply' method being immediately callable
    infix (IndexVector) ("::", Nil, ((("end", MInt), ("start", MInt)) :: IndexVector)) implements composite ${ IndexVector($start, $end) }

    // --- Wildcard Constructor
    // New implementation of IndexWildcard
    // Note that Wildcard is considered a range vector - use carefully!
    direct (IndexVector) ("*", Nil, Nil :: IndexVector) implements composite ${
      indexvector_allocate(MArray1D[Int](unit(0)), unit(-1), unit(-1), unit(true), unit(true), unit(true))
    }

    val IndexVectorOps = withTpe(IndexVector)
    IndexVectorOps {
      infix ("isRow") (Nil :: MBoolean) implements getter(0, "_isRow")
      infix ("isCol") (Nil :: MBoolean) implements composite ${ !$self.isRow }

      infix ("toBoolean") (Nil :: DenseVector(MBoolean), ("conv", MInt ==> MBoolean)) implements composite ${ $self.map{$conv} }
      infix ("toDouble") (Nil :: DenseVector(MDouble), ("conv", MInt ==> MDouble)) implements composite ${ $self.map{$conv} } 
      infix ("toFloat") (Nil :: DenseVector(MFloat), ("conv", MInt ==> MFloat)) implements composite ${ $self.map{$conv} }
      infix ("toInt") (Nil :: DenseVector(MInt)) implements composite ${ $self.toDense }

      infix ("start") (Nil :: MInt) implements composite ${
        fassert(!isWild($self), "Method start is not defined for Wildcard")
        fassert(isRange($self), "Method start is not defined for non-range IndexVector")
        indexvector_get_start($self)
      }
      infix ("end") (Nil :: MInt) implements composite ${
        fassert(!isWild($self), "Method end is not defined for Wildcard")
        fassert(isRange($self), "Method end is not defined for non-range IndexVector")
        indexvector_get_end($self)
      }
      infix ("length") (Nil :: MInt) implements composite ${ 
        fassert(!isWild($self), "Method length is not defined for Wildcard")
        if (isRange($self))
          indexvector_get_end($self) - indexvector_get_start($self)
        else
          fmultia_size(indexvector_raw_data($self))
      }
      infix ("indices") (Nil :: IndexVector) implements composite ${ 
        fassert(!isWild($self), "Method indices is not defined for Wildcard")
        IndexVector(unit(0), $self.length, $self.isRow) 
      }
      
      // --- Single element accesses
      infix ("apply") (MInt :: MInt) implements composite ${ 
        fassert(!isWild($self), "Method apply is not defined for Wildcard")
        if (isRange($self))
          $self.start + $1 
        else
          indexvector_raw_data($self).apply($1)
      }

      // --- Continuous accesses
      // NOTE: Modified from the way IndexVectors previously worked where slice(x,y) gave an index vector from x to y for range indexvectors
      // Previous implementation was rather strange since v(0) was the same as v.start, but v.slice(0, 1) was out of bounds
      // if v was a range and had a start > 0
      infix ("slice") (IndexVector :: IndexVector) implements composite ${
        fassert(!isWild($self), "Method slice is not defined for Wildcard")
        fassert(isRange($1), "Method slice is not defined for non-range IndexVector arguments")
        if (isRange($1)) {
					if (isRange($self))
        		IndexVector($self.start + $1.start, $self.start + $1.end, $self.isRow)
	        else {
	          val data = indexvector_raw_data($self).slice($1.start, $1.length)
            indexvector_fromarray1d(data, $self.isRow)
	        }
	      }
      }

      // --- Discontinuous accesses
      infix ("dice") (IndexVector :: IndexVector) implements composite ${
        fassert(!isWild($self), "Method dice is not defined for Wildcard")
        if (isWild($1)) $self
        else if (isRange($1) && isRange($self)) $self.slice($1)
        else {
          val data = Array1D.fromFunction($1.length){i => $self($1(i)) }
          indexvector_fromarray1d(data, $self.isRow)
        }
      }

      // --- Reshaping
      infix ("t") (Nil :: IndexVector) implements composite ${
        indexvector_allocate(indexvector_raw_data($self), $self.start, $self.end, !self.isRow, isRange($self), isWild($self))
      }

      // --- IndexVector copiers
      infix ("Clone") (Nil :: IndexVector, aliasHint = copies(0)) implements composite ${ 
        if (isWild($self)) $self
        else if (isRange($self))
          IndexVector($self.start, $self.end, $self.isRow)
        else
          indexvector_fromarray1d(indexvector_raw_data($self).Clone, vector_dims($self))
      }
      infix ("toDense") (Nil :: DenseVector(MInt)) implements composite ${ 
        fassert(!isWild($self), "Cannot convert Wildcard to DenseVector")
        if (isRange($self))
          $self.map{e => e} 
        else
          densevector_fromarray(indexvector_raw_data($self), $self.isRow)
      }
      infix ("mutable") (Nil :: DenseVector(MInt), effect = mutable, aliasHint = copies(0)) implements composite ${ 
        cassert(!isWild($self), "Cannot convert Wildcard to DenseVector")
        $self.map{e => e}.mutable
      }

      // --- Comparisons
      direct ("__equal") (IndexVector :: MBoolean) implements composite ${ 
        if (isWild($self) && isWild($1)) { unit(true) }
        else if (isWild($self) || isWild($1)) { unit(false) }
        else if ($self.isRow != $1.isRow) { unit(false) }
        else if (isRange($self) && isRange($1))
          ($self.start == $1.start) && ($self.end == $1.end)
        else
          $self.toDense == $1
      }
      direct ("__equal") (DenseVector(MInt) :: MBoolean) implements composite ${ 
        if (isWild($self)) { unit(false) } 
        else { $1 == $self } 
      }

      // --- Ordering
      infix ("min") (Nil :: MInt) implements composite ${ 
        fassert(!isWild($self), "Method min is not defined for Wildcard")
        if (isRange($self))
          $0.first 
        else
          $self.toDense.min
      }
      infix ("max") (Nil :: MInt) implements composite ${ 
        fassert(!isWild($self), "Method max is not defined for Wildcard")
        if (isRange($self))
          $0.last
        else
          $self.toDense.max 
      }

      // TODO: these should give better errors for empty vectors
      infix ("minIndex") (Nil :: MInt) implements composite ${ 
        fassert(!isWild($self), "Method minIndex is not defined for Wildcard")
        fassert($self.length > unit(0), unit("Index out of bounds: no minIndex in empty IndexVector"))
        if (isRange($self))
          unit(0)
        else
          $self.toDense.minIndex
      } 
      infix ("maxIndex") (Nil :: MInt) implements composite ${ 
        fassert(!isWild($self), "Method maxIndex is not defined for Wildcard")
        fassert($self.length > unit(0), unit("Index out of bounds: no maxIndex in empty IndexVector"))
        if (isRange($self))
          $self.length - 1 
        else
          $self.toDense.maxIndex
      }

      // --- Math operations on IndexVector
     	infix ("+") (MInt :: DenseVector(MInt)) implements composite ${ $self.map{e => e + $1} }
      infix ("-") (MInt :: DenseVector(MInt)) implements composite ${ $self.map{e => e - $1} }
      infix ("*") (MInt :: DenseVector(MInt)) implements composite ${ $self.map{e => e * $1} }
      infix ("/") (MInt :: DenseVector(MInt)) implements composite ${ $self.map{e => e / $1} }
      infix ("+") (DenseVector(MInt) :: DenseVector(MInt)) implements composite ${ $self.zip($1){(a,b) => a + b} }
      infix ("-") (DenseVector(MInt) :: DenseVector(MInt)) implements composite ${ $self.zip($1){(a,b) => a - b} }
      infix ("*") (DenseVector(MInt) :: DenseVector(MInt)) implements composite ${ $self.zip($1){(a,b) => a * b} }
      infix ("/") (DenseVector(MInt) :: DenseVector(MInt)) implements composite ${ $self.zip($1){(a,b) => a / b} }

      // dot product
      infix ("*:*") (DenseVector(MInt) :: MInt) implements composite ${
        fassert($self.length == $1.length, "Dimension mismatch in vector dot product (" + $self.length + " != " + $1.length + ")")
        if (isRange($self)) 
        	$self.zip($1){(a,b) => a * b}.sum
       	else {
       		val a = fmultia_zip(indexvector_raw_data($self), densevector_raw_data($1), {(a: Rep[Int],b: Rep[Int]) => a * b})
       		fmultia_reduce(a, {(a: Rep[Int],b: Rep[Int]) => a + b}, unit(0))
       	}
      }
      // vector outer product
      infix ("**") (DenseVector(MInt) :: DenseMatrix(MInt)) implements composite ${
        fwarn($self.isCol && $1.isRow, "Ignoring dimension mismatch in vector outer product")
        ($self.indices, $1.indices) {(i,j) => $self(i) * $1(j) }
      }

      infix ("*") (DenseMatrix(MInt) :: DenseVector(MInt)) implements composite ${ 
        fwarn($self.isRow, "Ignoring dimension mismatch in vector-matrix product (Expected row vector, found column vector")
        fassert($self.length == $1.numRows, "Dimension mismatch in vector-matrix product")
        $1.mapColsToVector{col => $self *:* col}
      }

      infix ("sum") (Nil :: MInt) implements composite ${ 
        fassert(!isWild($self), "Method sum is not defined for Wildcard")
        if (isRange($self)) {
          if ($self.length == 0) unit(0)
          else if ($self.start == 0) { (($self.length - 1) * ($self.start + $self.end))/2 }
          else { ($self.length * ($self.start + $self.end - 1))/2 }
        }
        else
          $self.reduce{(a,b) => a + b}
      }
      infix ("prod") (Nil :: MInt) implements composite ${ 
        fassert(!isWild($self), "Method prod is not defined for Wildcard")
        $self.fold(1){(a,b) => a * b}   // FIXME: change to reduce
      }
      infix ("mean") (Nil :: MDouble) implements composite ${ 
        fassert(!isWild($self), "Method mean is not defined for Wildcard")
        $0.sum / ($0.length.toDouble) 
      }
      infix ("median") (Nil :: MDouble) implements composite ${
        fassert(!isWild($self), "Method median is not defined for Wildcard")
        if (isRange($self)) {
          if ($self.length % 2 == 0)
            0.5 + ($self.start + $self.length/2).toDouble
          else
            ($self.start + $self.length/2).toDouble
        }
        else
          $self.toDense.median
      }

      infix ("prefixSum") (Nil :: DenseVector(MInt)) implements composite ${
        fassert(!isWild($self), "Method prefixSum is not defined for Wildcard")
        if (isRange($self))
          (0::$self.length) {i => ($self.start::($self.start + i + 1)).sum }
        else
          $self.toDense.prefixSum
      }

      // --- Arith operations
      infix ("abs") (Nil :: DenseVector(MInt)) implements composite ${ $self.toDense.abs }
      infix ("square") (Nil :: DenseVector(MInt)) implements composite ${ $self.map{square(_)} }
      infix ("ceil") (Nil :: DenseVector(MInt)) implements composite ${ $self.toDense }
      infix ("floor") (Nil :: DenseVector(MInt)) implements composite ${ $self.toDense }

      // --- Parallel Ops
      infix ("contains") (MInt :: MBoolean) implements composite ${ 
        fassert(!isWild($self), "Method contains is not defined for Wildcard")
        if (isRange($self))
          $1 >= $self.start && $1 <= $self.end 
        else
          $self.toDense.contains($1)
      } 
      infix ("distinct") (Nil :: DenseVector(MInt)) implements composite ${ 
        fassert(!isWild($self), "Method distinct is not defined for Wildcard")
        if (isRange($self))
          $self.toDense 
        else  
          $self.toDense.distinct
      }
      infix ("count") ((MInt ==> MBoolean) :: MInt) implements composite ${ 
        fassert(!isWild($self), "Method count is not defined for Wildcard")
        $self.toDense.count($1) 
      }
      // Having both versions doesn't work
      /*infix ("count") (MInt :: MInt) implements composite ${ 
        fassert(!isWild($self), "Method count is not defined for Wildcard")
        if (isRange($self))
          if ($self.contains($1)) unit(1) else unit(0) 
        else
          $self.toDense.count($1)
      }*/

      infix ("filter") ((MInt ==> MBoolean) :: IndexVector) implements composite ${ 
        fassert(!isWild($self), "Method filter is not defined for Wildcard")
        IndexVector($self.toDense.filter($1), $self.isRow)
      }

      infix ("map") ((MInt ==> R) :: DenseVector(R), addTpePars = R) implements composite ${
        fassert(!isWild($self), "Method map is not defined for Wildcard") 
        if (isRange($self))
          densevector_fromfunction($self.length, {i => $1(i)})
        else
          $self.toDense.map{$1}
      } 

      // TODO: Are these necessary? Fusion should ultimately make these identical
      //compiler ("indexvector_unsafe_reduce") ((MInt, ((MInt, MInt) ==> MInt)) :: MInt) implements reduce(MInt, 0, ${$1}, ${ (a,b) => $2(a,b) })
      //compiler ("indexvector_unsafe_foreach") ((MInt ==> MUnit) :: MUnit, effect = simple) implements foreach(MInt, 0, ${ e => $1(e) })
      //compiler ("indexvector_unsafe_flatmap") ((MInt ==> DenseVector(R)) :: DenseVector(R), addTpePars = R) implements flatMap((MInt, R), 0, ${i => $1(i)})

      infix ("flatMap") ((MInt ==> DenseVector(R)) :: DenseVector(R), addTpePars = R) implements composite ${
        fassert(!isWild($self), "Method flatMap is not defined for Wildcard")
        $self.toDense.flatMap($1)
        //if (isRange($self))
        //  indexvector_unsafe_flatmap($self, {i => $1(i + $self.start)})
        //else
        //  densevector_fromarray(indexvector_raw_data($self).flatMap{i => densevector_raw_data($1(i))})
      }

      // TODO: Reduce should have no zero value (e.g. product reduce)
      infix ("reduce") (((MInt, MInt) ==> MInt) :: MInt) implements composite ${
        fassert(!isWild($self), "Method reduce is not defined for Wildcard")
        $self.toDense.reduce($1)
        //if (isRange($self))
        //  indexvector_unsafe_reduce($self, unit(0), {(a,b) => $1(a + $self.start, b + $self.start) })
        //else
        //  fmultia_fold(indexvector_raw_data($self), $1, unit(0))
      }
      infix ("fold") (CurriedMethodSignature(List(List(MInt), List((MInt, MInt) ==> MInt)), MInt)) implements composite ${
        fassert(!isWild($self), "Method reduce is not defined for Wildcard")
        $self.toDense.fold($1)($2)
        //if (isRange($self))
        //  indexvector_unsafe_reduce($self, $1, {(a,b) => $2(a + $self.start, b + $self.start) })
        //else
        //  fmultia_fold(indexvector_raw_data($self), $2, $1)
      }
      infix ("forall") ((MInt ==> MBoolean) :: MBoolean) implements composite ${ 
        $self.map($1).fold(unit(true)){_&&_}
      }

      infix ("foreach") ((MInt ==> MUnit) :: MUnit, effect = simple) implements composite ${
        fassert(!isWild($self), "Method foreach is not defined for Wildcard")
        $self.toDense.foreach($1)
        //if (isRange($self))
        //indexvector_unsafe_foreach($self, $1)
      }
      infix ("forIndices") ((MInt ==> MUnit) :: MUnit, effect = simple) implements composite ${
        $self.indices.foreach($1)
      }

      // Just in case you want to do this for some reason..
      infix ("zip") (CurriedMethodSignature(List(List(IndexVector), List((MInt,MInt) ==> R)), DenseVector(R)), addTpePars = R) implements composite ${
        fassert(!isWild($self), "Method zip is not defined for Wildcard")
        fassert(!isWild($1), "Cannot zip with Wildcard")
        fassert($self.length == $1.length, "Vector length mismatch in IndexVector zipwith")
        if (isRange($self) && isRange($1))
          densevector_fromfunction($self.length, {i => $2($self(i), $1(i))})
        else 
          $self.toDense.zip($1)($2)
      }
      infix ("zip") (CurriedMethodSignature(List(List(DenseVector(T)), List((MInt,T) ==> R)), DenseVector(R)), addTpePars = (T, R)) implements composite ${
        fassert(!isWild($self), "Method zip is not defined for Wildcard")
        fassert($self.length == $1.length, "Vector length mismatch in IndexVector zipwith")
        $self.toDense.zip($1)($2)
      } 

      infix ("groupBy") ((MInt ==> K, MInt ==> V) :: MMap(K, DenseVector(V)), addTpePars = (K,V)) implements composite ${
        fassert(!isWild($self), "Method groupBy is not defined for Wildcard")
        $self.toDense.groupBy($1,$2)
      }
      
      // --- IndexVector as a Parallel Collection
      // parallel, so the conversion can fuse with the consumer
      // is this fast and robust enough to capture parallel operators over index vectors?
      fimplicit ("indexToVector") (Nil :: DenseVector(MInt)) implements composite ${        
        fassert(!isWild($self), "Cannot cast Wildcard to DenseVector")
        fwarn("Automatic conversion from IndexVector to DenseVector - may reduce performance")
        $self.toDense
      }

      compiler ("indexvector_illegalalloc") (MInt :: MNothing) implements composite ${ fatal("IndexVectors cannot be allocated from a parallel op") }
      compiler ("indexvector_illegalupdate") ((MInt, MInt) :: MNothing) implements composite ${ fatal("IndexVectors cannot be updated") }
      // IndexVectors can't be mapped over, but they can be zipped with or reduced
      //  -- Not quite true? map just turns into a mapIndices with some offset
      parallelize as ParallelCollection(MInt, lookupOp("indexvector_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",5), lookupOp("indexvector_illegalupdate"))
    
      infix ("makeString") (Nil :: MString) implements single ${
        var s = ""
        if ($self == null) s = "null"
        else if (isWild($self)) s = "*"
        else if ($self.length == 0) s = "[ ]"
        else {
          val break = if ($self.isRow) "" else "\\n"
          for (i <- 0 until $self.length - 1) { 
            s = s + optila_padspace($self(i).makeStr) + break 
          } 
          s = s + optila_padspace($self.last.makeStr)
        }
        (s)
      }
      infix ("toString") (Nil :: MString) implements single ${
        var s = ""
        if ($self == null) s = "null"
        else if (isWild($self)) s = "*"
        else if ($self.length == 0) s = "[ ]"
        else {
          val break = if ($self.isRow) "" else "\\n"
          for (i <- 0 until $self.length - 1) { 
            s = s + optila_padspace(optila_fmt_str($self(i))) + break 
          }
          s = s + optila_padspace(optila_fmt_str($self.last))
        }
        (s)
      }

      infix ("pprint") (Nil :: MUnit, effect = simple) implements composite ${ println($self.makeStr + "\\n") }
    }

    // --- Direct math, ordering, and arith ops
	  val Arith = lookupGrp("BasicArith")
    direct (Arith) ("abs", Nil, IndexVector :: DenseVector(MInt)) implements redirect ${ $0.abs }
    direct (Arith) ("square", Nil, IndexVector :: DenseVector(MInt)) implements composite ${ $0.map{square(_)} }
    direct (Arith) ("ceil", Nil, IndexVector :: DenseVector(MInt)) implements redirect ${ $0.toDense }
    direct (Arith) ("floor", Nil, IndexVector :: DenseVector(MInt)) implements redirect ${ $0.toDense }

    val Math = lookupGrp("BasicMath")
    direct (Math) ("prod", Nil, IndexVector :: MInt) implements redirect ${ $0.prod }
    direct (Math) ("mean", Nil, IndexVector :: MDouble) implements redirect ${ $0.mean }
    direct (Math) ("sum", Nil, IndexVector :: MInt) implements redirect ${ $0.sum }
    UnaryMath.forType(MInt){(op, R) => direct (Math) (op, Nil, IndexVector :: DenseVector(R)) implements redirect { quotedArg(0) + ".map{" + op + "(_)}" } }

    val Order = lookupGrp("BasicOrder")
    direct (Order) ("min", Nil, IndexVector :: MInt) implements redirect ${ $0.min }
    direct (Order) ("max", Nil, IndexVector:: MInt) implements redirect ${ $0.max }
  }
}

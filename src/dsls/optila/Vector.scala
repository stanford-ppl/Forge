package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait VectorOps {
  this: OptiLADSL =>

  /**
   * Code generation can be an alternative to subtyping for achieving code re-use:
   *
   * This interface represents a convenient set of vector accessor functions.
   * They require v to define length, isRow, slice and apply, and v must be a ParallelCollection (which is checked at Forge stage-time)
   *
   * The trade-off here is compile-time vs. run-time: we are generating more code, but do not require an implicit conversion (which
   * previously copied into a DenseVector) to expose the common API. Note that in the Delite case the conversion should fuse anyway,
   * resulting in no run-time overhead. However, in the library-case, it will certainly hurt.
   */
  def addVectorCommonOps(v: Rep[DSLType], T: Rep[DSLType]) {
    val IndexVector = lookupTpe("IndexVector")
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val DenseMatrix = lookupTpe("DenseMatrix")
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
    if (!isTpePar(T)) compiler (v) ("zeroT", Nil, Nil :: T) implements composite ${ 0.asInstanceOf[\$TT] }

    // We need to be a little careful using empty for zero here. With nested vectors, the zero vector
    // will not be the correct dimension. This implementation relies on the fact that DeliteOps will not
    // attempt to reduce with the zero element unless the collection is empty, in which case the behavior
    // is still not correct (but should probably throw an exception instead of silently returning []).
    val AZ = if (isTpePar(T)) (List(TArith(asTpePar(T))), "implicitly[Arith[T]].empty") else (Nil, "zeroT")
    val A = AZ._1; val Z = AZ._2; // can't use capital letters with tuple return pattern matching
    val O = if (isTpePar(T)) List(TOrdering(asTpePar(T))) else Nil
    val S = if (isTpePar(T)) List(TStringable(asTpePar(T))) else Nil
    val V = if (isTpePar(T)) v(T) else v

    val VectorCommonOps = withTpe(v)
    VectorCommonOps {
      /**
       * Conversions
       */
      infix ("toBoolean") (Nil :: DenseVector(MBoolean), ("conv",T ==> MBoolean)) implements composite ${ self.map(conv) }
      infix ("toDouble") (Nil :: DenseVector(MDouble), ("conv",T ==> MDouble)) implements composite ${ self.map(conv) }
      infix ("toFloat") (Nil :: DenseVector(MFloat), ("conv",T ==> MFloat)) implements composite ${ self.map(conv) }
      infix ("toInt") (Nil :: DenseVector(MInt), ("conv",T ==> MInt)) implements composite ${ self.map(conv) }

      /**
       * Accessors
       */
      infix ("indices") (Nil :: IndexVector) implements composite ${ IndexVector(unit(0), $self.length, $self.isRow) }
      infix ("isEmpty") (Nil :: MBoolean) implements composite ${ $self.length == 0 }
      infix ("first") (Nil :: T) implements composite ${ $self(0) }
      infix ("last") (Nil :: T) implements composite ${ $self($self.length - 1) }
      infix ("drop") (MInt :: V) implements composite ${ $self.slice($1, $self.length) }
      infix ("take") (MInt :: V) implements composite ${ $self.slice(0, $1) }
      infix ("contains") (T :: MBoolean) implements composite ${
        var found = false
        var i = 0
        while (i < $self.length && !found) {
          if ($self(i) == $1) {
            found = true
          }
          i += 1
        }
        found
      }

      infix ("histogram") (Nil :: MHashMap(T,MInt)) implements composite ${
        $self.groupByReduce(e => e, v => 1, (a: Rep[Int],b: Rep[Int]) => a+b)
      }

      infix ("distinct") (Nil :: DenseVector(T)) implements composite ${
        val elements = $self.histogram
        densevector_fromarray(elements.keys(), true)
      }

      infix ("mutable") (Nil :: DenseVector(T), effect = mutable, aliasHint = copies(0)) implements composite ${
        val out = DenseVector[\$TT]($self.length, $self.isRow)
        // TODO - investigate: Using composite and 'foreach' below breaks testBulkUpdate() in DenseVectorSuite when SoA is on. Why?
        // out.indices foreach { i => out(i) = $self(i) }
        for (i <- 0 until out.length) {
          out(i) = $self(i)
        }
        out
      }

      infix ("replicate") ((MInt,MInt) :: DenseMatrix(T)) implements composite ${
        if ($self.isRow) {
          val out = DenseMatrix[\$TT]($1, $2*$self.length)
          for (col <- 0 until $2*$self.length){
            val colToJ = col % $self.length
            for (rI <- 0 until $1) {
              out(rI, col) = $self(colToJ)
            }
          }
          out.unsafeImmutable
        }
        else {
          val out = DenseMatrix[\$TT]($1*$self.length, $2)
          for (row <- 0 until $1*$self.length){
            val rowToI = row % $self.length
            for (cI <- 0 until $2) {
              out(row, cI) = $self(rowToI)
            }
          }
          out.unsafeImmutable
        }
      }

      // we need two versions so that we can override toString in the lib, and use makeStr in Delite. sad.
      infix ("makeString") (Nil :: MString, S) implements single ${
        var s = ""
        if ($self.length == 0) {
          s = "[ ]"
        }
        else if ($self.isRow) {
          for (i <- 0 until $self.length - 1) {
            s = s + optila_padspace($self(i).makeStr)
          }
          s = s + optila_padspace($self($self.length-1).makeStr)
        }
        else {
          for (i <- 0 until $self.length - 1) {
            s = s + optila_padspace($self(i).makeStr) + "\\n"
          }
          s = s + optila_padspace($self($self.length-1).makeStr)
        }
        s
      }

      infix ("toString") (Nil :: MString) implements single ${
        var s = ""
        if ($self.length == 0) {
          s = "[ ]"
        }
        else if ($self.isRow) {
          for (i <- 0 until $self.length - 1) {
            s = s + optila_padspace(optila_fmt_str($self(i)))
          }
          s = s + optila_padspace(optila_fmt_str($self($self.length-1)))
        }
        else {
          for (i <- 0 until $self.length - 1) {
            s = s + optila_padspace(optila_fmt_str($self(i))) + "\\n"
          }
          s = s + optila_padspace(optila_fmt_str($self($self.length-1)))
        }
        s
      }

      infix ("pprint") (Nil :: MUnit, S, effect = simple) implements composite ${ println($self.makeStr + "\\n") } // $self.toString doesn't work in Delite

      infix ("makeStrWithDelim") (("delim",MString) :: MString, S) implements composite ${
        array_mkstring($self.toArray, delim)
      }

      /**
       * Math
       */

      // allow arbitrary rhs arguments as well.. are we getting carried away?
      for (rhs <- List(DenseVector(T),DenseVectorView(T))) {
        infix ("+") (rhs :: DenseVector(T), A) implements composite ${ $self.zip($1) { (a,b) => a+b } }
        infix ("-") (rhs :: DenseVector(T), A) implements composite ${ $self.zip($1) { (a,b) => a-b } }
        infix ("*") (rhs :: DenseVector(T), A) implements composite ${ $self.zip($1) { (a,b) => a*b } }
        infix ("*:*") (rhs :: T, A) implements composite ${
          fassert($self.length == $1.length, "dimension mismatch: vector dot product")
          sum($self*$1)
        }
        infix ("**") (rhs :: DenseMatrix(T), A) implements composite ${
          fassert(!$self.isRow && $1.isRow, "dimension mismatch: vector outer product")
          val out = DenseMatrix[\$TT]($self.length, $1.length)
          for (i <- 0 until $self.length ){
            for (j <- 0 until $1.length ){
              out(i,j) = $self(i)*$1(j)
            }
          }
          out.unsafeImmutable
        }
        infix ("/") (rhs :: DenseVector(T), A) implements composite ${ $self.zip($1) { (a,b) => a/b } }
      }

      for (rhs <- List(SparseVector(T),SparseVectorView(T))) {
        infix ("+") (rhs :: DenseVector(T), A) implements composite ${ $self + $1.toDense }
        infix ("-") (rhs :: DenseVector(T), A) implements composite ${ $self - $1.toDense }
        infix ("*") (rhs :: DenseVector(T), A) implements composite ${ $self * $1.toDense }
        infix ("*:*") (rhs :: T, A) implements composite ${ $self *:* $1.toDense }
        infix ("**") (rhs :: DenseMatrix(T), A) implements composite ${ $self ** $1.toDense }
        infix ("/") (rhs :: DenseVector(T), A) implements composite ${ $self / $1.toDense }
      }

      for (rhs <- List(DenseVector(B),DenseVectorView(B))) {
        // infix ("+") (rhs :: DenseVector(T), (A, B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a+b })
        // infix ("-") (rhs :: DenseVector(T), (A, B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a-b })
        // infix ("*") (rhs :: DenseVector(T), (A, B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a*b })
        // infix ("/") (rhs :: DenseVector(T), (A, B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a/b })
        infix ("zip") (CurriedMethodSignature(List(List(rhs), List((T,B) ==> R)), DenseVector(R)), addTpePars = (B,R)) implements zip((T,B,R), (0,1), ${ (a,b) => $2(a,b) })
      }

      infix ("+") (T :: DenseVector(T), A) implements composite ${ self.map(e => e+$1) }
      infix ("-") (T :: DenseVector(T), A) implements composite ${ self.map(e => e-$1) }
      infix ("*") (T :: DenseVector(T), A) implements composite ${ self.map(e => e*$1) }
      infix ("*") (DenseMatrix(T) :: DenseVector(T), A) implements composite ${
        fassert($self.isRow, "dimension mismatch: vector * matrix")
        $1.mapColsToVector { col => $self *:* col }
      }
      infix ("/") (T :: DenseVector(T), A) implements composite ${ self.map(e => e/$1) }

      infix ("abs") (Nil :: DenseVector(T), A) implements composite ${ self.map(e => e.abs) }
      infix ("exp") (Nil :: DenseVector(T), A) implements composite ${ self.map(e => e.exp) }
      infix ("log") (Nil :: DenseVector(T), A) implements composite ${ self.map(e => e.log) }
      infix ("sum") (Nil :: T, A) implements composite ${ self.reduce((a,b) => a+b ) }
      infix ("prod") (Nil :: T, A) implements reduce(T, 0, ${unit(1.asInstanceOf[\$TT])}, ${ (a,b) => a*b })
      infix ("mean") (Nil :: MDouble, ("conv",T ==> MDouble)) implements composite ${ $self.map(conv).sum / $self.length }


      /**
       * Ordering
       */
      infix ("min") (Nil :: T, O ::: A) implements composite ${
        $self.reduce { (a,b) => if (a < b) a else b }
        //reduce(T, 0, ${$self(0)}, ${ (a,b) => if (a < b) a else b })
      }
      infix ("max") (Nil :: T, O ::: A) implements composite ${
        $self.reduce { (a,b) => if (a > b) a else b }
        //reduce(T, 0, ${$self(0)}, ${ (a,b) => if (a > b) a else b })
      }

      infix ("minIndex") (Nil :: MInt, O ::: A) implements composite ${
        $self.indices.reduce { (a,b) => if ($self(a) < $self(b)) a else b }
        // ($self.zip($self.indices) { (a,b) => pack((a,b)) } reduce { (t1,t2) => if (t1._1 < t2._1) t1 else t2 })._2
      }
      infix ("maxIndex") (Nil :: MInt, O ::: A) implements composite ${
        $self.indices.reduce { (a,b) => if ($self(a) > $self(b)) a else b }
        // ($self.zip($self.indices) { (a,b) => pack((a,b)) } reduce { (t1,t2) => if (t1._1 > t2._1) t1 else t2 })._2
      }


      /**
       * Bulk
       */
      infix ("map") ((T ==> R) :: DenseVector(R), addTpePars = R) implements map((T,R), 0, ${ e => $1(e) })
      infix ("reduce") (((T,T) ==> T) :: T, A) implements reduce(T, 0, Z, ${ (a,b) => $1(a,b) })
      infix ("foreach") ((T ==> MUnit) :: MUnit) implements foreach(T, 0, ${ e => $1(e) })
      infix ("forall") ((T ==> MBoolean) :: MBoolean) implements composite ${ reduce_and($self.map($1)) }
      infix ("find") ((T ==> MBoolean) :: IndexVector) implements composite ${ $self.indices.filter(i => $1($self(i))) }

      val filterMap = v.name.toLowerCase + "_densevector_filter_map"
      compiler (filterMap) (((T ==> MBoolean), (T ==> R)) :: DenseVector(R), addTpePars = R) implements filter((T,R), 0, ${ e => $1(e) }, ${ e => $2(e) })
      infix ("count") ((T ==> MBoolean) :: MInt) implements composite ${
        val x = \$filterMap($self, $1, (e: Rep[\$TT]) => 1)
        if (x.length > 0) sum(x)
        else 0
      }

      val partitionReturn = if (v.name == "IndexVector") IndexVector else DenseVector(T)
      infix ("partition") (("pred",(T ==> MBoolean)) :: Tuple2(partitionReturn,partitionReturn)) implements composite ${
        val partT = $self.filter(pred)
        val partF = $self.filter(e => !pred(e))
        pack((partT, partF))
      }

      infix ("flatMap") ((T ==> DenseVector(R)) :: DenseVector(R), addTpePars = R) implements flatMap((T,R), 0, ${ e => $1(e) })

      // TODO: need to implement with a DeliteOp
      infix ("scan") (CurriedMethodSignature(List(List(("zero", R)), List((R,T) ==> R)), DenseVector(R)), addTpePars = R) implements composite ${
        val out = DenseVector[R]($self.length, $self.isRow)
        out(0) = $2($zero,$self(0))
        var i = 1
        while (i < $self.length) {
          out(i) = $2(out(i-1), $self(i))
          i += 1
        }
        out.unsafeImmutable
      }

      infix ("prefixSum") (Nil :: DenseVector(T), A) implements composite ${ $self.scan(\$Z)((a,b) => a+b) }

      // TODO
      // infix ("groupBy") (((T ==> R)) :: DenseVector(DenseVector(T)))


      /**
       * Data exchange
       */

      infix ("toArray") (Nil :: MArray(T)) implements composite ${
        array_fromfunction($self.length, i => $self(i))
      }
    }
  }
}

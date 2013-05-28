package ppl.dsl.forge
package examples
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait DenseVectorOps {
  this: OptiLADSL => 
 
  /**
   * Code generation can be an alternative to subtyping for achieving code re-use:
   * 
   * This interface represents a convenient set of vector accessor functions. 
   * They require v to define length, isRow, and apply. (should we programatically encode this somehow?)
   */
  def addVectorCommonOps(v: Rep[DSLType], T: Rep[DSLType]) {
    val DenseVector = lookupTpe("DenseVector")
    
    // have to be careful about the type argument name we use in single and composite since T is being passed in
    // We splice this name into blocks using the escaped \$ to inform the preprocessor that the value already exists.
    val TT = T.name
    
    val VectorCommonOps = withTpe(v)
    VectorCommonOps {
      infix ("isEmpty") (Nil :: MBoolean) implements single ${ $self.length == 0 }
      infix ("first") (Nil :: T) implements single ${ $self(0) }
      infix ("last") (Nil :: T) implements single ${ $self($self.length - 1) }
      infix ("slice") ((("start",MInt),("end",MInt)) :: DenseVector(T)) implements single ${
        val out = DenseVector[\$TT]($end - $start, $self.isRow)
        for (i <- $start until $end) {
          out(i-$start) = $self(i)
        }
        out.unsafeImmutable
      }                                   
      infix ("drop") (MInt :: DenseVector(T)) implements composite ${ $self.slice($1, $self.length) }
      infix ("take") (MInt :: DenseVector(T)) implements composite ${ $self.slice(unit(0), $1) }
      infix ("contains") (T :: MBoolean) implements single ${
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
      infix ("distinct") (Nil :: DenseVector(T)) implements single ${
        val out = DenseVector[\$TT](0, $self.isRow)
        for (i <- 0 until $self.length) {
          // slow -- should use a hashmap when it's available as a primitive
          if (!out.contains($self(i))) out <<= $self(i)
        }
        out.unsafeImmutable
      }
    
      infix ("toString") (Nil :: MString) implements single ${    
        var s = ""
        if ($self.isRow) { 
          s = s + "["
          for (i <- 0 until $self.length - 1) {
            s = s + $self(i) + " "
          }
          s = s + $self($self.length-1)
          s = s + "]"
        }
        else {
          for (i <- 0 until $self.length) {
            s = s + "[" + $self(i) + "]\\n"
          }
        }
        s
      }
      
      infix ("pprint") (Nil :: MUnit, effect = simple) implements composite ${ println($self.toString) }      
      
      // we can also perform reductions generically, since they don't require allocation (this is important for mapRowsToVector with VectorView)
      // Arith is only required if T is actually a tpePar here      
      if (isTpePar(T)) {
        val A = TArith(asTpePar(T))
        direct ("sum") (Nil :: T, A) implements reduce(T, 0, lookupOp("Arith","zero"), ${ (a,b) => a+b })      
        infix ("reduce") (((T,T) ==> T) :: T, A) implements reduce(T, 0, lookupOp("Arith","zero"), ${ (a,b) => $1(a,b) })            
      }
      else {
        compiler ("zeroT") (Nil :: T) implements composite ${ 0.asInstanceOf[\$TT] }
        direct ("sum") (Nil :: T) implements reduce(T, 0, lookupOp("zeroT"), ${ (a,b) => a+b })      
        infix ("reduce") (((T,T) ==> T) :: T) implements reduce(T, 0, lookupOp("zeroT"), ${ (a,b) => $1(a,b) })                    
      }
    }
  }
  
  /**
   * The main DenseVector specification.
   */
  def importDenseVectorOps() {
    val T = tpePar("T") 
    val R = tpePar("R")
    val B = tpePar("B")
    
    val DenseVector = lookupTpe("DenseVector") // tpe("DenseVector", T) 
    val IndexVector = lookupTpe("IndexVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
  
    // data fields     
    data(DenseVector, ("_length", MInt), ("_isRow", MBoolean), ("_data", MArray(T)))      
  
    // static methods
    static (DenseVector) ("apply", T, (MInt, MBoolean) :: DenseVector(T), effect = mutable) implements allocates(DenseVector, ${$0}, ${$1}, ${array_empty[T]($0)})
    static (DenseVector) ("apply", T, varArgs(T) :: DenseVector(T)) implements allocates(DenseVector, ${unit($0.length)}, ${unit(true)}, ${array_fromseq($0)})
    
    // helper
    compiler (DenseVector) ("densevector_fromfunc", T, (MInt, MInt ==> T) :: DenseVector(T)) implements single ${
      val out = DenseVector[T]($0, true)
      for (i <- 0 until out.length) {
        out(i) = $1(i)
      }      
      out.unsafeImmutable
    }    
    static (DenseVector) ("zeros", Nil, MInt :: DenseVector(MDouble)) implements single ${ densevector_fromfunc($0, i => 0.0 )}
    static (DenseVector) ("zerosf", Nil, MInt :: DenseVector(MFloat)) implements single ${ densevector_fromfunc($0, i => 0f )}
    static (DenseVector) ("ones", Nil, MInt :: DenseVector(MDouble)) implements single ${ densevector_fromfunc($0, i => 1.0) }
    static (DenseVector) ("onesf", Nil, MInt :: DenseVector(MFloat)) implements single ${ densevector_fromfunc($0, i => 1f) }
    static (DenseVector) ("rand", Nil, MInt :: DenseVector(MDouble)) implements single ${ densevector_fromfunc($0, i => random[Double]) }
    static (DenseVector) ("randf", Nil, MInt :: DenseVector(MFloat)) implements single ${ densevector_fromfunc($0, i => random[Float]) }
    static (DenseVector) ("uniform", Nil, MethodSignature(List(("start", MInt), ("step_size", MDouble), ("end", MInt), ("isRow", MBoolean, "true")), DenseVector(MDouble))) implements single ${ 
      val length = ceil(($end-$start)/$step_size)
      densevector_fromfunc(length, i => $step_size*i + $start)
    }
    
    static (DenseVector) ("flatten", T, ("pieces",DenseVector(DenseVector(T))) :: DenseVector(T)) implements single ${
      if ($pieces.length == 0){
        DenseVector[T](0, $pieces.isRow).unsafeImmutable
      }
      else {
        val sizes = $pieces map { e => e.length }
        val (total,begins) = t2(densevector_precumulate[Int](sizes, 0, (_: Rep[Int]) + (_: Rep[Int])))
        val result = DenseVector[T](total, $pieces.isRow)
        for (i <- 0 until $pieces.length) {
          result.copyFrom(begins(i), $pieces(i))
        }
        result.unsafeImmutable
      }
    }              
    
    // only composite ops can return non-lifted tuples (or anything else). using CTuple2 should work, but there is a problem with iFThenElse that I don't fully understand yet.
    // val CTuple2 = lookupTpe("Tuple2",now)
    val Tuple2 = lookupTpe("Tup2")
    compiler (DenseVector) ("densevector_precumulate", T, ((("v",DenseVector(T)), ("identity",T), ("func",(T,T) ==> T)) :: Tuple2(T,DenseVector(T)))) implements composite ${
      if ($v.length == 0) {
        (($identity,DenseVector[T](0,$v.isRow).unsafeImmutable))
      } 
      else {
        val result = DenseVector[T](0, $v.isRow)
        var accum = $identity
        for (i <- 0 until $v.length) {
          result <<= accum
          accum = $func(accum, $v(i))
        }
        (accum,result.unsafeImmutable)      
      }
    } 
          
    val DenseVectorOps = withTpe (DenseVector)          
    DenseVectorOps {                                        
      /**
       * Conversions
       */       
      infix ("toBoolean") (Nil :: DenseVector(MBoolean), ("conv",T ==> MBoolean)) implements map((T,MBoolean), 0, ${$conv})
      infix ("toDouble") (Nil :: DenseVector(MDouble), ("conv",T ==> MDouble)) implements map((T,MDouble), 0, ${$conv})
      infix ("toFloat") (Nil :: DenseVector(MFloat), ("conv",T ==> MFloat)) implements map((T,MFloat), 0, ${$conv})
      infix ("toInt") (Nil :: DenseVector(MInt), ("conv",T ==> MInt)) implements map((T,MInt), 0, ${$conv})
       
            
      /**
       * Accessors
       */
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("isRow") (Nil :: MBoolean) implements getter(0, "_isRow")
      infix ("apply") (MInt :: T) implements composite ${ array_apply(densevector_raw_data($self), $1) }                        
      infix ("indices") (Nil :: IndexVector) implements composite ${ IndexVector(unit(0), $self.length) } 
      
      
      /**
       * Miscellaneous
       */
      infix ("t") (Nil :: DenseVector(T)) implements allocates(DenseVector, ${densevector_length($0)}, ${!(densevector_isrow($0))}, ${array_clone(densevector_raw_data($0))})
      infix ("mt") (Nil :: MUnit, effect = write(0)) implements composite ${
        densevector_set_isrow($0, !$0.isRow)
      }
      infix ("Clone") (Nil :: DenseVector(T)) implements map((T,T), 0, "e => e")
      infix ("mutable") (Nil :: DenseVector(T), effect = mutable) implements single ${
        val out = DenseVector[T]($self.length, $self.isRow)        
        for (i <- 0 until out.length) {
          out(i) = $self(i)
        }
        out
      }   
      infix ("replicate") ((MInt,MInt) :: DenseMatrix(T)) implements single ${
        if ($self.isRow) {
          val out = DenseMatrix[T]($1, $2*$self.length)
          for (col <- 0 until $2*$self.length){
            val colToJ = col % $self.length
            for (rI <- 0 until $1) {
              out(rI, col) = $self(colToJ)
            }
          }
          out.unsafeImmutable
        }
        else {
          val out = DenseMatrix[T]($1*$self.length, $2)
          for (row <- 0 until $1*$self.length){
            val rowToI = row % $self.length
            for (cI <- 0 until $2) {
              out(row, cI) = $self(rowToI)
            }
          }
          out.unsafeImmutable
        }
      }
        
      
      /**
       * Data operations
       */
      compiler ("densevector_raw_data") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("densevector_set_raw_data") (MArray(T) :: MUnit, effect = write(0)) implements setter(0, "_data", ${$1})
      compiler ("densevector_set_length") (MInt :: MUnit, effect = write(0)) implements setter(0, "_length", ${$1})                  
      compiler ("densevector_set_isrow") (MBoolean :: MUnit, effect = write(0)) implements setter(0, "_isRow", ${$1})
      
      infix ("update") ((("i",MInt),("e",T)) :: MUnit, effect = write(0)) implements composite ${
        array_update(densevector_raw_data($self), $i, $e)
      }      
      infix ("<<") (T :: DenseVector(T)) implements composite ${
        val out = $self.mutable()
        out <<= $1
        out.unsafeImmutable
      }      
      infix("<<") (DenseVector(T) :: DenseVector(T)) implements single ${
        val out = DenseVector[T]($self.length+$1.length, $self.isRow)
        for (i <- 0 until $self.length){
          out(i) = $self(i)
        }
        for (i <- 0 until $1.length){
          out(i+$self.length) = $1(i)
        }
        out.unsafeImmutable        
      }
      infix ("<<=") (T :: MUnit, effect = write(0)) implements composite ${ $self.insert($self.length,$1) }          
      infix ("<<=") (DenseVector(T) :: MUnit, effect = write(0)) implements composite ${ $self.insertAll($self.length,$1) }
            
      infix ("insert") ((MInt,T) :: MUnit, effect = write(0)) implements single ${
        densevector_insertspace($self,$1,1)
        $self($1) = $2
      }      
      infix ("insertAll") ((MInt,DenseVector(T)) :: MUnit, effect = write(0)) implements single ${
        densevector_insertspace($self, $1, $2.length)
        $self.copyFrom($1, $2)     
      }
      infix ("remove") (MInt :: MUnit, effect = write(0)) implements composite ${ $self.removeAll($1, unit(1)) }
      infix ("removeAll") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements single ${
        val data = densevector_raw_data($self)
        array_copy(data, $pos + $len, data, $pos, $self.length - ($pos + $len))
        densevector_set_length($self, $self.length - $len)        
      }
            
      infix ("copyFrom") ((MInt,DenseVector(T)) :: MUnit, effect = write(0)) implements single ${        
        val d = densevector_raw_data($self)
        for (i <- 0 until $2.length) {
          array_update(d,$1+i,$2(i))
        }        
      }
      infix ("trim") (Nil :: MUnit, effect = write(0)) implements single ${
        val data = densevector_raw_data($self)
        if ($self.length < array_length(data)) {
          val d = array_empty[T]($self.length)
          array_copy(data, 0, d, 0, $self.length)
          densevector_set_raw_data($self, d.unsafeImmutable)
        }        
      }
      infix ("clear") (Nil :: MUnit, effect = write(0)) implements single ${
        densevector_set_length($self, 0)
        densevector_set_raw_data($self, (array_empty[T](0)).unsafeImmutable)        
      }      
      
      compiler ("densevector_insertspace") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements single ${
        densevector_ensureextra($self,$len)
        val data = densevector_raw_data($self)
        array_copy(data,$pos,data,$pos+$len,$self.length-$pos)
        densevector_set_length($self,$self.length+$len)
      }      
      compiler ("densevector_ensureextra") (("extra",MInt) :: MUnit, effect = write(0)) implements single ${
        val data = densevector_raw_data($self)
        if (array_length(data) - $self.length < $extra) {
          densevector_realloc($self, $self.length+$extra)
        }
      }      
      compiler ("densevector_realloc") (("minLen",MInt) :: MUnit, effect = write(0)) implements single ${
        val data = densevector_raw_data($self)
        var n = Math.max(4, array_length(data)*2)
        while (n < $minLen) n = n*2
        val d = array_empty[T](n)
        array_copy(data, 0, d, 0, $self.length)
        densevector_set_raw_data($self, d.unsafeImmutable)  
      }        
      
      
      /**
       * Math
       */                    
      infix ("+") (DenseVector(T) :: DenseVector(T), TArith(T)) implements zip((T,T,T), (0,1), ${ (a,b) => a+b })
      // infix ("+") (DenseVector(B) :: DenseVector(T), (TArith(T), B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a+b })
      infix ("+") (T :: DenseVector(T), TArith(T)) implements map((T,T), 0, ${ e => e+$1 })      
      infix ("+=") (DenseVector(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) + $1(i) }
      }
      infix ("+=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) + $1 }
      }
            
      infix ("*") (DenseVector(T) :: DenseVector(T), TArith(T)) implements zip((T,T,T), (0,1), ${ (a,b) => a*b })
      // infix ("*") (DenseVector(B) :: DenseVector(T), (TArith(T), B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a*b })
      infix ("*") (T :: DenseVector(T), TArith(T)) implements map((T,T), 0, ${ e => e*$1 })
      infix ("*=") (DenseVector(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) * $1(i) }
      }
      infix ("*=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) * $1 }
      }      
      infix ("*:*") (DenseVector(T) :: T, TArith(T)) implements composite ${ sum($self*$1) }      
      // TODO: DenseMatrix
      // infix ("*") (DenseMatrix(T) :: DenseVector(T), TArith(T))
      // infix ("**") (DenseVector(T) :: DenseMatrix(T), TArith(T))
      
      infix ("-") (DenseVector(T) :: DenseVector(T), TArith(T)) implements zip((T,T,T), (0,1), ${ (a,b) => a-b })
      // infix ("-") (DenseVector(B) :: DenseVector(T), (TArith(T), B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a-b })
      infix ("-") (T :: DenseVector(T), TArith(T)) implements map((T,T), 0, ${ e => e-$1 })
      infix ("-=") (DenseVector(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) - $1(i) }
      }
      infix ("-=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) - $1 }
      }      
      
      infix ("/") (DenseVector(T) :: DenseVector(T), TArith(T)) implements zip((T,T,T), (0,1), ${ (a,b) => a/b })
      // infix ("/") (DenseVector(B) :: DenseVector(T), (TArith(T), B ==> T), addTpePars = B) implements zip((T,B,T), (0,1), ${ (a,b) => a/b })
      infix ("/") (T :: DenseVector(T), TArith(T)) implements map((T,T), 0, ${ e => e/$1 })
      infix ("/=") (DenseVector(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) / $1(i) }
      }
      infix ("/=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${
        $self.indices.foreach { i => $self(i) = $self(i) / $1 }
      }      
      
      direct ("abs") (Nil :: DenseVector(T), TArith(T)) implements map((T,T), 0, ${ e => e.abs })
      direct ("exp") (Nil :: DenseVector(T), TArith(T)) implements map((T,T), 0, ${ e => e.exp })
      
      
      /**
       * Ordering
       */      
      infix ("sort") (Nil :: DenseVector(T), TOrdering(T)) implements allocates(DenseVector, ${densevector_length($0)}, ${!(densevector_isrow($0))}, ${array_sort(densevector_raw_data($0))})

      // TODO: HasMinMax, TupleReduce?
      // infix ("min" (Nil :: T, TOrdering(T))) implements reduce(T, 0, lookupOp("HasMinMax","min"), ${ if (a < b) a else b }) 
      // infix ("minIndex" (Nil :: T, TOrdering(T))) implements reduce(T, 0, lookupOp("HasMinMax","min"), ${ }) 
      // infix ("max" (Nil :: T, TOrdering(T))) implements reduce(T, 0, lookupOp("HasMinMax","max"), ${ if (a > b) a else b }) 
      // infix ("maxIndex" (Nil :: T, TOrdering(T))) implements reduce(T, 0, lookupOp("HasMinMax","max"), ${ }) 
      
      infix ("median") (Nil :: T, (TNumeric(T),TOrdering(T))) implements single ${ 
        val x = $self.sort
        val mid = x.length / 2
        if (x.length % 2 == 0) {
          ((x(mid).AsInstanceOf[Double] + x(mid-1).AsInstanceOf[Double]) / 2).AsInstanceOf[T]
        }
        else x(mid)
      }
      infix (":>") (DenseVector(T) :: DenseVector(MBoolean), TOrdering(T)) implements zip((T,T,MBoolean), (0,1), ${ (a,b) => a > b })
      infix (":<") (DenseVector(T) :: DenseVector(MBoolean), TOrdering(T)) implements zip((T,T,MBoolean), (0,1), ${ (a,b) => a < b })
      
             
      /**
       * Bulk (except for reductions, which are in VectorCommonOps)
       */
      infix ("map") ((T ==> R) :: DenseVector(R), addTpePars = R) implements map((T,R), 0, ${ e => $1(e) })      
      infix ("zip") (((DenseVector(B), (T,B) ==> R)) :: DenseVector(R), addTpePars = (B,R)) implements zip((T,B,R), (0,1), ${ (a,b) => $2(a,b) })
      infix ("filter") ((T ==> MBoolean) :: DenseVector(T)) implements filter((T,T), 0, ${e => $1(e)}, ${e => e})
      infix ("foreach") ((T ==> MUnit) :: MUnit, effect = simple) implements foreach(T, 0, ${ e => $1(e) })
      infix ("find") ((T ==> MBoolean) :: DenseVector(MInt)) implements composite ${ $self.indices.filter(i => $1($self(i))) }
      infix ("count") ((T ==> MBoolean) :: MInt) implements composite ${
        sum(densevector_filter_map[T,Int]($self, $1, e => unit(1)))
      }
      compiler ("densevector_filter_map") (((T ==> MBoolean), (T ==> R)) :: DenseVector(R), addTpePars = R) implements filter((T,R), 0, ${ e => $1(e) }, ${ e => $2(e) })
      
      infix ("partition") (("pred",(T ==> MBoolean)) :: Tuple2(DenseVector(T),DenseVector(T))) implements composite ${
        val outT = DenseVector[T](0, $self.isRow)
        val outF = DenseVector[T](0, $self.isRow)
        for (i <- 0 until $self.length) {
          val x = $self(i)
          if (pred(x)) outT <<= x
          else outF <<= x
        }
        (outT.unsafeImmutable, outF.unsafeImmutable)        
      }
      infix ("flatMap") ((T ==> DenseVector(R)) :: DenseVector(R), addTpePars = R) implements composite ${
        DenseVector.flatten($self.map($1))
      }       
      // TODO
      // infix ("groupBy") (((T ==> R)) :: DenseVector(DenseVector(T))) 
            
                        
      /**
       * Required for parallel collection       
       */
      compiler ("densevector_raw_alloc") (MInt :: DenseVector(R), addTpePars = R) implements single ${
        DenseVector[R]($1, $self.isRow)
      }
      compiler ("densevector_appendable") ((MInt,T) :: MBoolean) implements single("true")
      compiler ("densevector_append") ((MInt,T) :: MUnit, effect = write(0)) implements single ${
        $self.insert($self.length, $2)
      }                                  
      compiler ("densevector_copy") ((MInt,DenseVector(T),MInt,MInt) :: MUnit, effect = write(2)) implements single ${
        val src = densevector_raw_data($self)
        val dest = densevector_raw_data($2)
        array_copy(src, $1, dest, $3, $4)
      }

      parallelize as ParallelCollectionBuffer(T, lookupOp("densevector_raw_alloc"), lookupOp("length"), lookupOverloaded("apply",2), lookupOp("update"), lookupOp("densevector_set_length"), lookupOp("densevector_appendable"), lookupOp("densevector_append"), lookupOp("densevector_copy"))            
    } 
    
    
    // Add DenseVector to Arith
    val Arith = lookupGrp("Arith").asInstanceOf[Rep[DSLTypeClass]]
    val DenseVectorArith = tpeClassInst("ArithDenseVector", T withBound TArith, Arith(DenseVector(T)))
    infix (DenseVectorArith) ("zero", T withBound TArith, Nil :: DenseVector(T)) implements composite ${ DenseVector[T](unit(0),unit(true)) } // TODO: not right -- zero needs to take an argument to implement the same semantics
    infix (DenseVectorArith) ("+", T withBound TArith, (DenseVector(T),DenseVector(T)) :: DenseVector(T)) implements composite ${ densevector_pl($0,$1) }
    infix (DenseVectorArith) ("-", T withBound TArith, (DenseVector(T),DenseVector(T)) :: DenseVector(T)) implements composite ${ densevector_sub($0,$1) }
    infix (DenseVectorArith) ("*", T withBound TArith, (DenseVector(T),DenseVector(T)) :: DenseVector(T)) implements composite ${ densevector_mul($0,$1) } 
    infix (DenseVectorArith) ("/", T withBound TArith, (DenseVector(T),DenseVector(T)) :: DenseVector(T)) implements composite ${ densevector_div($0,$1) }          
    infix (DenseVectorArith) ("abs", T withBound TArith, DenseVector(T) :: DenseVector(T)) implements composite ${ densevector_abs($0) }
    infix (DenseVectorArith) ("exp", T withBound TArith, DenseVector(T) :: DenseVector(T)) implements composite ${ densevector_exp($0) }         
        
    importDenseVectorPrimitiveOps()    
    addVectorCommonOps(DenseVector,T)
  }  
  
  
  
  /**
   * Special cases for DenseVector primitive arithmetic. This is annoying, so let's hide it at the bottom.
   */
  def importDenseVectorPrimitiveOps() {
    val DenseVector = lookupTpe("DenseVector")
    
    // the conversions here will be costly unless things fuse. alternatively, we could convert element by element.

    infix (DenseVector) ("+", Nil, (MInt,DenseVector(MInt)) :: DenseVector(MInt)) implements composite ${ densevector_pl[Int]($1,$0) }
    infix (DenseVector) ("+", Nil, (MInt,DenseVector(MFloat)) :: DenseVector(MFloat)) implements composite ${ densevector_pl[Float]($1,$0.toFloat) }
    infix (DenseVector) ("+", Nil, (MInt,DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_pl[Double]($1,$0.toDouble) }
    infix (DenseVector) ("+", Nil, (MFloat,DenseVector(MInt)) :: DenseVector(MFloat)) implements composite ${ densevector_pl[Float]($1.toFloat,$0) }
    infix (DenseVector) ("+", Nil, (MFloat,DenseVector(MFloat)) :: DenseVector(MFloat)) implements composite ${ densevector_pl[Float]($1,$0) }
    infix (DenseVector) ("+", Nil, (MFloat,DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_pl[Double]($1,$0.toDouble) }
    infix (DenseVector) ("+", Nil, (MDouble,DenseVector(MInt)) :: DenseVector(MDouble)) implements composite ${ densevector_pl[Double]($1.toDouble,$0) }
    infix (DenseVector) ("+", Nil, (MDouble,DenseVector(MFloat)) :: DenseVector(MDouble)) implements composite ${ densevector_pl[Double]($1.toDouble,$0) }
    infix (DenseVector) ("+", Nil, (MDouble,DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_pl[Double]($1,$0) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),MInt) :: DenseVector(MInt)) implements composite ${ densevector_pl[Int]($0,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),MFloat) :: DenseVector(MFloat)) implements composite ${ densevector_pl[Float]($0.toFloat,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),MDouble) :: DenseVector(MDouble)) implements composite ${ densevector_pl[Double]($0.toDouble,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),MInt) :: DenseVector(MFloat)) implements composite ${ densevector_pl[Float]($0,$1.toFloat) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),MFloat) :: DenseVector(MFloat)) implements composite ${ densevector_pl[Float]($0,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),MDouble) :: DenseVector(MDouble)) implements composite ${ densevector_pl[Double]($0.toDouble,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),MInt) :: DenseVector(MDouble)) implements composite ${ densevector_pl[Double]($0,$1.toDouble) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),MFloat) :: DenseVector(MDouble)) implements composite ${ densevector_pl[Double]($0.toDouble,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),MDouble) :: DenseVector(MDouble)) implements composite ${ densevector_pl[Double]($0,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),DenseVector(MInt)) :: DenseVector(MInt)) implements composite ${ densevector_pl[Int]($0,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),DenseVector(MFloat)) :: DenseVector(MFloat)) implements composite ${ densevector_pl[Float]($0.toFloat,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MInt),DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_pl[Double]($0.toDouble,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),DenseVector(MInt)) :: DenseVector(MFloat)) implements composite ${ densevector_pl[Float]($0,$1.toFloat) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),DenseVector(MFloat)) :: DenseVector(MFloat)) implements composite ${ densevector_pl[Float]($0,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MFloat),DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_pl[Double]($0.toDouble,$1) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),DenseVector(MInt)) :: DenseVector(MDouble)) implements composite ${ densevector_pl[Double]($0,$1.toDouble) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),DenseVector(MFloat)) :: DenseVector(MDouble)) implements composite ${ densevector_pl[Double]($0,$1.toDouble) }
    infix (DenseVector) ("+", Nil, (DenseVector(MDouble),DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_pl[Double]($0,$1) }    
    
    infix (DenseVector) ("-", Nil, (DenseVector(MInt),MInt) :: DenseVector(MInt)) implements composite ${ densevector_sub[Int]($0,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MInt),MFloat) :: DenseVector(MFloat)) implements composite ${ densevector_sub[Float]($0.toFloat,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MInt),MDouble) :: DenseVector(MDouble)) implements composite ${ densevector_sub[Double]($0.toDouble,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),MInt) :: DenseVector(MFloat)) implements composite ${ densevector_sub[Float]($0,$1.toFloat) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),MFloat) :: DenseVector(MFloat)) implements composite ${ densevector_sub[Float]($0,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),MDouble) :: DenseVector(MDouble)) implements composite ${ densevector_sub[Double]($0.toDouble,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),MInt) :: DenseVector(MDouble)) implements composite ${ densevector_sub[Double]($0,$1.toDouble) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),MFloat) :: DenseVector(MDouble)) implements composite ${ densevector_sub[Double]($0.toDouble,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),MDouble) :: DenseVector(MDouble)) implements composite ${ densevector_sub[Double]($0,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MInt),DenseVector(MInt)) :: DenseVector(MInt)) implements composite ${ densevector_sub[Int]($0,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MInt),DenseVector(MFloat)) :: DenseVector(MFloat)) implements composite ${ densevector_sub[Float]($0.toFloat,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MInt),DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_sub[Double]($0.toDouble,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),DenseVector(MInt)) :: DenseVector(MFloat)) implements composite ${ densevector_sub[Float]($0,$1.toFloat) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),DenseVector(MFloat)) :: DenseVector(MFloat)) implements composite ${ densevector_sub[Float]($0,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MFloat),DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_sub[Double]($0.toDouble,$1) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),DenseVector(MInt)) :: DenseVector(MDouble)) implements composite ${ densevector_sub[Double]($0,$1.toDouble) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),DenseVector(MFloat)) :: DenseVector(MDouble)) implements composite ${ densevector_sub[Double]($0,$1.toDouble) }
    infix (DenseVector) ("-", Nil, (DenseVector(MDouble),DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_sub[Double]($0,$1) }    
    
    infix (DenseVector) ("unary_-", Nil, (DenseVector(MInt)) :: DenseVector(MInt)) implements composite ${ densevector_mul[Int]($0,unit(-1)) }
    infix (DenseVector) ("unary_-", Nil, (DenseVector(MFloat)) :: DenseVector(MFloat)) implements composite ${ densevector_mul[Float]($0,unit(-1f)) }
    infix (DenseVector) ("unary_-", Nil, (DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_mul[Double]($0,unit(-1.0)) }
    infix (DenseVector) ("*", Nil, (MInt,DenseVector(MInt)) :: DenseVector(MInt)) implements composite ${ densevector_mul[Int]($1,$0) }
    infix (DenseVector) ("*", Nil, (MInt,DenseVector(MFloat)) :: DenseVector(MFloat)) implements composite ${ densevector_mul[Float]($1,$0.toFloat) }
    infix (DenseVector) ("*", Nil, (MInt,DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_mul[Double]($1,$0.toDouble) }
    infix (DenseVector) ("*", Nil, (MFloat,DenseVector(MInt)) :: DenseVector(MFloat)) implements composite ${ densevector_mul[Float]($1.toFloat,$0) }
    infix (DenseVector) ("*", Nil, (MFloat,DenseVector(MFloat)) :: DenseVector(MFloat)) implements composite ${ densevector_mul[Float]($1,$0) }
    infix (DenseVector) ("*", Nil, (MFloat,DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_mul[Double]($1,$0.toDouble) }
    infix (DenseVector) ("*", Nil, (MDouble,DenseVector(MInt)) :: DenseVector(MDouble)) implements composite ${ densevector_mul[Double]($1.toDouble,$0) }
    infix (DenseVector) ("*", Nil, (MDouble,DenseVector(MFloat)) :: DenseVector(MDouble)) implements composite ${ densevector_mul[Double]($1.toDouble,$0) }
    infix (DenseVector) ("*", Nil, (MDouble,DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_mul[Double]($1,$0) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),MInt) :: DenseVector(MInt)) implements composite ${ densevector_mul[Int]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),MFloat) :: DenseVector(MFloat)) implements composite ${ densevector_mul[Float]($0.toFloat,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),MDouble) :: DenseVector(MDouble)) implements composite ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),MInt) :: DenseVector(MFloat)) implements composite ${ densevector_mul[Float]($0,$1.toFloat) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),MFloat) :: DenseVector(MFloat)) implements composite ${ densevector_mul[Float]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),MDouble) :: DenseVector(MDouble)) implements composite ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),MInt) :: DenseVector(MDouble)) implements composite ${ densevector_mul[Double]($0,$1.toDouble) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),MFloat) :: DenseVector(MDouble)) implements composite ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),MDouble) :: DenseVector(MDouble)) implements composite ${ densevector_mul[Double]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),DenseVector(MInt)) :: DenseVector(MInt)) implements composite ${ densevector_mul[Int]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),DenseVector(MFloat)) :: DenseVector(MFloat)) implements composite ${ densevector_mul[Float]($0.toFloat,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MInt),DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),DenseVector(MInt)) :: DenseVector(MFloat)) implements composite ${ densevector_mul[Float]($0,$1.toFloat) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),DenseVector(MFloat)) :: DenseVector(MFloat)) implements composite ${ densevector_mul[Float]($0,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MFloat),DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_mul[Double]($0.toDouble,$1) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),DenseVector(MInt)) :: DenseVector(MDouble)) implements composite ${ densevector_mul[Double]($0,$1.toDouble) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),DenseVector(MFloat)) :: DenseVector(MDouble)) implements composite ${ densevector_mul[Double]($0,$1.toDouble) }
    infix (DenseVector) ("*", Nil, (DenseVector(MDouble),DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_mul[Double]($0,$1) }    

    infix (DenseVector) ("/", Nil, (DenseVector(MInt),MInt) :: DenseVector(MInt)) implements composite ${ densevector_div[Int]($0,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MInt),MFloat) :: DenseVector(MFloat)) implements composite ${ densevector_div[Float]($0.toFloat,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MInt),MDouble) :: DenseVector(MDouble)) implements composite ${ densevector_div[Double]($0.toDouble,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),MInt) :: DenseVector(MFloat)) implements composite ${ densevector_div[Float]($0,$1.toFloat) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),MFloat) :: DenseVector(MFloat)) implements composite ${ densevector_div[Float]($0,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),MDouble) :: DenseVector(MDouble)) implements composite ${ densevector_div[Double]($0.toDouble,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),MInt) :: DenseVector(MDouble)) implements composite ${ densevector_div[Double]($0,$1.toDouble) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),MFloat) :: DenseVector(MDouble)) implements composite ${ densevector_div[Double]($0.toDouble,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),MDouble) :: DenseVector(MDouble)) implements composite ${ densevector_div[Double]($0,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MInt),DenseVector(MInt)) :: DenseVector(MInt)) implements composite ${ densevector_div[Int]($0,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MInt),DenseVector(MFloat)) :: DenseVector(MFloat)) implements composite ${ densevector_div[Float]($0.toFloat,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MInt),DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_div[Double]($0.toDouble,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),DenseVector(MInt)) :: DenseVector(MFloat)) implements composite ${ densevector_div[Float]($0,$1.toFloat) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),DenseVector(MFloat)) :: DenseVector(MFloat)) implements composite ${ densevector_div[Float]($0,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MFloat),DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_div[Double]($0.toDouble,$1) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),DenseVector(MInt)) :: DenseVector(MDouble)) implements composite ${ densevector_div[Double]($0,$1.toDouble) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),DenseVector(MFloat)) :: DenseVector(MDouble)) implements composite ${ densevector_div[Double]($0,$1.toDouble) }
    infix (DenseVector) ("/", Nil, (DenseVector(MDouble),DenseVector(MDouble)) :: DenseVector(MDouble)) implements composite ${ densevector_div[Double]($0,$1) }    
  }
}

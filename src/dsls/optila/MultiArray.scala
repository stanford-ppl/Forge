package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait MultiArrayOps {
  this: OptiLADSL =>

  /** 
   * Common functions across MultiArray types
   * Requires the following methods:
   * - direct 'raw_data' method - extracts MultiArray subclass from struct
   */
  def addMultiArrayCommonOps(MA: Rep[DSLType], ndims: Int, mult: String, wrapper: Rep[String] => OpType) {
    val T = tpePar("T")
    val R = tpePar("R")
    val S = tpePar("S")
    val name = MA.name.toLowerCase
    val MIntArgs = List.fill(ndims){MInt}
    // Default vector is in 2nd dimension (row vector)
    val argListN = List.tabulate(ndims){i => quotedArg(i)}.mkString(",")
    
    compiler (MA) ("fmultia_shape_mismatch", T, (MA(T), MA(T)) :: MBoolean) implements composite ${
      Seq.tabulate(\$ndims){d => $0.dim(d) != $1.dim(d)}.reduce{_||_}
    }

    // Multidimensional array constructors
    val Ctr = lookupGrp("Constructors")
    direct (Ctr) ("zeros", Nil, MIntArgs :: MA(MDouble)) implements wrapper ${ fmultia_fromfunction[Double](\$argListN, {i => 0.0}) }
    direct (Ctr) ("zerosf", Nil, MIntArgs :: MA(MFloat)) implements wrapper ${ fmultia_fromfunction[Float](\$argListN, {i => 0.0f}) }
    direct (Ctr) ("ones", Nil, MIntArgs :: MA(MDouble)) implements wrapper ${ fmultia_fromfunction[Double](\$argListN, {i => 1.0}) }
    direct (Ctr) ("onesf", Nil, MIntArgs :: MA(MFloat)) implements wrapper ${ fmultia_fromfunction[Float](\$argListN, {i => 1.0f}) }
    direct (Ctr) ("rand", Nil, MIntArgs :: MA(MDouble)) implements wrapper ${ fmultia_fromfunction[Double](\$argListN, {i => random[Double]}) }
    direct (Ctr) ("randf", Nil, MIntArgs :: MA(MFloat)) implements wrapper ${ fmultia_fromfunction[Float](\$argListN, {i => random[Float]}) }
    direct (Ctr) ("randn", Nil, MIntArgs :: MA(MDouble)) implements wrapper ${ fmultia_fromfunction[Double](\$argListN, {i => randomGaussian}) }
    direct (Ctr) ("randnf", Nil, MIntArgs :: MA(MFloat)) implements wrapper ${ fmultia_fromfunction[Float](\$argListN, {i => randomGaussian.toFloat}) }
    // These don't seem particularly useful
    //direct (Ctr) ("falses", Nil, MIntArgs :: MA(MBoolean)) implements wrapper ${ fmultia_fromfunc(\$argListN, false, {s: Seq[Rep[Int]] => false}) }
    //direct (Ctr) ("trues", Nil, MIntArgs :: MA(MBoolean)) implements wrapper ${ fmultia_fromfunc(\$argListN, false, {s: Seq[Rep[Int]] => true}) }
   
    infix (MA) ("||", Nil, (MA(MBoolean), MA(MBoolean)) :: MA(MBoolean)) implements composite ${ $0.zip($1){(a,b) => a || b } }
    infix (MA) ("&&", Nil, (MA(MBoolean), MA(MBoolean)) :: MA(MBoolean)) implements composite ${ $0.zip($1){(a,b) => a && b } }
    infix (MA) ("unary_!", Nil, MA(MBoolean) :: MA(MBoolean)) implements composite ${ $0.map{a => !a} }
    
    val MultiArrayCommonOps = withTpe(MA)
    MultiArrayCommonOps {
      // --- Properties
      infix ("dim") (IInt :: MInt) implements composite ${ 
        fassert($1 > 0, "Argument to dim must be greater than zero")
        raw_data($self).dim($1) 
      }
      infix ("dimIndices") (IInt :: IndexVector) implements composite ${  
        fassert($1 > 0, "Dimension argument to dimIndices must be greater than zero")
        IndexVector(0, $self.dim($1), $1)
      }

      // Aliases for specific dimension sizes
      // TODO: keep "numPages" and "numBanks"? These aren't entirely intuitive
      infix ("numRows") (Nil :: MInt) implements composite ${ $self.dim(1) }
      infix ("numCols") (Nil :: MInt) implements composite ${ $self.dim(2) }
      infix ("numPages") (Nil :: MInt) implements composite ${ $self.dim(3) }
      infix ("numBanks") (Nil :: MInt) implements composite ${ $self.dim(4) }
      infix ("rowIndices") (Nil :: IndexVector) implements composite ${ $self.dimIndices(1) }
      infix ("colIndices") (Nil :: IndexVector) implements composite ${ $self.dimIndices(2) }
      infix ("pageIndices") (Nil :: IndexVector) implements composite ${ $self.dimIndices(3) }
      infix ("bankIndices") (Nil :: IndexVector) implements composite ${ $self.dimIndices(4) }
      infix ("size") (Nil :: MInt) implements composite ${ raw_data($self).size }

      if (ndims > 1) 
        infix ("indices") (Nil :: IndexVector) implements composite ${ IndexVector(0, $self.size, unit(true)) }
      else
        infix ("indices") (Nil :: IndexVector) implements composite ${ IndexVector(0, $self.size, $self.isRow) }

      // --- Conversions
      infix ("toBoolean") (Nil :: MA(MBoolean), ("conv",T ==> MBoolean)) implements composite ${ $self.map($conv(_))}
      infix ("toDouble") (Nil :: MA(MDouble), ("conv",T ==> MDouble)) implements composite ${ $self.map($conv(_))}
      infix ("toFloat") (Nil :: MA(MFloat), ("conv",T ==> MFloat)) implements composite ${ $self.map($conv(_))}
      infix ("toLong") (Nil :: MA(MLong), ("conv",T ==> MLong)) implements composite ${ $self.map($conv(_))}
      infix ("toInt") (Nil :: MA(MInt), ("conv",T ==> MInt)) implements composite ${ $self.map($conv(_))}

      // --- Ordering
      infix ("<")( MA(T) :: MA(MBoolean), TOrder(T)) implements composite ${ $self.zip($1){(a,b) => implicitly[Order[T]].lt(a,b)} }
      infix ("<=") (MA(T) :: MA(MBoolean), TOrder(T)) implements composite ${ $self.zip($1){(a,b) => implicitly[Order[T]].lteq(a,b)} }
      infix (">") (MA(T) :: MA(MBoolean), TOrder(T)) implements composite ${ $self.zip($1){(a,b) => implicitly[Order[T]].gt(a,b)} }
      infix (">=") (MA(T) :: MA(MBoolean), TOrder(T)) implements composite ${ $self.zip($1){(a,b) => implicitly[Order[T]].gteq(a,b)} }
      // element-wise equality comparison
      infix ("=:=") (MA(T):: MA(MBoolean)) implements composite ${ $self.zip($1){(a,b) => a == b} }
      infix ("=/=") (MA(T) :: MA(MBoolean)) implements composite ${ $self.zip($1){(a,b) => a != b} }
      
      infix ("<")( T :: MA(MBoolean), TOrder(T)) implements composite ${ $self.map{e => implicitly[Order[T]].lt(e,$1)} }
      infix ("<=") (T :: MA(MBoolean), TOrder(T)) implements composite ${ $self.map{e => implicitly[Order[T]].lteq(e,$1)} }
      infix (">") (T :: MA(MBoolean), TOrder(T)) implements composite ${ $self.map{e => implicitly[Order[T]].gt(e,$1)} }
      infix (">=") (T :: MA(MBoolean), TOrder(T)) implements composite ${ $self.map{e => implicitly[Order[T]].gteq(e,$1)} }
      // element-wise equality comparison
      infix ("=:=") (T :: MA(MBoolean)) implements composite ${ $self.map{e => e == $1} }
      infix ("=/=") (T :: MA(MBoolean)) implements composite ${ $self.map{e => e != $1} }

      // full collection equality (shapes equal, all elements equal)
      direct ("__equal") (MA(T) :: MBoolean) implements composite ${
        if (fmultia_shape_mismatch($self,$1)) false
        else {
          $self.zip($1){(a,b) => a == b}.fold(unit(true)){(a,b) => a && b}
        }
      }

      // --- Copiers/mutability
      infix ("Clone") (Nil :: MA(T), aliasHint = copies(0)) implements wrapper ${ raw_data($self).Clone }
      infix ("mutable") (Nil :: MA(T), effect = mutable, aliasHint = copies(0)) implements wrapper ${raw_data($self).mutable }
      //infix ("unsafeImmutable") (Nil :: MA(T)) implements composite s"""fmultia_copy(self, mutable = false, unsafe = true).$tcast"""
      //infix ("unsafeMutable") (Nil :: MA(T)) implements composite s"""fmultia_copy(self, mutable = true, unsafe = true).$tcast"""

      // --- Reshaping
      infix ("reshape") ((MInt, MBoolean) :: DenseVector(T)) implements composite ${
        fassert($1 == $self.size, "Dimension mismatch - Number of elements must not change in reshape")
        densevector_from_array1d(raw_data($self).reshape($1), $2)
      }
      infix ("reshape") (MInt :: DenseVector(T)) implements redirect ${ $self.reshape($1,unit(true)) }

      // --- Basic Arith
      infix ("abs") (Nil :: MA(T), TArith(T)) implements composite ${ $0 map {e => abs(e) }}
      infix ("square") (Nil :: MA(T), TArith(T)) implements composite ${ $0.map {e => square(e) }}
      infix ("negate") (Nil :: MA(T), TArith(T)) implements composite ${ $0.map {e => implicitly[Arith[T]].negate(e) }}
      infix ("ceil") (Nil :: MA(T), TArith(T)) implements composite ${ $0.map {e => ceil(e) }}
      infix ("floor") (Nil :: MA(T), TArith(T)) implements composite ${ $0.map {e => floor(e) }}

      // Legacy Arith
      infix ("exp") (Nil :: MA(T), TArith(T)) implements composite ${ $0.map{e => e.log} }
      infix ("log") (Nil :: MA(T), TArith(T)) implements composite ${ $0.map{e => e.log} }

      // --- Math operations
      infix ("unary_-") (Nil :: MA(T), TArith(T)) implements redirect ${ $self.negate }
      infix ("+") (T :: MA(T), TArith(T)) implements composite ${ $self.map{e => implicitly[Arith[T]] + (e,$1) }}
      infix ("-") (T :: MA(T), TArith(T)) implements composite ${ $self.map{e => implicitly[Arith[T]] - (e,$1) }}
      infix ("*") (T :: MA(T), TArith(T)) implements composite ${ $self.map{e => implicitly[Arith[T]] * (e,$1) }}
      infix ("/") (T :: MA(T), TArith(T)) implements composite ${ $self.map{e => implicitly[Arith[T]] / (e,$1) }}
      infix ("+") (MA(T) :: MA(T), TArith(T)) implements composite ${ $self.zip($1){(a,b) => implicitly[Arith[T]] + (a,b) }}
      infix ("-") (MA(T) :: MA(T), TArith(T)) implements composite ${ $self.zip($1){(a,b) => implicitly[Arith[T]] - (a,b) }}
      infix ("/") (MA(T) :: MA(T), TArith(T)) implements composite ${ $self.zip($1){(a,b) => implicitly[Arith[T]] / (a,b) }}

      // --- Element updates (in place)
      infix ("+=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${ $self.mmap{e => e + $1 }}
      infix ("-=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${ $self.mmap{e => implicitly[Arith[T]] - (e,$1) }}
      infix ("*=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${ $self.mmap{e => implicitly[Arith[T]] * (e,$1) }}
      infix ("/=") (T :: MUnit, TArith(T), effect = write(0)) implements composite ${ $self.mmap{e => implicitly[Arith[T]] / (e,$1) }}
      infix ("+=") (MA(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${ $self.mzip($1){(a,b) => a + b }}
      infix ("-=") (MA(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${ $self.mzip($1){(a,b) => implicitly[Arith[T]] - (a,b) }}
      infix ("*=") (MA(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${ $self.mzip($1){(a,b) => implicitly[Arith[T]] * (a,b) }}
      infix ("/=") (MA(T) :: MUnit, TArith(T), effect = write(0)) implements composite ${ $self.mzip($1){(a,b) => implicitly[Arith[T]] / (a,b) }}

      // --- Ordering
      infix ("min") (Nil :: T, TOrder(T)) implements composite ${ $0.reduce{(a,b) => implicitly[Order[T]].min(a,b)} }
      infix ("max") (Nil :: T, TOrder(T)) implements composite ${ $0.reduce{(a,b) => implicitly[Order[T]].max(a,b)} }

      // --- Math operations
      infix ("sum") (Nil :: T, TArith(T)) implements composite ${ $self.reduce{(a,b) => implicitly[Arith[T]] + (a,b)} }
      infix ("prod") (Nil :: T, TArith(T)) implements composite ${ $self.reduceOne{(a,b) => implicitly[Arith[T]] * (a,b)} }
      infix ("mean") (Nil :: MDouble, (TArith(T), TNumeric(T))) implements composite ${ $self.sum.toDouble / $self.size }

      // --- Parallel operations
      infix ("groupBy") ((T ==> K, T ==> V) :: MMap(K, DenseVector(V)), addTpePars = (K,V)) implements composite ${
        val hash = raw_data($self).groupBy($1, $2)
        val vals = fmulmap_values(hash).map{ab => densevector_fromarray1d(ab, true)}
        fmulmap_from_1d_arrays(fmulmap_keys(hash), vals)
      }
      infix ("groupByReduce") ((T ==> K,T ==> V,(V,V) ==> V) :: MMap(K,V), TArith(V), addTpePars = (K,V)) implements composite ${
        raw_data($self).groupByReduce($1,$2,$3)
      }

      infix ("count") ((T ==> MBoolean) :: MInt) implements composite ${ $self.map{a => if ($1(a)) 1 else 0}.sum }
      infix ("foreach") ((T ==> MUnit) :: MUnit, effect = simple) implements composite ${ fmultia_foreach(raw_data($self), $1) }
      
      val loopIndices = Seq.tabulate(ndims){i => "li(" + i + ")"}.mkString(",")
      infix ("forIndices") (MIntArgs ==> MUnit :: MUnit, effect = simple) implements composite ${ 
        raw_data($self).forIndices{li => $1(\$loopIndices) }
      }
      // Reduce not yet available - only fold is currently supported
      //infix ("reduce") (((T, T) ==> T) :: T) implements composite "fmultia_reduce(" + rawData("self") + "," + quotedArg(1) + ")"
      // Reduction hacks to allow user to not give zero, even though we don't yet have fold/reduce distinction
      compiler ("fmultia_first_hack") (Nil :: T) implements composite ${
        fmultia_apply($0, findices_new(Seq.fill(\$ndims)(unit(0))) )
      }
      infix ("reduce") (((T, T) ==> T) :: T, TArith(T)) implements composite ${ fmultia_fold(raw_data($self), $1, implicitly[Arith[T]].zero(fmultia_first_hack(self))) }
      infix ("reduceOne") (((T, T) ==> T) :: T, TArith(T)) implements composite ${ fmultia_fold(raw_data($self), $1, implicitly[Arith[T]].one(fmultia_first_hack(self))) }
      
      infix ("fold") (CurriedMethodSignature(List(List(("zero", T)), List((T, T) ==> T)), T)) implements composite ${  fmultia_fold(raw_data($self), $2, $1) }
      infix ("map") ((T ==> R) :: MA(R), addTpePars = R) implements wrapper ${ fmultia_map(raw_data($self), $1) }
      infix ("zip") (CurriedMethodSignature(List(List(MA(S)), List((T,S) ==> R)), MA(R)), addTpePars = (S,R)) implements wrapper ${ fmultia_zip(raw_data($self), raw_data($1), $2) }

      // mutable map and zip (in place)
      infix ("mmap") ((T ==> T) :: MUnit, effect = write(0)) implements composite ${ raw_data($self).mmap($1) }
      infix ("mzip") (CurriedMethodSignature(List(List(MA(T)), List((T,T) ==> T)), MUnit), effect = write(0)) implements composite ${ raw_data($self).mzip($1)($2) }

      // --- Miscellaneous
      // $self.toString doesn't work in Delite, since there is no 'self' instance
      val delims = if (ndims == 1) s"""(if (self.isRow) "" else "\\n")"""
                   else Seq.tabulate(ndims){d => if (d  < ndims - 2) "\"\\n\\n\"" else if (d == ndims - 2) "\"\\n\"" else "\"\"" }.mkString(", ")

      // These hide the actual implementation, but after transformation these methods are pretty
      // much the same as they were in the previous version of optila. 
      // Still need null checks (don't really want a DeliteNullCheck node just for this)
      infix ("makeString") (Nil :: MString, TStringable(T)) implements composite ${
        if (self == null) unit("null")
        else raw_data($self).mkStringFunc(\$delims){x => optila_padspace(x.makeStr) }
      }
      infix ("toString") (Nil :: MString) implements composite ${
        if (self == null) unit("null")
        else raw_data($self).mkStringFunc(\$delims){x => optila_padspace(optila_fmt_str(x)) }
      }

      infix ("pprint") (Nil :: MUnit, TStringable(T), effect = simple) implements composite ${ println($self.makeStr + "\\n") }
    } /* End of MultiArrayCommonOps */
    
    // --- Math
    val Math = lookupGrp("BasicMath")
    UnaryMath.forall{(op, T, R) => direct (Math) (op, Nil, MA(T) :: MA(R)) implements composite { quotedArg(0) + ".map{" + op + "(_)}" } }
    direct (Math) ("sum", T, MA(T) :: T, TArith(T)) implements composite ${ $0.sum }
    direct (Math) ("prod", T, MA(T) :: T, TArith(T)) implements composite ${ $0.prod }
    // Arith for summation, Numeric for converting to Double
    // TBD: old version used implicit conv(T => Double) - are there any apps that need this?
    direct (Math) ("mean", T, MA(T) :: MDouble, (TArith(T), TNumeric(T))) implements composite ${ $0.mean }

    // --- Ordering
    val Order = lookupGrp("BasicOrder")
    direct (Order) ("min", T, MA(T) :: T, TOrder(T)) implements composite ${ $0.min }
    direct (Order) ("max", T, MA(T) :: T, TOrder(T)) implements composite ${ $0.max }

    // --- Implicit type casting in math and comparison ops
    CastHelp.pairs{ (T, B, R) => 
      infix (MA) ("+", Nil, (T, MA(B)) :: MA(R)) implements redirect {name + "_pl[" + R.name + "]" + CastHelp.reorderCasting((1, B), (0, T)) }
      infix (MA) ("-", Nil, (T, MA(B)) :: MA(R)) implements redirect {name + "_sub[" + R.name + "]" + CastHelp.reorderCasting((1, B), (0, T)) } 
      infix (MA) ("*", Nil, (T, MA(B)) :: MA(R)) implements redirect {name + "_mul[" + R.name + "]" + CastHelp.reorderCasting((1, B), (0, T)) }
      infix (MA) ("/", Nil, (T, MA(B)) :: MA(R)) implements redirect {name + "_div[" + R.name + "]" + CastHelp.reorderCasting((1, B), (0, T)) }
      
      infix (MA) ("+", Nil, (MA(T), B) :: MA(R)) implements redirect {name + "_pl[" + R.name + "]" + CastHelp.casting(T, B) }
      infix (MA) ("-", Nil, (MA(T), B) :: MA(R)) implements redirect {name + "_sub[" + R.name + "]" + CastHelp.casting(T, B) } 
      infix (MA) ("*", Nil, (MA(T), B) :: MA(R)) implements redirect {name + "_mul[" + R.name + "]" + CastHelp.casting(T, B) }
      infix (MA) ("/", Nil, (MA(T), B) :: MA(R)) implements redirect {name + "_div[" + R.name + "]" + CastHelp.casting(T, B) }
      
      // MA * MA Not included since Matrix * Matrix has a different meaning than others
      infix (MA) ("+", Nil, (MA(T), MA(B)) :: MA(R)) implements redirect {name + "_pl[" + R.name + "]" + CastHelp.casting(T, B) }
      infix (MA) ("-", Nil, (MA(T), MA(B)) :: MA(R)) implements redirect {name + "_sub[" + R.name + "]" + CastHelp.casting(T, B) }
      infix (MA) ("/", Nil, (MA(T), MA(B)) :: MA(R)) implements redirect {name + "_div[" + R.name + "]" + CastHelp.casting(T, B) }
    }
    
    CastHelp.leftDomPairs{ (T,B) => 
      infix (MA) ("+=", Nil, (MA(T), B) :: MUnit, effect = write(0)) implements redirect {name + "_pleq[" + T.name + "]" + CastHelp.casting(T, B) }
      infix (MA) ("-=", Nil, (MA(T), B) :: MUnit, effect = write(0)) implements redirect {name + "_subeq[" + T.name + "]" + CastHelp.casting(T, B) }
      infix (MA) ("*=", Nil, (MA(T), B) :: MUnit, effect = write(0)) implements redirect {name + "_muleq[" + T.name + "]" + CastHelp.casting(T, B) }
      infix (MA) ("/=", Nil, (MA(T), B) :: MUnit, effect = write(0)) implements redirect {name + "_diveq[" + T.name + "]" + CastHelp.casting(T, B) }

      infix (MA) ("+=", Nil, (MA(T), MA(B)) :: MUnit, effect = write(0)) implements redirect {name + "_pleq[" + T.name + "]" + CastHelp.casting(T, B) }
      infix (MA) ("-=", Nil, (MA(T), MA(B)) :: MUnit, effect = write(0)) implements redirect {name + "_subeq[" + T.name + "]" + CastHelp.casting(T, B) }
      infix (MA) ("*=", Nil, (MA(T), MA(B)) :: MUnit, effect = write(0)) implements redirect {name + "_muleq[" + T.name + "]" + CastHelp.casting(T, B) }
      infix (MA) ("/=", Nil, (MA(T), MA(B)) :: MUnit, effect = write(0)) implements redirect {name + "_diveq[" + T.name + "]" + CastHelp.casting(T, B) }
    }

    CastHelp.pairs{ (T,B,_) => 
      infix (MA) ("<", Nil, (MA(T), B) :: MA(MBoolean)) implements redirect {name + "_lt" + CastHelp.casting(T, B) }
      infix (MA) ("<=", Nil, (MA(T), B) :: MA(MBoolean)) implements redirect {name + "_lteq" + CastHelp.casting(T, B) } 
      infix (MA) (">", Nil, (MA(T), B) :: MA(MBoolean)) implements redirect {name + "_gt" + CastHelp.casting(T, B) }
      infix (MA) (">=", Nil, (MA(T), B) :: MA(MBoolean)) implements redirect {name + "_gteq" + CastHelp.casting(T, B) }
      infix (MA) ("=:=", Nil, (MA(T), B) :: MA(MBoolean)) implements redirect {name + "_eqclneq" + CastHelp.casting(T, B) }
      infix (MA) ("=/=", Nil, (MA(T), B) :: MA(MBoolean)) implements redirect {name + "_eqdiveq" + CastHelp.casting(T, B) }
    
      infix (MA) ("<", Nil, (MA(T), MA(B)) :: MA(MBoolean)) implements redirect {name + "_lt" + CastHelp.casting(T, B) }
      infix (MA) ("<=", Nil, (MA(T), MA(B)) :: MA(MBoolean)) implements redirect {name + "_lteq" + CastHelp.casting(T, B) } 
      infix (MA) (">", Nil, (MA(T), MA(B)) :: MA(MBoolean)) implements redirect {name + "_gt" + CastHelp.casting(T, B) }
      infix (MA) (">=", Nil, (MA(T), MA(B)) :: MA(MBoolean)) implements redirect {name + "_gteq" + CastHelp.casting(T, B) }
      infix (MA) ("=:=", Nil, (MA(T), MA(B)) :: MA(MBoolean)) implements redirect {name + "_eqclneq" + CastHelp.casting(T, B) }
      infix (MA) ("=/=", Nil, (MA(T), MA(B)) :: MA(MBoolean)) implements redirect {name + "_eqdiveq" + CastHelp.casting(T, B) }
    }

    // temporary fix for constants not being lifted for some infix functions
    // TODO: This adds a large number of extra functions that should be unnecessary - remove as soon as possible
    CastHelp.leftConsts{ (T,B,R) =>
      infix (MA) ("+", Nil, (T, MA(B)) :: MA(R)) implements redirect {name + "_pl[" + R.name + "]" + CastHelp.reorderCasting((1,B),(0,T)) }
    }
    CastHelp.rightConsts{ (T,B,R) =>
      infix (MA) ("+", Nil, (MA(T), B) :: MA(R)) implements redirect {name + "_pl[" + R.name + "]" + CastHelp.casting(T, B) }
      infix (MA) ("=:=", Nil, (MA(T), B) :: MA(MBoolean)) implements redirect {name + "_eqclneq" + CastHelp.casting(T, B) }
      infix (MA) ("=/=", Nil, (MA(T), B) :: MA(MBoolean)) implements redirect {name + "_eqdiveq" + CastHelp.casting(T, B) }
    }
    CastHelp.leftDomRightConsts { (T,B) =>
      infix (MA) ("+=", Nil, (MA(T), B) :: MUnit, effect = write(0)) implements redirect {name + "_pleq[" + T.name + "]" + CastHelp.casting(T, B) }
      infix (MA) ("-=", Nil, (MA(T), B) :: MUnit, effect = write(0)) implements redirect {name + "_subeq[" + T.name + "]" + CastHelp.casting(T, B) }
      infix (MA) ("*=", Nil, (MA(T), B) :: MUnit, effect = write(0)) implements redirect {name + "_muleq[" + T.name + "]" + CastHelp.casting(T, B) }
      infix (MA) ("/=", Nil, (MA(T), B) :: MUnit, effect = write(0)) implements redirect {name + "_diveq[" + T.name + "]" + CastHelp.casting(T, B) }
    }
    
    // --- Add to Arith
    val shape = "Seq" + Seq.tabulate(ndims){d => rawData(0) + ".dim(" + d + ")"}.mkString("(",",",")")
    val emptyShape = "Seq" + Seq.fill(ndims)("0").mkString("(",",",")")

    val Arith = lookupGrp("Arith").asInstanceOf[Rep[DSLTypeClass]]
    val MDArith = tpeClassInst("Arith" + MA.name, T withBound TArith, Arith(MA(T)))
    infix (MDArith) ("one", T withBound TArith, MA(T) :: MA(T)) implements wrapper ${ fmultia_fromfunction(\$shape,{i => implicitly[Arith[T]].one(fmultia_apply(raw_data(self), i))})}
    infix (MDArith) ("zero", T withBound TArith, MA(T) :: MA(T)) implements wrapper ${ fmultia_fromfunction(\$shape,{i => implicitly[Arith[T]].zero(fmultia_apply(raw_data(self), i))}) } 
    infix (MDArith) ("empty", T withBound TArith, Nil :: MA(T)) implements wrapper ${ fmultia_new_immutable(\$emptyShape) }

    infix (MDArith) ("+", T withBound TArith, (MA(T), MA(T)) :: MA(T)) implements composite {name + "_pl(" + quotedArg(0) + "," + quotedArg(1) + ")"}
    infix (MDArith) ("-", T withBound TArith, (MA(T), MA(T)) :: MA(T)) implements composite {name + "_sub(" + quotedArg(0) + "," + quotedArg(1) + ")"}
    infix (MDArith) ("*", T withBound TArith, (MA(T), MA(T)) :: MA(T)) implements composite {name + "_" + mult + "(" + quotedArg(0) + "," + quotedArg(1) + ")"}
    infix (MDArith) ("/", T withBound TArith, (MA(T), MA(T)) :: MA(T)) implements composite {name + "_div(" + quotedArg(0) + "," + quotedArg(1) + ")"}
    infix (MDArith) ("negate", T withBound TArith, MA(T) :: MA(T)) implements composite {name + "_negate(" + quotedArg(0) + ")"}
    infix (MDArith) ("square", T withBound TArith, MA(T) :: MA(T)) implements composite {name + "_square(" + quotedArg(0) + ")"}
    infix (MDArith) ("abs", T withBound TArith, MA(T) :: MA(T)) implements composite {name + "_abs(" + quotedArg(0) + ")"}
    infix (MDArith) ("ceil", T withBound TArith, MA(T) :: MA(T)) implements composite {name + "_ceil(" + quotedArg(0) + ")"}
    infix (MDArith) ("floor", T withBound TArith, MA(T) :: MA(T)) implements composite {name + "_floor(" + quotedArg(0) + ")"}
    infix (MDArith) ("exp", T withBound TArith, MA(T) :: MA(T)) implements composite {name + "_exp(" + quotedArg(0) + ")"}
    infix (MDArith) ("log", T withBound TArith, MA(T) :: MA(T)) implements composite {name + "_log(" + quotedArg(0) + ")"}

    // --- Add to Stringable
    val Stringable = lookupGrp("Stringable").asInstanceOf[Rep[DSLTypeClass]]
    val MDString = tpeClassInst("Stringable" + MA.name, T withBound TStringable, Stringable(MA(T)))
    infix (MDString) ("makeStr", T withBound TStringable, MA(T) :: MString) implements composite ${ $0.makeString }
  } /* end of CommonMultiArrayOps */
}

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
    val A = tpePar("A")
    val R = tpePar("R")
    val S = tpePar("S")
    val name = MA.name.toLowerCase
    val MIntArgs = List.fill(ndims){MInt}
    // Default vector is in 2nd dimension (row vector)
    val argListN = List.tabulate(ndims){i => quotedArg(i)}.mkString(",")
    
    compiler (MA) ("fmultia_shape_mismatch", A, (MA(A), MA(A)) :: MBoolean) implements composite ${
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
      infix ("toBoolean") (Nil :: MA(MBoolean), ("conv",A ==> MBoolean)) implements composite ${ $self.map($conv(_))}
      infix ("toDouble") (Nil :: MA(MDouble), ("conv",A ==> MDouble)) implements composite ${ $self.map($conv(_))}
      infix ("toFloat") (Nil :: MA(MFloat), ("conv",A ==> MFloat)) implements composite ${ $self.map($conv(_))}
      infix ("toLong") (Nil :: MA(MLong), ("conv",A ==> MLong)) implements composite ${ $self.map($conv(_))}
      infix ("toInt") (Nil :: MA(MInt), ("conv",A ==> MInt)) implements composite ${ $self.map($conv(_))}

      // --- Ordering
      infix ("<")( MA(A) :: MA(MBoolean), TOrder(A)) implements composite ${ $self.zip($1){(a,b) => implicitly[Order[A]].lt(a,b)} }
      infix ("<=") (MA(A) :: MA(MBoolean), TOrder(A)) implements composite ${ $self.zip($1){(a,b) => implicitly[Order[A]].lteq(a,b)} }
      infix (">") (MA(A) :: MA(MBoolean), TOrder(A)) implements composite ${ $self.zip($1){(a,b) => implicitly[Order[A]].gt(a,b)} }
      infix (">=") (MA(A) :: MA(MBoolean), TOrder(A)) implements composite ${ $self.zip($1){(a,b) => implicitly[Order[A]].gteq(a,b)} }
      // element-wise equality comparison
      infix ("=:=") (MA(A):: MA(MBoolean)) implements composite ${ $self.zip($1){(a,b) => a == b} }
      infix ("=/=") (MA(A) :: MA(MBoolean)) implements composite ${ $self.zip($1){(a,b) => a != b} }
      
      infix ("<")( A :: MA(MBoolean), TOrder(A)) implements composite ${ $self.map{e => implicitly[Order[A]].lt(e,$1)} }
      infix ("<=") (A :: MA(MBoolean), TOrder(A)) implements composite ${ $self.map{e => implicitly[Order[A]].lteq(e,$1)} }
      infix (">") (A :: MA(MBoolean), TOrder(A)) implements composite ${ $self.map{e => implicitly[Order[A]].gt(e,$1)} }
      infix (">=") (A :: MA(MBoolean), TOrder(A)) implements composite ${ $self.map{e => implicitly[Order[A]].gteq(e,$1)} }
      // element-wise equality comparison
      infix ("=:=") (A :: MA(MBoolean)) implements composite ${ $self.map{e => e == $1} }
      infix ("=/=") (A :: MA(MBoolean)) implements composite ${ $self.map{e => e != $1} }

      // full collection equality (shapes equal, all elements equal)
      direct ("__equal") (MA(A) :: MBoolean) implements composite ${
        if (fmultia_shape_mismatch($self,$1)) false
        else {
          $self.zip($1){(a,b) => a == b}.fold(unit(true)){(a,b) => a && b}
        }
      }

      // --- Copiers/mutability
      infix ("Clone") (Nil :: MA(A), aliasHint = copies(0)) implements wrapper ${ raw_data($self).Clone }
      infix ("mutable") (Nil :: MA(A), effect = mutable, aliasHint = copies(0)) implements wrapper ${raw_data($self).mutable }
      //infix ("unsafeImmutable") (Nil :: MA(A)) implements composite s"""fmultia_copy(self, mutable = false, unsafe = true).$tcast"""
      //infix ("unsafeMutable") (Nil :: MA(A)) implements composite s"""fmultia_copy(self, mutable = true, unsafe = true).$tcast"""

      // --- Reshaping
      infix ("reshape") ((MInt, MBoolean) :: DenseVector(A)) implements composite ${
        fassert($1 == $self.size, "Dimension mismatch - Number of elements must not change in reshape")
        densevector_from_array1d(raw_data($self).reshape($1), $2)
      }
      infix ("reshape") (MInt :: DenseVector(A)) implements redirect ${ $self.reshape($1,unit(true)) }

      // --- Basic Arith
      infix ("abs") (Nil :: MA(A), TArith(A)) implements composite ${ $0 map {e => abs(e) }}
      infix ("square") (Nil :: MA(A), TArith(A)) implements composite ${ $0.map {e => square(e) }}
      infix ("negate") (Nil :: MA(A), TArith(A)) implements composite ${ $0.map {e => implicitly[Arith[A]].negate(e) }}
      infix ("ceil") (Nil :: MA(A), TArith(A)) implements composite ${ $0.map {e => ceil(e) }}
      infix ("floor") (Nil :: MA(A), TArith(A)) implements composite ${ $0.map {e => floor(e) }}

      // Legacy Arith
      infix ("exp") (Nil :: MA(A), TArith(A)) implements composite ${ $0.map{e => e.log} }
      infix ("log") (Nil :: MA(A), TArith(A)) implements composite ${ $0.map{e => e.log} }

      // --- Math operations
      infix ("unary_-") (Nil :: MA(A), TArith(A)) implements redirect ${ $self.negate }
      infix ("+") (A :: MA(A), TArith(A)) implements composite ${ $self.map{e => implicitly[Arith[A]] + (e,$1) }}
      infix ("-") (A :: MA(A), TArith(A)) implements composite ${ $self.map{e => implicitly[Arith[A]] - (e,$1) }}
      infix ("*") (A :: MA(A), TArith(A)) implements composite ${ $self.map{e => implicitly[Arith[A]] * (e,$1) }}
      infix ("/") (A :: MA(A), TArith(A)) implements composite ${ $self.map{e => implicitly[Arith[A]] / (e,$1) }}
      infix ("+") (MA(A) :: MA(A), TArith(A)) implements composite ${ $self.zip($1){(a,b) => implicitly[Arith[A]] + (a,b) }}
      infix ("-") (MA(A) :: MA(A), TArith(A)) implements composite ${ $self.zip($1){(a,b) => implicitly[Arith[A]] - (a,b) }}
      infix ("/") (MA(A) :: MA(A), TArith(A)) implements composite ${ $self.zip($1){(a,b) => implicitly[Arith[A]] / (a,b) }}

      // --- Element updates (in place)
      infix ("+=") (A :: MUnit, TArith(A), effect = write(0)) implements composite ${ $self.mmap{e => e + $1 }}
      infix ("-=") (A :: MUnit, TArith(A), effect = write(0)) implements composite ${ $self.mmap{e => implicitly[Arith[A]] - (e,$1) }}
      infix ("*=") (A :: MUnit, TArith(A), effect = write(0)) implements composite ${ $self.mmap{e => implicitly[Arith[A]] * (e,$1) }}
      infix ("/=") (A :: MUnit, TArith(A), effect = write(0)) implements composite ${ $self.mmap{e => implicitly[Arith[A]] / (e,$1) }}
      infix ("+=") (MA(A) :: MUnit, TArith(A), effect = write(0)) implements composite ${ $self.mzip($1){(a,b) => a + b }}
      infix ("-=") (MA(A) :: MUnit, TArith(A), effect = write(0)) implements composite ${ $self.mzip($1){(a,b) => implicitly[Arith[A]] - (a,b) }}
      infix ("*=") (MA(A) :: MUnit, TArith(A), effect = write(0)) implements composite ${ $self.mzip($1){(a,b) => implicitly[Arith[A]] * (a,b) }}
      infix ("/=") (MA(A) :: MUnit, TArith(A), effect = write(0)) implements composite ${ $self.mzip($1){(a,b) => implicitly[Arith[A]] / (a,b) }}

      // --- Ordering
      infix ("min") (Nil :: A, TOrder(A)) implements composite ${ $0.reduce{(a,b) => implicitly[Order[A]].min(a,b)} }
      infix ("max") (Nil :: A, TOrder(A)) implements composite ${ $0.reduce{(a,b) => implicitly[Order[A]].max(a,b)} }

      // --- Math operations
      infix ("sum") (Nil :: A, TArith(A)) implements composite ${ $self.reduce{(a,b) => implicitly[Arith[A]] + (a,b)} }
      infix ("prod") (Nil :: A, TArith(A)) implements composite ${ $self.reduceOne{(a,b) => implicitly[Arith[A]] * (a,b)} }
      infix ("mean") (Nil :: MDouble, (TArith(A), TNumeric(A))) implements composite ${ $self.sum.toDouble / $self.size }

      // --- Parallel operations
      infix ("groupBy") ((A ==> K, A ==> V) :: MMap(K, DenseVector(V)), addTpePars = (K,V)) implements composite ${
        val hash = raw_data($self).groupBy($1, $2)
        val vals = fmulmap_values(hash).map{ab => densevector_fromarray1d(ab, true)}
        fmulmap_from_1d_arrays(fmulmap_keys(hash), vals)
      }
      infix ("groupByReduce") ((A ==> K,A ==> V,(V,V) ==> V) :: MMap(K,V), TArith(V), addTpePars = (K,V)) implements composite ${
        raw_data($self).groupByReduce($1,$2,$3)
      }

      infix ("count") ((A ==> MBoolean) :: MInt) implements composite ${ $self.map{a => if ($1(a)) 1 else 0}.sum }
      infix ("foreach") ((A ==> MUnit) :: MUnit, effect = simple) implements composite ${ fmultia_foreach(raw_data($self), $1) }
      
      val loopIndices = Seq.tabulate(ndims){i => "li(" + i + ")"}.mkString(",")
      infix ("forIndices") (MIntArgs ==> MUnit :: MUnit, effect = simple) implements composite ${ 
        raw_data($self).forIndices{li => $1(\$loopIndices) }
      }
      // Reduce not yet available - only fold is currently supported
      //infix ("reduce") (((A, A) ==> A) :: A) implements composite "fmultia_reduce(" + rawData("self") + "," + quotedArg(1) + ")"
      // Reduction hacks to allow user to not give zero, even though we don't yet have fold/reduce distinction
      compiler ("fmultia_first_hack") (Nil :: A) implements composite ${
        fmultia_apply($0, findices_new(Seq.fill(\$ndims)(unit(0))) )
      }
      infix ("reduce") (((A, A) ==> A) :: A, TArith(A)) implements composite ${ fmultia_fold(raw_data($self), $1, implicitly[Arith[A]].zero(fmultia_first_hack(self))) }
      infix ("reduceOne") (((A, A) ==> A) :: A, TArith(A)) implements composite ${ fmultia_fold(raw_data($self), $1, implicitly[Arith[A]].one(fmultia_first_hack(self))) }
      
      infix ("fold") (CurriedMethodSignature(List(List(("zero", A)), List((A, A) ==> A)), A)) implements composite ${  fmultia_fold(raw_data($self), $2, $1) }
      infix ("map") ((A ==> R) :: MA(R), addTpePars = R) implements wrapper ${ fmultia_map(raw_data($self), $1) }
      infix ("zip") (CurriedMethodSignature(List(List(MA(S)), List((A,S) ==> R)), MA(R)), addTpePars = (S,R)) implements wrapper ${ fmultia_zip(raw_data($self), raw_data($1), $2) }

      // mutable map and zip (in place)
      infix ("mmap") ((A ==> A) :: MUnit, effect = write(0)) implements composite ${ raw_data($self).mmap($1) }
      infix ("mzip") (CurriedMethodSignature(List(List(MA(A)), List((A,A) ==> A)), MUnit), effect = write(0)) implements composite ${ raw_data($self).mzip($1)($2) }

      // --- Miscellaneous
      // $self.toString doesn't work in Delite, since there is no 'self' instance
      val delims = if (ndims == 1) s"""(if (self.isRow) "" else "\\n")"""
                   else Seq.tabulate(ndims){d => if (d  < ndims - 2) "\"\\n\\n\"" else if (d == ndims - 2) "\"\\n\"" else "\"\"" }.mkString(", ")

      // These hide the actual implementation, but after transformation these methods are pretty
      // much the same as they were in the previous version of optila. 
      // Still need null checks (don't really want a DeliteNullCheck node just for this)
      infix ("makeString") (Nil :: MString, TStringable(A)) implements composite ${
        if (self == null) unit("null")
        else raw_data($self).mkStringFunc(\$delims){x => optila_padspace(x.makeStr) }
      }
      infix ("toString") (Nil :: MString) implements composite ${
        if (self == null) unit("null")
        else raw_data($self).mkStringFunc(\$delims){x => optila_padspace(optila_fmt_str(x)) }
      }

      infix ("pprint") (Nil :: MUnit, TStringable(A), effect = simple) implements composite ${ println($self.makeStr + "\\n") }
    } /* End of MultiArrayCommonOps */
    
    // --- Math
    val Math = lookupGrp("BasicMath")
    UnaryMath.forall{(op, A, R) => direct (Math) (op, Nil, MA(A) :: MA(R)) implements composite { quotedArg(0) + ".map{" + op + "(_)}" } }
    direct (Math) ("sum", A, MA(A) :: A, TArith(A)) implements composite ${ $0.sum }
    direct (Math) ("prod", A, MA(A) :: A, TArith(A)) implements composite ${ $0.prod }
    // Arith for summation, Numeric for converting to Double
    // TBD: old version used implicit conv(A => Double) - are there any apps that need this?
    direct (Math) ("mean", A, MA(A) :: MDouble, (TArith(A), TNumeric(A))) implements composite ${ $0.mean }

    // --- Ordering
    val Order = lookupGrp("BasicOrder")
    direct (Order) ("min", A, MA(A) :: A, TOrder(A)) implements composite ${ $0.min }
    direct (Order) ("max", A, MA(A) :: A, TOrder(A)) implements composite ${ $0.max }

    // --- Implicit type casting in math and comparison ops
    CastHelp.pairs{ (A, B, R) => 
      infix (MA) ("+", Nil, (A, MA(B)) :: MA(R)) implements redirect {name + "_pl[" + R.name + "]" + CastHelp.reorderCasting((1, B), (0, A)) }
      infix (MA) ("-", Nil, (A, MA(B)) :: MA(R)) implements redirect {name + "_sub[" + R.name + "]" + CastHelp.reorderCasting((1, B), (0, A)) } 
      infix (MA) ("*", Nil, (A, MA(B)) :: MA(R)) implements redirect {name + "_mul[" + R.name + "]" + CastHelp.reorderCasting((1, B), (0, A)) }
      infix (MA) ("/", Nil, (A, MA(B)) :: MA(R)) implements redirect {name + "_div[" + R.name + "]" + CastHelp.reorderCasting((1, B), (0, A)) }
      
      infix (MA) ("+", Nil, (MA(A), B) :: MA(R)) implements redirect {name + "_pl[" + R.name + "]" + CastHelp.casting(A, B) }
      infix (MA) ("-", Nil, (MA(A), B) :: MA(R)) implements redirect {name + "_sub[" + R.name + "]" + CastHelp.casting(A, B) } 
      infix (MA) ("*", Nil, (MA(A), B) :: MA(R)) implements redirect {name + "_mul[" + R.name + "]" + CastHelp.casting(A, B) }
      infix (MA) ("/", Nil, (MA(A), B) :: MA(R)) implements redirect {name + "_div[" + R.name + "]" + CastHelp.casting(A, B) }
      
      // MA * MA Not included since Matrix * Matrix has a different meaning than others
      infix (MA) ("+", Nil, (MA(A), MA(B)) :: MA(R)) implements redirect {name + "_pl[" + R.name + "]" + CastHelp.casting(A, B) }
      infix (MA) ("-", Nil, (MA(A), MA(B)) :: MA(R)) implements redirect {name + "_sub[" + R.name + "]" + CastHelp.casting(A, B) }
      infix (MA) ("/", Nil, (MA(A), MA(B)) :: MA(R)) implements redirect {name + "_div[" + R.name + "]" + CastHelp.casting(A, B) }
    }
    
    CastHelp.leftDomPairs{ (A,B) => 
      infix (MA) ("+=", Nil, (MA(A), B) :: MUnit, effect = write(0)) implements redirect {name + "_pleq[" + A.name + "]" + CastHelp.casting(A, B) }
      infix (MA) ("-=", Nil, (MA(A), B) :: MUnit, effect = write(0)) implements redirect {name + "_subeq[" + A.name + "]" + CastHelp.casting(A, B) }
      infix (MA) ("*=", Nil, (MA(A), B) :: MUnit, effect = write(0)) implements redirect {name + "_muleq[" + A.name + "]" + CastHelp.casting(A, B) }
      infix (MA) ("/=", Nil, (MA(A), B) :: MUnit, effect = write(0)) implements redirect {name + "_diveq[" + A.name + "]" + CastHelp.casting(A, B) }

      infix (MA) ("+=", Nil, (MA(A), MA(B)) :: MUnit, effect = write(0)) implements redirect {name + "_pleq[" + A.name + "]" + CastHelp.casting(A, B) }
      infix (MA) ("-=", Nil, (MA(A), MA(B)) :: MUnit, effect = write(0)) implements redirect {name + "_subeq[" + A.name + "]" + CastHelp.casting(A, B) }
      infix (MA) ("*=", Nil, (MA(A), MA(B)) :: MUnit, effect = write(0)) implements redirect {name + "_muleq[" + A.name + "]" + CastHelp.casting(A, B) }
      infix (MA) ("/=", Nil, (MA(A), MA(B)) :: MUnit, effect = write(0)) implements redirect {name + "_diveq[" + A.name + "]" + CastHelp.casting(A, B) }
    }

    CastHelp.pairs{ (A,B,_) => 
      infix (MA) ("<", Nil, (MA(A), B) :: MA(MBoolean)) implements redirect {name + "_lt" + CastHelp.casting(A, B) }
      infix (MA) ("<=", Nil, (MA(A), B) :: MA(MBoolean)) implements redirect {name + "_lteq" + CastHelp.casting(A, B) } 
      infix (MA) (">", Nil, (MA(A), B) :: MA(MBoolean)) implements redirect {name + "_gt" + CastHelp.casting(A, B) }
      infix (MA) (">=", Nil, (MA(A), B) :: MA(MBoolean)) implements redirect {name + "_gteq" + CastHelp.casting(A, B) }
      infix (MA) ("=:=", Nil, (MA(A), B) :: MA(MBoolean)) implements redirect {name + "_eqclneq" + CastHelp.casting(A, B) }
      infix (MA) ("=/=", Nil, (MA(A), B) :: MA(MBoolean)) implements redirect {name + "_eqdiveq" + CastHelp.casting(A, B) }
    
      infix (MA) ("<", Nil, (MA(A), MA(B)) :: MA(MBoolean)) implements redirect {name + "_lt" + CastHelp.casting(A, B) }
      infix (MA) ("<=", Nil, (MA(A), MA(B)) :: MA(MBoolean)) implements redirect {name + "_lteq" + CastHelp.casting(A, B) } 
      infix (MA) (">", Nil, (MA(A), MA(B)) :: MA(MBoolean)) implements redirect {name + "_gt" + CastHelp.casting(A, B) }
      infix (MA) (">=", Nil, (MA(A), MA(B)) :: MA(MBoolean)) implements redirect {name + "_gteq" + CastHelp.casting(A, B) }
      infix (MA) ("=:=", Nil, (MA(A), MA(B)) :: MA(MBoolean)) implements redirect {name + "_eqclneq" + CastHelp.casting(A, B) }
      infix (MA) ("=/=", Nil, (MA(A), MA(B)) :: MA(MBoolean)) implements redirect {name + "_eqdiveq" + CastHelp.casting(A, B) }
    }

    // temporary fix for constants not being lifted for some infix functions
    // TODO: This adds a large number of extra functions that should be unnecessary - remove as soon as possible
    CastHelp.leftConsts{ (A,B,R) =>
      infix (MA) ("+", Nil, (A, MA(B)) :: MA(R)) implements redirect {name + "_pl[" + R.name + "]" + CastHelp.reorderCasting((1,B),(0,A)) }
    }
    CastHelp.rightConsts{ (A,B,R) =>
      infix (MA) ("+", Nil, (MA(A), B) :: MA(R)) implements redirect {name + "_pl[" + R.name + "]" + CastHelp.casting(A, B) }
      infix (MA) ("=:=", Nil, (MA(A), B) :: MA(MBoolean)) implements redirect {name + "_eqclneq" + CastHelp.casting(A, B) }
      infix (MA) ("=/=", Nil, (MA(A), B) :: MA(MBoolean)) implements redirect {name + "_eqdiveq" + CastHelp.casting(A, B) }
    }
    CastHelp.leftDomRightConsts { (A,B) =>
      infix (MA) ("+=", Nil, (MA(A), B) :: MUnit, effect = write(0)) implements redirect {name + "_pleq[" + A.name + "]" + CastHelp.casting(A, B) }
      infix (MA) ("-=", Nil, (MA(A), B) :: MUnit, effect = write(0)) implements redirect {name + "_subeq[" + A.name + "]" + CastHelp.casting(A, B) }
      infix (MA) ("*=", Nil, (MA(A), B) :: MUnit, effect = write(0)) implements redirect {name + "_muleq[" + A.name + "]" + CastHelp.casting(A, B) }
      infix (MA) ("/=", Nil, (MA(A), B) :: MUnit, effect = write(0)) implements redirect {name + "_diveq[" + A.name + "]" + CastHelp.casting(A, B) }
    }
    
    // --- Add to Arith
    val shape = "Seq" + Seq.tabulate(ndims){d => rawData(0) + ".dim(" + d + ")"}.mkString("(",",",")")
    val emptyShape = "Seq" + Seq.fill(ndims)("0").mkString("(",",",")")

    val Arith = lookupGrp("Arith").asInstanceOf[Rep[DSLTypeClass]]
    val MDArith = tpeClassInst("Arith" + MA.name, A withBound TArith, Arith(MA(A)))
    infix (MDArith) ("one", A withBound TArith, MA(A) :: MA(A)) implements wrapper ${ fmultia_fromfunction(\$shape,{i => implicitly[Arith[A]].one(fmultia_apply(raw_data(self), i))})}
    infix (MDArith) ("zero", A withBound TArith, MA(A) :: MA(A)) implements wrapper ${ fmultia_fromfunction(\$shape,{i => implicitly[Arith[A]].zero(fmultia_apply(raw_data(self), i))}) } 
    infix (MDArith) ("empty", A withBound TArith, Nil :: MA(A)) implements wrapper ${ fmultia_new_immutable(\$emptyShape) }

    infix (MDArith) ("+", A withBound TArith, (MA(A), MA(A)) :: MA(A)) implements composite {name + "_pl(" + quotedArg(0) + "," + quotedArg(1) + ")"}
    infix (MDArith) ("-", A withBound TArith, (MA(A), MA(A)) :: MA(A)) implements composite {name + "_sub(" + quotedArg(0) + "," + quotedArg(1) + ")"}
    infix (MDArith) ("*", A withBound TArith, (MA(A), MA(A)) :: MA(A)) implements composite {name + "_" + mult + "(" + quotedArg(0) + "," + quotedArg(1) + ")"}
    infix (MDArith) ("/", A withBound TArith, (MA(A), MA(A)) :: MA(A)) implements composite {name + "_div(" + quotedArg(0) + "," + quotedArg(1) + ")"}
    infix (MDArith) ("negate", A withBound TArith, MA(A) :: MA(A)) implements composite {name + "_negate(" + quotedArg(0) + ")"}
    infix (MDArith) ("square", A withBound TArith, MA(A) :: MA(A)) implements composite {name + "_square(" + quotedArg(0) + ")"}
    infix (MDArith) ("abs", A withBound TArith, MA(A) :: MA(A)) implements composite {name + "_abs(" + quotedArg(0) + ")"}
    infix (MDArith) ("ceil", A withBound TArith, MA(A) :: MA(A)) implements composite {name + "_ceil(" + quotedArg(0) + ")"}
    infix (MDArith) ("floor", A withBound TArith, MA(A) :: MA(A)) implements composite {name + "_floor(" + quotedArg(0) + ")"}
    infix (MDArith) ("exp", A withBound TArith, MA(A) :: MA(A)) implements composite {name + "_exp(" + quotedArg(0) + ")"}
    infix (MDArith) ("log", A withBound TArith, MA(A) :: MA(A)) implements composite {name + "_log(" + quotedArg(0) + ")"}

    // --- Add to Stringable
    val Stringable = lookupGrp("Stringable").asInstanceOf[Rep[DSLTypeClass]]
    val MDString = tpeClassInst("Stringable" + MA.name, A withBound TStringable, Stringable(MA(A)))
    infix (MDString) ("makeStr", A withBound TStringable, MA(A) :: MString) implements composite ${ $0.makeString }
  } /* end of CommonMultiArrayOps */
}

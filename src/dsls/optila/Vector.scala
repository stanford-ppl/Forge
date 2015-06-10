package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait VectorOps {
  this: OptiLADSL =>

  def addVectorCommonOps(v: Rep[DSLType], T: Rep[DSLType]) {
    val V = if (isTpePar(T)) v(T) else v
    val A = if (isTpePar(T)) List(TArith(asTpePar(T))) else Nil
    val S = if (isTpePar(T)) List(TStringable(asTpePar(T))) else Nil

    val R = tpePar("R")
    val IndexVector = lookupTpe("IndexVector")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val Tuple2 = lookupTpe("Tup2")  

    /** 
     * Common vector ops (between IndexVector and DenseVector)
     * Requires the following methods:
     * - infix 'isCol' method (Boolean)
     * - infix 'isRow' method (Boolean)
     * - infix 'length' method
     * - direct 'isRange' method (for IndexVectors, can be const. false otherwise)
     * - direct 'isWild' method (for IndexVectors, can be const. false otherwise)
     */
    val VectorCommonOps = withTpe(v)
    VectorCommonOps {
      // --- Accessors
      infix ("isEmpty") (Nil :: MBoolean) implements composite ${ 
        fassert(!isWild($self), "Method isEmpty is not defined for Wildcard")
        $self.length == 0 
      }

      // --- Single element accesses
      infix ("first") (Nil :: T) implements composite ${ 
        fassert(!isWild($self), "Method first is not defined for Wildcard")
        $self(0) 
      }
      infix ("last") (Nil :: T) implements composite ${ 
        fassert(!isWild($self), "Method last is not defined for Wildcard")
        $self($self.length - 1) 
      }

      // --- Continuous accesses
      infix ("slice") ( (("start", MInt), ("end", MInt)) :: V) implements composite ${ $0.slice(start::end) }
      infix ("drop") (MInt :: V) implements composite ${ 
        fassert(!isWild($self), "Method drop is not defined for Wildcard")
        $self.slice($1, $self.length) 
      }
      infix ("take") (MInt :: V) implements composite ${ 
        fassert(!isWild($self), "Method take is not defined for Wildcard")
        $self.slice(0, $1) 
      } 

      // continuous/discontinuous accesses
      infix ("apply") (IndexVector :: V) implements composite ${ 
        fassert(!isWild($self), "Slice is not defined for Wildcard")
      	if (isRange($1)) $self.slice($1)
      	else $self.dice($1) 
      }

      // --- Reshaping

      // TODO: Generalize this to n-dimensions
      infix ("replicate") ( (MInt, MInt) :: DenseMatrix(T)) implements composite ${
        fassert(!isWild($self), "Method replicate is not defined for Wildcard")
        if ($0.isCol)
          (0::($1*$0.length), 0::$2) {(i,j) => $0(i % $0.length) }
        else
          (0::$1, 0::($2*$0.length)) {(i,j) => $0(j % $0.length) }
      }

      // --- List operations
      infix ("find") ((T ==> MBoolean) :: IndexVector) implements composite ${ 
        fassert(!isWild($self), "Method find is not defined for Wildcard")
        $self.indices.filter(i => $1($self(i))) 
      }

      infix ("scan") (CurriedMethodSignature(List(List(("zero", R)), List((R,T) ==> R)), DenseVector(R)), addTpePars = R) implements composite ${
        fassert(!isWild($self), "Method scan is not defined for Wildcard")
        val out = DenseVector[R]($self.length, $self.isRow)
        out(0) = $2(zero, $self(0))
        var i = 1
        while (i < $self.length) {
          out(i) = $2(out(i - 1), $self(i))
          i += 1
        }
        out.unsafeImmutable
      }

      // Two filters instead of a sequential loop with mutable additions
      // Technically O(2n) instead of O(n) - but these two loops should always fuse
      infix("partition") ((T ==> MBoolean) :: Tuple2(DenseVector(T), DenseVector(T))) implements composite ${
        fassert(!isWild($self), "Method partition is not defined for Wildcard")
        val outT = $self.toDense.filter{$1}
        val outF = $self.toDense.filter{e => !$1(e)}
        pack((outT, outF))
      } 
      
      // --- Data exchange
      // TODO: Need to implement layout pinning for extracting Arrays (not Array1Ds)
      //infix ("toArray") (Nil :: MArray(T)) implements composite ${ densevector_raw_data($self.map(e => e)) }
      infix ("toArray1D") (Nil :: MArray1D(T)) implements composite ${ densevector_raw_data($self).Clone }

    }
  }

}
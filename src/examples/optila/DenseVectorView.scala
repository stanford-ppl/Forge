package ppl.dsl.forge
package examples
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait DenseVectorViewOps {
  this: OptiLADSL => 
 
  def importDenseVectorViewOps() {
    val T = tpePar("T")
    val DenseVectorView = tpe("DenseVectorView", T) 
    val DenseVector = lookupTpe("DenseVector")
  
    // data fields     
    data(DenseVectorView, ("_data", MArray(T)), ("_start", MInt), ("_stride", MInt), ("_length", MInt), ("_isRow", MBoolean))      
  
    // static methods
    static (DenseVectorView) ("apply", T, ((MArray(T), MInt ,MInt, MInt, MBoolean) :: DenseVectorView)) implements allocates(DenseVectorView, ${$0}, ${$1}, ${$2}, ${$3}, ${$4})
        
    val DenseVectorViewOps = withTpe(DenseVectorView)
    DenseVectorViewOps {
      compiler ("densevectorview_data") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("densevectorview_start") (Nil :: MInt) implements getter(0, "_start")
      compiler ("densevectorview_stride") (Nil :: MInt) implements getter(0, "_stride")
      
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("isRow") (Nil :: MBoolean) implements getter(0, "_isRow")
      infix ("apply") (MInt :: T) implements composite ${ array_apply(densevectorview_data($self), densevectorview_start($self) + $1*densevectorview_stride($self)) }      
      infix ("toDense") (Nil :: DenseVector(T)) implements composite ${ viewToDense($self) }
            
      fimplicit ("viewToDense") (Nil :: DenseVector(T)) implements composite ${
        val out = DenseVector[T]($self.length, $self.isRow)
        // out.zip[T,T]($self, (a,b) => b)
        for (i <- 0 until $self.length) {
          out(i) = $self(i)
        }        
        out.unsafeImmutable
      }
      
      fimplicit ("chainViewToDenseOps") (Nil :: ephemeralTpe("DenseVectorOpsCls[T]", stage = now)) implements composite ${
        DenseVectorRepToDenseVectorOpsCls(viewToDense($self))
      }
            
      compiler ("densevectorview_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("DenseVectorViews cannot be allocated from a parallel op") }
      compiler ("densevectorview_illegalupdate") ((MInt, T) :: MNothing, effect = simple) implements composite ${ fatal("DenseVectorViews cannot be updated") }
      
      parallelize as ParallelCollection(T, lookupOp("densevectorview_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("densevectorview_illegalupdate"))            
    }
    
    // allows us to perform a number of simple accessor operations without converting to a DenseVector first
    addVectorCommonOps(DenseVectorView,T)
  }
}
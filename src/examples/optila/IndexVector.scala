package ppl.dsl.forge
package examples
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait IndexVectorOps {
  this: OptiLADSL => 
 
  def importIndexVectorOps() {
    val IndexVector = tpe("IndexVector") 
    val DenseVector = lookupTpe("DenseVector")
  
    // data fields     
    data(IndexVector, ("_start", MInt), ("_end", MInt), ("_isRow", MBoolean))      
  
    // static methods
    static (IndexVector) ("apply", Nil, MethodSignature(List(MInt,MInt,("isRow",MBoolean,"true")), IndexVector)) implements allocates(IndexVector, quotedArg(0), quotedArg(1), quotedArg(2))
        
    val IndexVectorOps = withTpe(IndexVector)
    IndexVectorOps {
      compiler ("indexvector_start") (Nil :: MInt) implements getter(0, "_start")
      compiler ("indexvector_end") (Nil :: MInt) implements getter(0, "_end")
      
      infix ("length") (Nil :: MInt) implements composite ${ indexvector_end($self) - indexvector_start($self) }
      infix ("isRow") (Nil :: MBoolean) implements getter(0, "_isRow")
      infix ("apply") (MInt :: MInt) implements composite ${ indexvector_start($self) + $1 }      
      infix ("toDense") (Nil :: DenseVector(MInt)) implements composite ${ indexToDense($self) }
            
      // should be parallel, so the conversion can fuse with the consumer
      // is this fast and robust enough to capture parallel operators over index vectors?
      fimplicit ("indexToDense") (Nil :: DenseVector(MInt)) implements composite ${
        val out = DenseVector[Int]($self.length, $self.isRow)
                
        // TODO: vector zip currently requires a DenseVector arg
        // out.zip[Int,Int]($self, (e,i) => $self(i))
        for (i <- 0 until $self.length) {
          out(i) = $self(i)
        }
        out.unsafeImmutable
      }
      
      // naming is by convention here, a little brittle. would it be better to put this in extern?
      fimplicit ("chainIndexToDenseOps") (Nil :: ephemeralTpe("DenseVectorOpsCls[Int]", stage = now)) implements composite ${
        DenseVectorRepToDenseVectorOpsCls(indexToDense($self))
      }      
      fimplicit ("chainIndexToDenseIntOps") (Nil :: ephemeralTpe("DenseVectorIntOpsCls", stage = now)) implements composite ${
        DenseVectorRepToDenseVectorIntOpsCls(indexToDense($self))
      }
            
      // TODO: need indexed loop pattern to do this efficiently (without allocating a dense IndexVector)
      // infix ("foreach") ((MInt ==> MUnit) :: MUnit, effect = simple) implements foreach(MInt, 0, ${ e => $1(e) })
      
      compiler ("indexvector_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("IndexVectors cannot be allocated from a parallel op") }
      compiler ("indexvector_illegalupdate") ((MInt, MInt) :: MNothing, effect = simple) implements composite ${ fatal("IndexVectors cannot be updated") }
      
      // IndexVectors can't be mapped over, but they can be zipped with or reduced
      parallelize as ParallelCollection(MInt, lookupOp("indexvector_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("indexvector_illegalupdate"))            
    }
    
    // allows us to perform a number of simple accessor operations without converting to a DenseVector first
    addVectorCommonOps(IndexVector,MInt)
  }
}
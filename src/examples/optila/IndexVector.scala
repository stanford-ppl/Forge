package ppl.dsl.forge
package examples
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait IndexVectorOps {
  this: OptiLADSL => 
 
  def importIndexVectorOps() {
    val IndexVector = lookupTpe("IndexVector") 
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
      infix ("t") (Nil :: IndexVector) implements allocates(IndexVector, ${indexvector_start($self)}, ${indexvector_end($self)}, ${!(indexvector_isrow($self))})
      infix ("toDense") (Nil :: DenseVector(MInt)) implements composite ${ $self.map(e => e) }
            
      // parallel, so the conversion can fuse with the consumer
      // is this fast and robust enough to capture parallel operators over index vectors?
      fimplicit ("indexToDense") (Nil :: DenseVector(MInt)) implements composite ${ 
        Console.println("(performance warning): automatic conversion from IndexVector to DenseVector")
        $self.toDense 
      }
      
      // naming is by convention here, a little brittle. would it be better to put this in extern?
      val grpName = if (Config.fastCompile) "$Flat" else "DenseVector"
      fimplicit ("chainIndexToDenseOps") (Nil :: ephemeralTpe(grpName+"DenseVectorOpsCls[Int]", stage = now)) implements composite ${
        repTo\${grpName}DenseVectorOpsCls(indexToDense($self))
      }      
      fimplicit ("chainIndexToDenseIntOps") (Nil :: ephemeralTpe(grpName+"DenseVectorIntOpsCls", stage = now)) implements composite ${
        repTo\${grpName}DenseVectorIntOpsCls(indexToDense($self))
      }
            
      compiler ("indexvector_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("IndexVectors cannot be allocated from a parallel op") }      
      compiler ("indexvector_illegalupdate") ((MInt, MInt) :: MNothing, effect = simple) implements composite ${ fatal("IndexVectors cannot be updated") }
      
      // IndexVectors can't be mapped over, but they can be zipped with or reduced
      parallelize as ParallelCollection(MInt, lookupOp("indexvector_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("indexvector_illegalupdate"))            
    }
    
    // allows us to perform operations without converting to a DenseVector first
    addVectorCommonOps(IndexVector,MInt)
  }
}
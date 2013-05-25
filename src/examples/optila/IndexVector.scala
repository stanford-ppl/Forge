package ppl.dsl.forge
package examples
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait IndexVectorOps {
  this: OptiLADSL => 
 
  def importIndexVectorOps() {
    val IndexVector = tpe("IndexVector") 
    val DenseVector = lookupTpe("DenseVector").get
  
    // data fields     
    data(IndexVector, ("_start", MInt), ("_end", MInt), ("_isRow", MBoolean))      
  
    // static methods
    static (IndexVector) ("apply", Nil, MethodSignature(List(MInt,MInt,("isRow",MBoolean,"true")), IndexVector)) implements allocates(IndexVector, quotedArg(0), quotedArg(1), quotedArg(2))
        
    val IndexVectorOps = withTpe(IndexVector)
    IndexVectorOps {
      compiler ("indexvector_start") (Nil :: MInt) implements getter(0, "_start")
      compiler ("indexvector_end") (Nil :: MInt) implements getter(0, "_end")
      
      infix ("length") (Nil :: MInt) implements composite ${ indexvector_start($self) - indexvector_end($self) }
      infix ("isRow") (Nil :: MBoolean) implements getter(0, "_isRow")
      infix ("apply") (MInt :: MInt) implements composite ${ indexvector_start($self) + $1 }                        
      
      // other accessors available on DenseVector (first, last, etc.) could be factored out and imported here
      // as described in OptiLA.scala
      
      // parallel, so the conversion can fuse with the consumer
      // is this fast and robust enough to capture parallel operators over index vectors?
      fimplicit ("toDense") (Nil :: DenseVector(MInt)) implements composite ${
        val out = DenseVector[Int]($self.length, $self.isRow)
        out.zip[Int,Int]($self, (e,i) => i)
      }
      
      // naming is by convention here, a little brittle. would it be better to put this in extern?
      fimplicit ("chainToDenseOps") (Nil :: ephemeralTpe("DenseVectorOpsCls[Int]", stage = now)) implements composite ${
        repToDenseVectorOps(toDense($self))
      }
      
      // TODO: need indexed loop pattern to do this efficiently (without allocating a dense IndexVector)
      // infix ("foreach") ((MInt ==> MUnit) :: MUnit, effect = simple) implements foreach(MInt, 0, ${ e => $1(e) })
      
      compiler ("indexvector_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("IndexVectors cannot be allocated from a parallel op") }
      compiler ("indexvector_illegalupdate") ((MInt, MInt) :: MNothing, effect = simple) implements composite ${ fatal("IndexVectors cannot be updated") }
      
      // IndexVectors can't be mapped over, but they can be zipped with
      parallelize as ParallelCollection(MInt, lookupOp("indexvector_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",0), lookupOp("indexvector_illegalupdate"))            
    }
  }
}
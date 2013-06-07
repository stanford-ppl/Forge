package ppl.dsl.forge
package examples
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

trait VecMatConstructor {
  this: OptiMLDSL => 
 
  def importVecMatConstructor() {
    val DenseVector = lookupTpe("DenseVector")
    val IndexVector = lookupTpe("IndexVector")
    val T = tpePar("T")
    
    // vector constructor (0 :: end) { ... }
    infix (IndexVector) ("::", Nil, ((("end", MInt), ("start", MInt)) :: IndexVector)) implements composite ${ IndexVector($start, $end) }    
    
    // should add apply directly to IndexVector, otherwise we have issues with ambiguous implicit conversions
    infix (IndexVector) ("apply", T, (IndexVector, MInt ==> T)  :: DenseVector(T)) implements composite ${ $0.map($1) }   
  }
}
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
    
    // TODO: doesn't curry properly. need to surpress the source context of the "::" operator?
    // we need to support explicit currying anyways. does that replace the need for this? (i don't think so, because we need both to work)

    // should add apply directly to IndexVector, otherwise we have issues with ambiguous implicit conversions
    infix (IndexVector) ("apply", T, (IndexVector, MInt ==> T)  :: DenseVector(T)) implements composite ${ $0.map($1) }   
  }
}
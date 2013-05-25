package ppl.dsl.forge
package examples
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

object OptiLADSLRunner extends ForgeApplicationRunner with OptiLADSL       

trait OptiLADSL extends ForgeApplication 
  with BasicMathOps with RandomOps with ArithOps with DenseVectorOps with IndexVectorOps {
    
  def dslName = "OptiLA"
  
  def specification() = {
    // our selection of Scala ops
    // we don't use Numeric or Fractional, since they are replaced by Arith
    importPrimitives()
    importMisc()
    importCasts()
    importOrdering()
    importStrings()
    importMath()
    importTuples()
    
    // Code generation can be an alternative to subtyping for achieving code re-use:
    /*
    def VectorImmutableInterface(tpe: Rep[DSLType]) = {
      val VectorImmOps = withTpe(tpe)
      // requirement: _data has a superset of the fields of Vector. can we check?
      VectorImmOps {
        "reduce" is ..
        "map" is ..
      }
    }
    addVectorImmutableOps(DenseVector)
    addVectorImmutableOps(IndexVector)
    */    
    
    // OptiLA ops
    // note that the order matters with respect to 'lookup' calls
    
    // sneak in a compiler-only range method
    val Range = tpe("Range")
    data(Range, ("start", MInt), ("end", MInt))
    compiler (Range) ("range_start", Nil, Range :: MInt) implements getter(0, "start")
    compiler (Range) ("range_end", Nil, Range :: MInt) implements getter(0, "end")
    
    noInfixList :::= List("infix_foreach")     
    compiler (Range) ("infix_until", Nil, (MInt,MInt) :: Range) implements allocates(Range, quotedArg(0), quotedArg(1))
    
    // this is a little convoluted unfortunately (because of the restriction on passing structs to codegen nodes)
    compiler (Range) ("infix_foreach", Nil, (Range, MInt ==> MUnit) :: MUnit, effect = simple) implements composite ${ range_foreach(range_start($0), range_end($0), $1) }    
    compiler (Range) ("range_foreach", Nil, (("start",MInt),("end",MInt),("func",MInt ==> MUnit)) :: MUnit, effect = simple) implements codegen($cala, ${
      var i = $start
      while (i < $end) {
        $b[func](i)
        i += 1
      }
    })
    
    importBasicMathOps()
    importRandomOps() 
    importArithOps()
    
    // needed by both DenseVector and IndexVector (need a better solution w.r.t. lookup, as this is awkward)
    // one option is to declare all tpes first, and only use them in the respective ops (similar to Delite)
    val DenseVector = tpe("DenseVector", tpePar("T")) 
    
    importIndexVectorOps()
    importDenseVectorOps()           
  }  
}
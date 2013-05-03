package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object HUMAN_DSL_NAMEDSLRunner extends ForgeApplicationRunner with HUMAN_DSL_NAMEDSL

trait HUMAN_DSL_NAMEDSL extends ForgeApplication with ScalaOps {
  /**
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  def dslName = "HUMAN_DSL_NAME"
    
  /**
   * The specification is the DSL definition (types, data structures, ops, code generators)
   */
  def specification() = {
    /**
     * Include Scala ops
     */
     addScalaOps()
        
    /**
     * Types
     */
    val T = tpePar("T")
    val ParType = tpe("ParType", List(T))

    /**
     * Data structures
     */
    data(ParType, List(T), ("_length", MInt), ("_data", MArray(T)))
                
    /**
     * Ops
     */                   
    val vnew = op (ParType) ("apply", static, List(T), List(MInt), ParType, codegenerated, effect = mutable)
    val vlength = op (ParType) ("length", infix, List(T), List(ParType), MInt, codegenerated)    
    val vapply = op (ParType) ("apply", infix, List(T), List(ParType,MInt), T, codegenerated)
    val vupdate = op (ParType) ("update", infix, List(T), List(ParType,MInt,T), MUnit, codegenerated, effect = write(0))
    
    /**
     * ParallelCollectionification
     * This enables a tpe to be passed in as the collection type of a Delite op
     */
    ParType is ParallelCollection(T, vnew, vlength, vapply, vupdate)
    
    /**
     * Code generators
     */      
    codegen (vnew) ($cala, "new "+vnew.tpeName+"["+vnew.tpeInstance(0)+"]("+quotedArg(0)+", new Array["+vnew.tpeInstance(0)+"]("+quotedArg(0)+"))")
    codegen (vlength) ($cala, quotedArg(0) + "._length")
    codegen (vapply) ($cala, quotedArg(0) + "._data.apply(" + quotedArg(1) + ")")
    codegen (vupdate) ($cala, quotedArg(0) + "._data.update(" + quotedArg(1) + ", " + quotedArg(2) + ")")    
    ()
  }
}
 

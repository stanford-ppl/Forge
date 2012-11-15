package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object SimpleVectorDSLRunner extends ForgeApplicationRunner with SimpleVectorDSL

trait SimpleVectorDSL extends ForgeApplication with ScalaOps {
  /**
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  def dslName = "SimpleVector"
    
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
    val Vector = tpe("Vector", List(T)) 
    
    /**
     * Data structures
     */
    data(Vector, List(T), ("_length", MInt), ("_data", MArray(T)))
        
    /**
     * Ops
     * 
     * We could simplify this by reusing templates even more, i.e. specializing for different types
     * (e.g. accept a list of binary zip ops that only differentiate in function applied)
     */           
    val vnew = op (Vector) ("apply", static, List(T), List(MInt), Vector, codegenerated, effect = mutable)
    val vlength = op (Vector) ("length", infix, List(T), List(Vector), MInt, codegenerated)    
    val vapply = op (Vector) ("apply", infix, List(T), List(Vector,MInt), T, codegenerated)
    val vupdate = op (Vector) ("update", infix, List(T), List(Vector,MInt,T), MUnit, codegenerated, effect = write(0))
    val vplus = op (Vector) ("+", infix, List(T withBound TNumeric), List(Vector,Vector), Vector, zip((T,T,T,Vector), (0,1), "(a,b) => a+b"))
        
    /**
     * DeliteCollectionification
     */
    Vector is DeliteCollection(T, vnew, vlength, vapply, vupdate)
    
    /**
     * Code generators
     */
    codegen (vnew) ($cala, "new "+vnew.tpeName+"["+vnew.tpeInstance(0)+"]("+vnew.quotedArg(0)+", new Array["+vnew.tpeInstance(0)+"]("+vnew.quotedArg(0)+"))")
    codegen (vlength) ($cala, vapply.quotedArg(0) + "._length")
    codegen (vapply) ($cala, vapply.quotedArg(0) + "._data.apply(" + vapply.quotedArg(1) + ")")
    codegen (vupdate) ($cala, vupdate.quotedArg(0) + "._data.update(" + vupdate.quotedArg(1) + ", " + vupdate.quotedArg(2) + ")")
        
    ()
  }
}
 
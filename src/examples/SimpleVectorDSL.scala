package ppl.dsl.meta
package examples

import core.{MetaDSLApplication,MetaDSLApplicationRunner}

object SimpleVectorDSLRunner extends MetaDSLApplicationRunner with SimpleVectorDSL

trait SimpleVectorDSL extends MetaDSLApplication {
  /**
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  def dslName = "SimpleVector"
  
  /**
   * The LMS common ops that you want made available inside the application
   */
  override def lmsAppOps = List(equalOps, ifThenElseOps, variableOps, whileOps, functionOps, implicitOps, 
                                numericOps, orderingOps, stringOps, booleanOps, primitiveOps, miscOps,
                                tupleOps, mathOps, castingOps, objectOps, arrayOps)
  
  /**
   * The LMS common ops that you want made available inside the generated compiler, 
   * in addition to LMSAppOps
   */
  override def lmsCompOps = List(rangeOps)
  
  /**
   * The specification is the DSL definition (types, data structures, ops, code generators)
   */
  def specification() = {
    /**
     * Types
     */
    val T = tpeArg("T")
    val Vector = tpe("Vector", List(T)) 
    
    /**
     * Data structures
     */
    data(Vector, List(T), ("_length", MInt), ("_data", MArray(List(T))))
        
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
    codegen (vnew) ($cala, quote("new "+vnew.tpeName+"["+vnew.tpeInstance(0)+"]("+vnew.quotedArg(0)+", new Array["+vnew.tpeInstance(0)+"]("+vnew.quotedArg(0)+"))"))
    codegen (vlength) ($cala, quote(vapply.quotedArg(0) + "._length"))
    codegen (vapply) ($cala, quote(vapply.quotedArg(0) + "._data.apply(" + vapply.quotedArg(1) + ")"))
    codegen (vupdate) ($cala, quote(vupdate.quotedArg(0) + "._data.update(" + vupdate.quotedArg(1) + ", " + vupdate.quotedArg(2) + ")"))
        
    ()
  }
}
 
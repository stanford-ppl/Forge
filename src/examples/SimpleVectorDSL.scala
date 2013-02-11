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
    
    // TODO: define tpeAliases
    // tpeAlias(Vector, "V")
    
    /**
     * Data structures
     */
    data(Vector, List(T), ("_length", MInt), ("_data", GArray(T)))
    
    /* Generic formatting instance */
    val stream = ForgePrinter()
        
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
    
    val vtimesScalar = op (Vector) ("*", infix, List(T withBound TNumeric), List(Vector,T), Vector, map((T,T,Vector), 0, "e => e*"+quotedArg(1)))
    // val vfoo = op (Vector) ("foo", infix, List(T), List(Vector,T), tpeInst(Vector,List(MDouble)), map((T,MDouble,Vector), 0, "e => 0.0")) // problem: primitive lifting isn't in scope in the ops
      
    val vplus = op (Vector) ("+", infix, List(T withBound TNumeric), List(Vector,Vector), Vector, zip((T,T,T,Vector), (0,1), "(a,b) => a+b"))
    
    val vsum = op (Vector) ("sum", infix, List(T withBound TNumeric), List(Vector), T, reduce((T,Vector), 0, lookup("Numeric","zero").get, "(a,b) => a+b"))
    
    val vprint = op (Vector) ("pprint", infix, List(T), List(Vector), MUnit, foreach((T,Vector), 0, "a => println(a)")) // will print out of order in parallel, but hey
     
    // val vfilter = op (Vector) ("filter", infix, List(T), List(Vector,MFunction(List(T), MBoolean)), Vector, filter((T,T,Vector), 0, "e => " + quotedArg(1) + "(e)", "e => e"))
    
    val vslice = op (Vector) ("slice", infix, List(T), List(Vector, MInt, MInt), Vector, single(Vector, { 
      // inside single tasks we use normal DSL code just like applications would (modulo arg names)
      stream.printLines(
        "val st = " + quotedArg(1),
        "val en = " + quotedArg(2),
        "val out = Vector[T](en - st)",
        "var i = st",        
        "while (i < en) {",
        "  out(i-st) = "+quotedArg(0)+"(i)",
        "  i += 1",
        "}",
        "out"
      )}))        
              
    /**
     * DeliteCollectionification
     * This enables a tpe to be passed in as the collection type of a Delite op
     */
    Vector is DeliteCollection(T, vnew, vlength, vapply, vupdate)
    
    /**
     * Code generators
     */
      
    // TODO: how do we refer to other methods or codegenerators inside a particular codegen impl? e.g. vfoo uses vlength   
    codegen (vnew) ($cala, "new "+vnew.tpeName+"["+vnew.tpeInstance(0)+"]("+quotedArg(0)+", new Array["+vnew.tpeInstance(0)+"]("+quotedArg(0)+"))")
    codegen (vlength) ($cala, quotedArg(0) + "._length")
    codegen (vapply) ($cala, quotedArg(0) + "._data.apply(" + quotedArg(1) + ")")
    codegen (vupdate) ($cala, quotedArg(0) + "._data.update(" + quotedArg(1) + ", " + quotedArg(2) + ")")    
    ()
  }
}
 
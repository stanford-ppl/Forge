package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

// This object lets us build our DSL
object HUMAN_DSL_NAMEDSLRunner extends ForgeApplicationRunner with HUMAN_DSL_NAMEDSL

trait HUMAN_DSL_NAMEDSL extends ForgeApplication {
  /**
   * The name of our DSL. This is the name that will be used in generated files,
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
    importScalaOps()
        
    /**
     * The main portion of our DSL
     */
    importHUMAN_DSL_NAMEOps()
  }
    
  def importHUMAN_DSL_NAMEOps() {

    /**
     * Types
     * - T is a generic Type
     * - ParType is an object we are creating, and will define to be parallel
     */
    val T = tpePar("T") 
    val ParType = tpe("ParType", T) 

    /**
     * Data structures
     */
    data(ParType, ("_length", MInt), ("_data", MArray(T)))
  
    /**
     * Static Allocation - 
     * The `::` separates arguments and return type
     * $0 indexes into the argument list
     * Because we are allocating a new Vector, $0 is the first element in the list
     * array_empty is defined in static/extern/shared/src/Arrays.scala
     *
     * We will define two versions - one with just the length
     * And another which takes a Vector instance - ie a copy constructor
     */
    static (ParType) ("apply", T, MInt :: ParType(T), effect = mutable) implements
    allocates (ParType, ${$0}, ${ array_empty[T]($0) })

    /**
     *  Because we have multiple arguments, we group them in a Tuple
     */ 
    compiler (ParType) ("apply", T, (ParType, MInt) :: ParType(T), effect = mutable) implements
    allocates (ParType, ${$1}, ${ array_empty[T]($1) })

    // A refence to our operations, necessary for scala compiler reasons
    val ParTypeOps = withTpe (ParType)

    ParTypeOps {
      /**
       * Ops :
       * (1) Scope
       *  - compiler  : only available to the DSL author
       *  - direct    : can be called as `name(arg0, arg1)`
       *  - infix     : can be called as `myParType.name(arg0, arg1) or name(myParType, arg0, arg1)`
       *  - static    : can be called as `ParType(arg0, arg1)`
       * (2) Implementation
       *  - getter    : return a field from data
       *  - setter    : assign a data field (if mutable)
       *  - single    : a DSL code block that can be run in serial
       *  - composite : a DSL code block that includes other DSL methods
       *  - map, zip  : example parallel operators, must be a ParallelCollection
       *  - codegen   : code implemented in a target language
       */                   

      /**
       *  Their is an implicit 0th argument of type ParType, which we refence as 0 
       *  and return the _length from that object
       *  Nil means the method takes no arguments
       */
      infix ("length") (Nil :: MInt) implements getter(0, "_length")    
      
      /**
       *  This compiler method is only available to the DSL author
       */
      compiler ("raw_data") (Nil :: MArray(T)) implements getter(0, "_data")

      /**
       *  This composite method calls our getter and array_apply. 
       *  $self accesses the implcit 0th element ($0 works too). 
       *  $1 access the MInt argument
       */
      infix ("apply") (MInt :: T) implements composite ${ array_apply(raw_data($self), $1) }
    
      /**
       *  We give names to our arguments, and can access index as $index or $1
       *  Update writes to the calling object, so we add that effect
       */
      infix ("update") ((("index", MInt), ("elem", T)) :: MUnit, effect = write(0)) implements
      composite ${ array_update(raw_data($self), $index, $elem) }

      /**
       * ParallelCollectionification
       * This enables a tpe to be passed in as the collection type of a Delite op
       * we use lookupOp to get references to the methods we defined
       * If an op is overloaded, we access them in declaration order
       */
      parallelize as ParallelCollection(T, lookupOverloaded("apply", 1), 
        lookupOp("length"), lookupOverloaded("apply", 2), lookupOp("update"))

      /**
       *  We can now use the parallel operators map, reduce, foreach, and zipWith
       *  If we wanted to use filter, we would need a ParallelCollectionBuffer - try it yourself!
       *
       *  Check out the example application to see how a user calls this function
       *  We add the constraint that only numerics can be added
       *  zip requires three type parameteres
       *   - The type of the first list
       *   - The type of the second list
       *   - The type of the resulting list
       *  We then specify the structures to be zipped - the calling argument and the first parameter
       *  Finally, we include the function to be applied during the zipping
       */
      infix ("+") (ParType(T) :: ParType(T), TNumeric(T)) implements zip((T, T, T), (0, 1), ${
        (a,b) => a + b
      })
    }

    // Return a Unit from the specification function
    ()
  }
}
 

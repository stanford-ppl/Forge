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
    // val IntVector = tpeInst(Vector, List(MInt)) 
       
    tpeAlias("V", Vector)
    tpeAlias("VI", tpeInst(Vector, List(MInt)))    
    
    /**
     * Data structures
     */    
    data(Vector, ("_length", MInt), ("_data", MArray(T)))
    
    /* Generic formatter */
    val stream = ForgePrinter()
        
    /**
     * Ops
     */               
    
    /* Proof of concept / experimental syntatic sugar */
    // doesn't rewrite correctly if we use "withTpe (Vector) {", but works if we use:
    val VectorOps = withTpe (Vector)
    VectorOps {
      "sum" is (infix, (T withBound TNumeric), Vector, T) implements {
        reduce((T,Vector), 0, lookup("Numeric","zero").get, "(a,b) => a+b")
      }

      "pprint" is (infix, T, Vector, MUnit, effect = simple) implements {
        foreach((T,Vector), 0, "a => println(a)") // will print out of order in parallel, but hey
      }
      
      "slice" is (infix, T, (Vector, MInt, MInt), Vector) implements 
        single { 
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
          )
        }
       
      "map" is (infix, T, (Vector, T ==> T), Vector) implements {
         map((T,T), 0, "e => "+quotedArg(1)+"(e)")
      }
    }
    /* -- */
    
    val vnew = op (Vector) ("apply", static, List(T), List(MInt), Vector, effect = mutable)
    impl (vnew) (allocates(Vector, quotedArg(0), "array_empty[T]("+quotedArg(0)+")"))
    
    val vapply = op (Vector) ("apply", infix, List(T), List(Vector,MInt), T)
    impl (vapply) (composite {
      "array_apply(vector_raw_data("+quotedArg(0)+"),"+quotedArg(1)+")"
    })
    
    val vupdate = op (Vector) ("update", infix, List(T), List(Vector,MInt,T), MUnit, effect = write(0))
    impl (vupdate) (composite {
      "array_update(vector_raw_data("+quotedArg(0)+"),"+quotedArg(1)+","+quotedArg(2)+")"
    })
    
    op (Vector) ("*", infix, (T withBound TNumeric), (Vector,T), Vector) implements {
      map((T,T), 0, "e => e*"+quotedArg(1))
    }
    
    val vfoo = op (Vector) ("foo", infix, List(T), List(Vector,T), tpeInst(Vector,List(MDouble)))
    impl (vfoo) (map((T,MDouble), 0, "e => unit(0.0)")) // problem: primitive lifting isn't in scope in the ops
     
    // uses a function arg inside a delite op
    val vbar = op (Vector) ("bar", infix, List(T), List(Vector,("f", MFunction(List(("x",T)),T))), Vector)
    impl (vbar) (map((T,T), 0, "e => "+quotedArg("f")+"(e)")) 

    // generic map, reduce functions for vector
    // val vmap = op (Vector) ("map", infix, List(T), List(Vector,T ==> T), Vector)
    // impl (vmap) (map((T,T), 0, "e => "+quotedArg(1)+"(e)"))
        
    val vreduce = op (Vector) ("reduce", infix, List(T withBound TNumeric), List(Vector,(T,T) ==> T), T)
    impl (vreduce) (reduce((T,Vector), 0, lookup("Numeric","zero").get, "(a,b) => "+quotedArg(1)+"(a,b)"))
  
    val vfilter = op (Vector) ("filter", infix, List(T), List(Vector,T ==> MBoolean), Vector)
    impl (vfilter) (filter((T,T), 0, "e => " + quotedArg(1) + "(e)", "e => e"))
    
    // a composite op
    val vmapreduce = op (Vector) ("mapreduce", infix, List(T withBound TNumeric), List(Vector,T ==> T,(T,T) ==> T), T)
    impl (vmapreduce) (composite { quotedArg(0)+".map("+quotedArg(1)+").reduce("+quotedArg(2)+")" })
    
    val vbasic = op (Vector) ("basic", infix, List(), List(MInt, ("y", MInt, "1"), ("z", MInt, "1")), MInt)
    impl (vbasic) (codegen($cala, quotedArg("y")+ "+3+" + quotedArg("z")))
 
    val vset = op (Vector) ("set", infix, List(T withBound TNumeric), List(Vector, ("x", MInt), ("z", MInt, "3")), tpeInst(Vector, List(MInt)))
    impl (vset) (map ((T,MInt), 0, "y  => "+quotedArg("x") + " + " + quotedArg("z")))

    val vplus = op (Vector) ("+", infix, List(T withBound TNumeric), List(Vector,Vector), Vector)
    impl (vplus) (zip((T,T,T), (0,1), "(a,b) => a+b"))
    
    // val vsum = op (Vector) ("sum", infix, List(T withBound TNumeric), List(Vector), T)
    // impl (vsum) (reduce((T,Vector), 0, lookup("Numeric","zero").get, "(a,b) => a+b"))
    // 
    // val vprint = op (Vector) ("pprint", infix, List(T), List(Vector), MUnit, effect = simple)
    // impl (vprint) (foreach((T,Vector), 0, "a => println(a)")) // will print out of order in parallel, but hey
    //      
    // val vslice = op (Vector) ("slice", infix, List(T), List(Vector, MInt, MInt), Vector)
    // impl (vslice) (single { 
    //   // inside single tasks we use normal DSL code just like applications would (modulo arg names)
    //   stream.printLines(
    //     "val st = " + quotedArg(1),
    //     "val en = " + quotedArg(2),
    //     "val out = Vector[T](en - st)",
    //     "var i = st",        
    //     "while (i < en) {",
    //     "  out(i-st) = "+quotedArg(0)+"(i)",
    //     "  i += 1",
    //     "}",
    //     "out"
    //   )})        
                     
    // -- getters and setters
  
    val vrawdata = op (Vector) ("vector_raw_data", compiler, List(T), List(Vector), MArray(T))
    impl (vrawdata) (getter(0, "_data"))
    val vsetrawdata = op (Vector) ("vector_set_raw_data", compiler, List(T), List(Vector, MArray(T)), MUnit, effect = write(0))
    impl (vsetrawdata) (setter(0, "_data", quotedArg(1)))
    val vlength = op (Vector) ("length", infix, List(T), List(Vector), MInt)
    impl (vlength) (getter(0, "_length"))    
    val vsetsize = op (Vector) ("vector_set_size", compiler, List(T), List(Vector,MInt), MUnit, effect = write(0))
    impl (vsetsize) (setter(0, "_length", quotedArg(1)))
    
    // -- data manipulation
    
    val vinsert = op (Vector) ("insert", infix, List(T), List(Vector,MInt,T), MUnit, effect = write(0))
    impl (vinsert) (single {
      stream.printLines(
        "vector_insertspace("+quotedArg(0)+","+quotedArg(1)+",1)",
        quotedArg(0)+"("+quotedArg(1)+") = " + quotedArg(2)
    )})

    val vappend = op (Vector) ("append", infix, List(T), List(Vector,MInt,T), MUnit, effect = write(0))
    impl (vappend) (single {
      quotedArg(0)+".insert("+quotedArg(0)+".length, "+quotedArg(2)+")"
    })
        
    val vinsertspace = op (Vector) ("vector_insertspace", compiler, List(T), List(Vector,MInt,MInt), MUnit, effect = write(0))
    impl (vinsertspace) (single {
      val v = quotedArg(0)
      val pos = quotedArg(1)
      val len = quotedArg(2)
      // can clean this up using string interpolation.. but how does that interact with stream.printLines? can we get rid of stream.printLines?
      stream.printLines(        
        "vector_ensureextra("+v+","+len+")",
        "val data = vector_raw_data("+v+")",
        "array_copy(data, "+pos+", data, "+pos+" + "+len+", "+v+".length - "+pos+")",
        "vector_set_size("+v+", "+v+".length + "+len+")"
    )})
    
    val vensureextra = op (Vector) ("vector_ensureextra", compiler, List(T), List(Vector,MInt), MUnit, effect = write(0))
    impl (vensureextra) (single {
      val v = quotedArg(0)
      val extra = quotedArg(1)
      stream.printLines(        
        "val data = vector_raw_data("+v+")",
        "if (array_length(data) - "+v+".length < "+extra+") {",
        "  vector_realloc("+v+", "+v+".length + "+extra+")",
        "}"
    )})
    
    val vrealloc = op (Vector) ("vector_realloc", compiler, List(T), List(Vector,MInt), MUnit, effect = write(0))
    impl (vrealloc) (single {
      val v = quotedArg(0)
      val minLen = quotedArg(1)
      stream.printLines(        
        "val data = vector_raw_data("+v+")",
        "var n = Math.max(4, array_length(data) * 2)",
        "while (n < "+minLen+") n = n*2",
        "val d = array_empty[T](n)",
        "array_copy(data, 0, d, 0, "+v+".length)",        
        "vector_set_raw_data("+v+", array_asimmutable(d))"    
    )})        
            
                  
    /**
     * ParallelCollectionification
     * This enables a tpe to be passed in as the collection type of a Delite op
     */
    
    val vappendable = op (Vector) ("vector_appendable", compiler, List(T), List(Vector,MInt,T), MBoolean)
    impl (vappendable) (single("true"))

    val vcopy = op (Vector) ("vector_copy", compiler, List(T), List(Vector,MInt,Vector,MInt,MInt), MUnit, effect = write(2))
    impl (vcopy) (single {
      val src = "vector_raw_data(" + quotedArg(0) + ")"
      val dest = "vector_raw_data(" + quotedArg(2) + ")"
      "array_copy("+src+","+quotedArg(1)+","+dest+","+quotedArg(3)+","+quotedArg(4)+")"
    })
    
    Vector is ParallelCollectionBuffer(T, vnew, vlength, vapply, vupdate, vsetsize, vappendable, vappend, vcopy)
        
    ()    
  }
}
 

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
  
  /* Generic code formatter */
  lazy val stream = ForgePrinter()
    
  /**
   * The specification is the DSL definition (types, data structures, ops)
   */
  def specification() = {
    /**
     * Include Scala ops
     */
    addScalaOps() 
    
    /**
     * The main portion of our DSL
     */        
    addVectorOps()
  }
  
  
  def addVectorOps() {            
    // generic type parameters we will use 
    val T = tpePar("T") 
    val R = tpePar("R")
    
    val Vector = tpe("Vector", T) 
  
    // doesn't rewrite correctly if we use "withTpe (Vector) {", but works if we use:
    val VectorOps = withTpe (Vector)
          
    VectorOps {                  
      // data fields     
      data(("_length", MInt), ("_data", MArray(T)))


      // allocation
      op (Vector) ("apply", static, T, (MInt), Vector(T), effect = mutable) implements allocates(Vector, quotedArg(0), "array_empty[T]("+quotedArg(0)+")")
      
            
      // getters and setters
      "vector_raw_data" is (compiler, Nil, MArray(T)) implements getter(0, "_data")
      "vector_set_raw_data" is (compiler, (MArray(T)), MUnit, effect = write(0)) implements setter(0, "_data", quotedArg(1))
      "length" is (infix, Nil, MInt) implements getter(0, "_length")
      "vector_set_length" is (compiler, (MInt), MUnit, effect = write(0)) implements setter(0, "_length", quotedArg(1))      
      
      
      // data ops             
      "apply" is (infix, (MInt), T) implements composite {
        "array_apply(vector_raw_data("+quotedArg("self")+"),"+quotedArg(1)+")"
      }      
      // example named arg
      "update" is (infix, (("i",MInt),("e",T)), MUnit, effect = write(0)) implements composite {
        "array_update(vector_raw_data("+quotedArg("self")+"),"+quotedArg("i")+","+quotedArg("e")+")"
      }
      // example named, default arg. List is currently explicitly needed when mixing arg types.
      "slice" is (infix, List(("start",MInt,"0"),("end",MInt)), Vector(T)) implements single {
        // inside single tasks we use normal DSL code just like applications would (modulo arg names)
        stream.printLines(
          "val st = " + quotedArg("start"),
          "val en = " + quotedArg("end"),
          "val out = Vector[T](en - st)",
          "var i = st",        
          "while (i < en) {",
          "  out(i-st) = "+quotedArg("self")+"(i)",
          "  i += 1",
          "}",
          "out"
        )
      }      
      "insert" is (infix, (MInt,T), MUnit, effect = write(0)) implements single {
        stream.printLines(
          "vector_insertspace("+quotedArg("self")+","+quotedArg(1)+",1)",
          quotedArg("self")+"("+quotedArg(1)+") = " + quotedArg(2)
        )
      }
      "append" is (infix, (MInt,T), MUnit, effect = write(0)) implements single {
        quotedArg("self")+".insert("+quotedArg("self")+".length, "+quotedArg(2)+")"
      }        
      "vector_insertspace" is (compiler, (MInt,MInt), MUnit, effect = write(0)) implements single {
        val v = quotedArg("self")
        val pos = quotedArg(1)
        val len = quotedArg(2)
        // can clean this up using string interpolation.. but how does that interact with stream.printLines? can we get rid of stream.printLines?
        stream.printLines(        
          "vector_ensureextra("+v+","+len+")",
          "val data = vector_raw_data("+v+")",
          "array_copy(data, "+pos+", data, "+pos+" + "+len+", "+v+".length - "+pos+")",
          "vector_set_length("+v+", "+v+".length + "+len+")"
        )
      }
      "vector_ensureextra" is (compiler, (MInt), MUnit, effect = write(0)) implements single {
        val v = quotedArg("self")
        val extra = quotedArg(1)
        stream.printLines(        
          "val data = vector_raw_data("+v+")",
          "if (array_length(data) - "+v+".length < "+extra+") {",
          "  vector_realloc("+v+", "+v+".length + "+extra+")",
          "}"
        )
      }
      "vector_realloc" is (compiler, (MInt), MUnit, effect = write(0)) implements single {
        val v = quotedArg("self")
        val minLen = quotedArg(1)
        stream.printLines(        
          "val data = vector_raw_data("+v+")",
          "var n = Math.max(4, array_length(data) * 2)",
          "while (n < "+minLen+") n = n*2",
          "val d = array_empty[T](n)",
          "array_copy(data, 0, d, 0, "+v+".length)",        
          "vector_set_raw_data("+v+", array_asimmutable(d))"    
        )
      }        
      
      
      // math      
      "+" is (infix, (Vector(T)), Vector(T), TNumeric(T)) implements zip((T,T,T), (0,1), "(a,b) => a+b")
      "*" is (infix, (T), Vector(T), TNumeric(T)) implements map((T,T), 0, "e => e*"+quotedArg(1))      
      "sum" is (infix, Nil, T, TNumeric(T)) implements reduce((T,Vector), 0, lookup("Numeric","zero"), "(a,b) => a+b")
             
      // bulk        
      "map" is (infix, (T ==> R), Vector(R), addTpePars = R) implements map((T,R), 0, "e => "+quotedArg(1)+"(e)")
      "reduce" is (infix, ((T,T) ==> T), T, TNumeric(T)) implements reduce((T,Vector), 0, lookup("Numeric","zero"), {
        "(a,b) => "+quotedArg(1)+"(a,b)"
      })
      "filter" is (infix, (T ==> MBoolean), Vector(T)) implements filter((T,T), 0, "e => " + quotedArg(1) + "(e)", "e => e")
      "mapreduce" is (infix, (T ==> T,(T,T) ==> T), T, TNumeric(T)) implements composite {
        quotedArg("self")+".map("+quotedArg(1)+").reduce("+quotedArg(2)+")"
      }
      
      
      // misc      
      // will print out of order in parallel, but hey
      "pprint" is (infix, Nil, MUnit, effect = simple) implements foreach((T,Vector), 0, "a => println(a)") 
      
            
      // parallel collectionification
      // This enables a tpe to be passed in as the collection type of a Delite op      
      "vector_appendable" is (compiler, (MInt,T), MBoolean) implements single("true")
      "vector_copy" is (compiler, (MInt,Vector(T),MInt,MInt), MUnit, effect = write(2)) implements single {
        val src = "vector_raw_data(" + quotedArg("self") + ")"
        val dest = "vector_raw_data(" + quotedArg(2) + ")"
        "array_copy("+src+","+quotedArg(1)+","+dest+","+quotedArg(3)+","+quotedArg(4)+")"
      }

      parallelize as ParallelCollectionBuffer(T, lookupOverloaded("apply",1), lookup("length"), lookupOverloaded("apply",0), lookup("update"), lookup("vector_set_length"), lookup("vector_appendable"), lookup("append"), lookup("vector_copy"))            
    }                    

                                           
    ()    
  }
}
 

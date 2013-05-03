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
      op (Vector) ("apply", static, T, (MInt), Vector(T), effect = mutable) implements allocates(Vector, ${$0}, ${ array_empty[T]($0) })
      
            
      // getters and setters
      "vector_raw_data" is (compiler, Nil, MArray(T)) implements getter(0, "_data")
      "vector_set_raw_data" is (compiler, (MArray(T)), MUnit, effect = write(0)) implements setter(0, "_data", quotedArg(1))
      "length" is (infix, Nil, MInt) implements getter(0, "_length")
      "vector_set_length" is (compiler, (MInt), MUnit, effect = write(0)) implements setter(0, "_length", quotedArg(1))      
      
      
      // data ops             
      "apply" is (infix, (MInt), T) implements composite ${ array_apply(vector_raw_data($self), $1) }                        
      // example named arg
      "update" is (infix, (("i",MInt),("e",T)), MUnit, effect = write(0)) implements composite ${
        array_update(vector_raw_data($self), $i, $e)
      }
      
      // example named, default arg. List is currently explicitly needed when mixing arg types.
      "slice" is (infix, List(("start",MInt,"0"),("end",MInt)), Vector(T)) implements single ${
        val out = Vector[T]($end - $start)
        var i = $start
        while (i < $end) {
          out(i-$start) = $self(i)
          i += 1
        }
        out
      }        
                
      "insert" is (infix, (MInt,T), MUnit, effect = write(0)) implements single ${
        vector_insertspace($self,$1,1)
        $self($1) = $2
      }
      
      "append" is (infix, (MInt,T), MUnit, effect = write(0)) implements single ${
        $self.insert($self.length, $2)
      }        
      
      "vector_insertspace" is (compiler, (("pos",MInt),("len",MInt)), MUnit, effect = write(0)) implements single ${
        vector_ensureextra($self,$len)
        val data = vector_raw_data($self)
        array_copy(data,$pos,data,$pos+$len,$self.length-$pos)
        vector_set_length($self,$self.length+$len)
      }
      
      "vector_ensureextra" is (compiler, ("extra",MInt), MUnit, effect = write(0)) implements single ${
        val data = vector_raw_data($self)
        if (array_length(data) - $self.length < $extra) {
          vector_realloc($self, $self.length+$extra)
        }
      }
      
      "vector_realloc" is (compiler, ("minLen",MInt), MUnit, effect = write(0)) implements single ${
        val data = vector_raw_data($self)
        var n = Math.max(4, array_length(data)*2)
        while (n < $minLen) n = n*2
        val d = array_empty[T](n)
        array_copy(data, 0, d, 0, $self.length)
        vector_set_raw_data($self, array_asimmutable(d))
      }        
      
      
      // math      
      "+" is (infix, (Vector(T)), Vector(T), TNumeric(T)) implements zip((T,T,T), (0,1), ${ (a,b) => a+b })
      "*" is (infix, (T), Vector(T), TNumeric(T)) implements map((T,T), 0, "e => e*"+quotedArg(1))      
      "sum" is (infix, Nil, T, TNumeric(T)) implements reduce((T,Vector), 0, lookup("Numeric","zero"), ${ (a,b) => a+b })
             
      // bulk        
      "map" is (infix, (T ==> R), Vector(R), addTpePars = R) implements map((T,R), 0, ${ e => $1(e) })
      
      "reduce" is (infix, ((T,T) ==> T), T, TNumeric(T)) implements reduce((T,Vector), 0, lookup("Numeric","zero"), ${
        (a,b) => $1(a,b)
      })
      
      "filter" is (infix, (T ==> MBoolean), Vector(T)) implements filter((T,T), 0, ${e => $1(e)}, ${e => e})
      
      "mapreduce" is (infix, (T ==> T,(T,T) ==> T), T, TNumeric(T)) implements composite ${
        $self.map($1).reduce($2)
      }
      
      
      // misc      
      // will print out of order in parallel, but hey
      "pprint" is (infix, Nil, MUnit, effect = simple) implements foreach((T,Vector), 0, ${a => println(a)}) 
      
            
      // parallel collectionification
      // This enables a tpe to be passed in as the collection type of a Delite op      
      "vector_appendable" is (compiler, (MInt,T), MBoolean) implements single("true")
      "vector_copy" is (compiler, (MInt,Vector(T),MInt,MInt), MUnit, effect = write(2)) implements single ${
        val src = vector_raw_data($self)
        val dest = vector_raw_data($2)
        array_copy(src, $1, dest, $3, $4)
      }

      parallelize as ParallelCollectionBuffer(T, lookupOverloaded("apply",1), lookup("length"), lookupOverloaded("apply",0), lookup("update"), lookup("vector_set_length"), lookup("vector_appendable"), lookup("append"), lookup("vector_copy"))            
    }                    

                                           
    ()    
  }
}
 

package ppl.dsl.forge
package examples
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait GraphCollectionOps {

  this: OptiGraphDSL =>
   
  def importGraphCollectionOps() {
	//////////////////////////////////////////////////////////////////////////////
	// GraphCollection DECLARATION
	//////////////////////////////////////////////////////////////////////////////
    val T = tpePar("T")
    val R = tpePar("R")
	val GraphCollection = tpe("GraphCollection", T) 
    data(GraphCollection,("_length", MInt),("_data",MArray(T)))
    //order between data and static allocates is implicit and must be the same
   
    //pass in a Int to create graph that will indicate # of nodes
    //edges is just *2 the # nodes, need to figure out dynamic allocation
    static(GraphCollection)("apply", T, MInt :: GraphCollection(T), effect=mutable) implements allocates(GraphCollection,${$0},${array_empty[T]($0)})
    static(GraphCollection)("apply", T, MArray(T) :: GraphCollection(T), effect=mutable) implements allocates(GraphCollection,${array_length($0)},${$0})

    val GraphCollectionOps = withTpe(GraphCollection)
    GraphCollectionOps{	
		compiler ("gc_raw_data") (Nil :: MArray(T)) implements getter(0, "_data")
		compiler ("gc_set_raw_data") (MArray(T) :: MUnit, effect = write(0)) implements setter(0, "_data", quotedArg(1))
	
		compiler ("length")(Nil :: MInt) implements getter(0,"_length")
		compiler ("gc_set_length")(MInt :: MUnit, effect = write(0)) implements setter(0, "_length",${$1})
		
		infix("apply")(MInt :: T) implements composite ${array_apply(gc_raw_data($self),$1)}
		
		infix("update")( (("id",MInt),("n",T)) :: MUnit, effect=write(0)) implements composite ${
			array_update(gc_raw_data($self),$id,$n)
		}

		compiler("gc_copy")((MInt,GraphCollection(T),MInt,MInt) :: MUnit, effect = write(2) ) implements composite ${
			val src = gc_raw_data($self)
			val dest = gc_raw_data($2) //fixme should be $2 but for some reason that won't work
			array_copy(src, $1, dest, $3, $4)
		}

		compiler("gc_raw_alloc")(MInt :: GraphCollection(R), addTpePars = R, effect=mutable) implements single ${
			GraphCollection[R]($1)
		}

		compiler ("gc_appendable") ((MInt,T) :: MBoolean) implements single("true")		

		compiler ("gc_append") ((MInt,T) :: MUnit, effect = write(0)) implements single ${
        	 gc_insert($self, $1, $2)
      	}
      	
        infix ("append") (T :: MUnit, effect = write(0)) implements single ${
        	gc_insert($self, length($self), $1)
      	}

      	compiler("gc_insert") ((MInt,T) :: MUnit, effect = write(0)) implements single ${
        	gc_insertspace($self,$1,1)
        	$self($1) = $2
      	} 
      	
      	compiler ("gc_insertspace") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements single ${
	        gc_ensureextra($self,$len)
	        val data = gc_raw_data($self)
	        array_copy(data,$pos,data,$pos+$len,length($self)-$pos)
	        gc_set_length($self,length($self)+$len)
	    }

	    compiler ("gc_ensureextra") (("extra",MInt) :: MUnit, effect = write(0)) implements single ${
	        val data = gc_raw_data($self)
	        if (array_length(data) - length($self) < $extra) {
	          gc_realloc($self, length($self)+$extra)
	        }
	    }
	    
		compiler ("gc_realloc") (("minLen",MInt) :: MUnit, effect = write(0)) implements single ${
	        val data = gc_raw_data($self)
	        var n = Math.max(4, array_length(data)*2).toInt
	        while (n < $minLen) n = n*2
	        val d = array_empty[T](n)
	        array_copy(data, 0, d, 0, length($self))
	        gc_set_raw_data($self, d.unsafeImmutable)
	    }

	    compiler ("table_copy") ((MInt,GraphCollection(T),MInt,MInt) :: MUnit, effect = write(2)) implements single ${
	        val src = gc_raw_data($self)
	        val dest = gc_raw_data($2)
	        array_copy(src, $1, dest, $3, $4)
	    }

		infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(T, 0, ${a => println(a)})

		parallelize as ParallelCollectionBuffer(T,lookupOp("gc_raw_alloc"),lookupOp("length"),lookupOverloaded("apply",2),lookupOp("update"),lookupOp("gc_set_length"),lookupOp("gc_appendable"),lookupOp("gc_append"),lookupOp("gc_copy"))
    }	
  } 
}

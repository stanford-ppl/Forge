package ppl.dsl.forge
package	dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait NodeDataOps {

  this: OptiGraphDSL =>
   
  def importNodeDataOps() {
	//////////////////////////////////////////////////////////////////////////////
	// NodeData DECLARATION
	//////////////////////////////////////////////////////////////////////////////
    val T = tpePar("T")
    val R = tpePar("R")
	val NodeData = tpe("NodeData", T) 
    data(NodeData,("_length", MInt),("_data",MArray(T)))
    //order between data and static allocates is implicit and must be the same
   
    //pass in a Int to create graph that will indicate # of nodes
    //edges is just *2 the # nodes, need to figure out dynamic allocation
    static(NodeData)("apply", T, MInt :: NodeData(T), effect=mutable) implements allocates(NodeData,${$0},${array_empty[T]($0)})
    static(NodeData)("apply", T, MArray(T) :: NodeData(T), effect=mutable) implements allocates(NodeData,${array_length($0)},${$0})

    val NodeDataOps = withTpe(NodeData)
    NodeDataOps{	
		compiler ("nd_raw_data") (Nil :: MArray(T)) implements getter(0, "_data")
		compiler ("nd_set_raw_data") (MArray(T) :: MUnit, effect = write(0)) implements setter(0, "_data", quotedArg(1))
	
		compiler ("nd_length")(Nil :: MInt) implements getter(0,"_length")
		compiler ("nd_set_length")(MInt :: MUnit, effect = write(0)) implements setter(0, "_length",${$1})
		
		infix("apply")(MInt :: T) implements composite ${array_apply(nd_raw_data($self),$1)}
		
		infix("update")( (("id",MInt),("n",T)) :: MUnit, effect=write(0)) implements composite ${
			array_update(nd_raw_data($self),$id,$n)
		}

		infix("nd_add")( (("id1",MInt),("id2",MInt)) :: T,TNumeric(T)) implements composite ${
			$self(id1)+$self(id2)
		}

		compiler("nd_copy")((MInt,NodeData(T),MInt,MInt) :: MUnit, effect = write(2) ) implements composite ${
			val src = nd_raw_data($self)
			val dest = nd_raw_data($2) //fixme should be $2 but for some reason that won't work
			array_copy(src, $1, dest, $3, $4)
		}

		compiler("nd_raw_alloc")(MInt :: NodeData(R), addTpePars = R, effect=mutable) implements single ${
			NodeData[R]($1)
		}

		compiler ("nd_appendable") ((MInt,T) :: MBoolean) implements single("true")		

		compiler ("nd_append") ((MInt,T) :: MUnit, effect = write(0)) implements single ${
        	 nd_insert($self, $1, $2)
      	}
      	
        infix ("append") (T :: MUnit, effect = write(0)) implements single ${
        	nd_insert($self, nd_length($self), $1)
      	}

      	compiler("nd_insert") ((MInt,T) :: MUnit, effect = write(0)) implements single ${
        	nd_insertspace($self,$1,1)
        	$self($1) = $2
      	} 
      	
      	compiler ("nd_insertspace") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements single ${
	        nd_ensureextra($self,$len)
	        val data = nd_raw_data($self)
	        array_copy(data,$pos,data,$pos+$len,nd_length($self)-$pos)
	        nd_set_length($self,nd_length($self)+$len)
	    }

	    compiler ("nd_ensureextra") (("extra",MInt) :: MUnit, effect = write(0)) implements single ${
	        val data = nd_raw_data($self)
	        if (array_length(data) - nd_length($self) < $extra) {
	          nd_realloc($self, nd_length($self)+$extra)
	        }
	    }
	    
		compiler ("nd_realloc") (("minLen",MInt) :: MUnit, effect = write(0)) implements single ${
	        val data = nd_raw_data($self)
	        var n = Math.max(4, array_length(data)*2).toInt
	        while (n < $minLen) n = n*2
	        val d = array_empty[T](n)
	        array_copy(data, 0, d, 0, nd_length($self))
	        nd_set_raw_data($self, d.unsafeImmutable)
	    }

		infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(T, 0, ${a => println("NodeData: " + a)})

		infix ("nd_print") (Nil :: MUnit, effect = simple) implements composite ${
			var i = 0
			while(i<nd_length($self)){
				println("NodeData -- Index: " + i + " Data: " + $self(i))
				i = i+1
			}
		}

		parallelize as ParallelCollectionBuffer(T,lookupOp("nd_raw_alloc"),lookupOp("nd_length"),lookupOverloaded("apply",2),lookupOp("update"),lookupOp("nd_set_length"),lookupOp("nd_appendable"),lookupOp("nd_append"),lookupOp("nd_copy"))
    }	
  } 
}

package ppl.dsl.forge
package examples
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait GraphCollectionBufferOps {

  this: OptiGraphDSL =>
   
  def importGraphCollectionBufferOps() {
  	/*
	//////////////////////////////////////////////////////////////////////////////
	// GraphCollectionBuffer DECLARATION
	//////////////////////////////////////////////////////////////////////////////
    val T = tpePar("T")
    val R = tpePar("R")
	val GraphCollectionBuffer = tpe("GraphCollectionBuffer", T) 
    data(GraphCollectionBuffer,("_length", MInt),("_data",MArrayBuffer(T)))
    //order between data and static allocates is implicit and must be the same
   
    //pass in a Int to create graph that will indicate # of nodes
    //edges is just *2 the # nodes, need to figure out dynamic allocation
    static(GraphCollectionBuffer)("apply", T, MInt :: GraphCollectionBuffer(T), effect=mutable) implements allocates(GraphCollectionBuffer,${$0},${array_buffer_empty[T]($0)})
    static(GraphCollectionBuffer)("apply", T, MArrayBuffer(T) :: GraphCollectionBuffer(T), effect=mutable) implements allocates(GraphCollectionBuffer,${array_buffer_length($0)},${$0})

    val GraphCollectionBufferOps = withTpe(GraphCollectionBuffer)
    GraphCollectionBufferOps{	
		compiler ("gc_raw_data") (Nil :: MArrayBuffer(T)) implements getter(0, "_data")
		compiler ("gc_set_raw_data") (MArrayBuffer(T) :: MUnit, effect = write(0)) implements setter(0, "_data", quotedArg(1))
	
		//array_length(data)	
		infix ("length")(Nil :: MInt) implements getter(0,"_length")
		compiler ("gc_set_length")(MInt :: MUnit, effect = write(0)) implements setter(0, "_length",${$1})
		
		compiler("apply")(MInt :: T) implements composite ${array_buffer_apply(gc_raw_data($self),$1)}
	
		//Should be add item	
		
		compiler("addItem") (T :: MUnit) implements composite ${
		    //NumNodes is set to 0, increase by 1 to add node
			//NumNodes will also serve as ID for this node
			val curPosition = $self.length()+1
			if(curPosition>=array_buffer_length(gc_raw_data($self))){
				gc_append($self,1,$1)
			}else{
				gc_update($self,curPosition,$1)
				gc_set_length($self,curPosition)
			}
		}
		
		//infix ("map") ((T ==> R) :: GraphCollectionBuffer(R), addTpePars = R) implements map((T,R), 0, ${ e => $1(e) })
		
		//infix ("flatMap") ((T ==> GraphCollectionBuffer(R)) :: GraphCollectionBuffer(R), addTpePars = R) implements composite ${
        //	GraphCollection.flatten($self.map($1))
      	//}
      	/*
	    infix ("flatten") ( GraphCollectionBuffer(GraphCollectionBuffer(T)) :: GraphCollectionBuffer(T) ) implements single ${
	      if ($master.length == 0){
	        GraphCollectionBuffer[T](0).unsafeImmutable
	      }
	      else {
	      	GraphCollectionBuffer[T](0).unsafeImmutable
	      }
	    }
		*/
	
		compiler("gc_copy")((MInt,GraphCollectionBuffer(T),MInt,MInt) :: MUnit, effect = write(2) ) implements composite ${
			val src = gc_raw_data($self)
			val dest = gc_raw_data($2) //fixme should be $2 but for some reason that won't work
			array_buffer_copy(src, $1, dest, $3, $4)
		}

		compiler ("gc_appendable") ((MInt,T) :: MBoolean) implements single("true")
		compiler("gc_append")((MInt,T) :: MUnit, effect=write(0)) implements single ${
			array_buffer_append(gc_raw_data($self),$2)
		}
		compiler("gc_update")( (("id",MInt),("n",T)) :: MUnit, effect=write(0)) implements single ${
			array_buffer_update(gc_raw_data($self),$id,$n)
		}	
		compiler("gc_raw_alloc")(MInt :: GraphCollectionBuffer(R), addTpePars = R, effect=mutable) implements single ${
			GraphCollectionBuffer[R]($1)
		}
		
		parallelize as ParallelCollectionBuffer(T,lookupOp("gc_raw_alloc"),lookupOp("length"),lookupOverloaded("apply",2),lookupOp("gc_update"),lookupOp("gc_set_length"),lookupOp("gc_appendable"),lookupOp("gc_append"),lookupOp("gc_copy"))
    }
    */	
  } 
}

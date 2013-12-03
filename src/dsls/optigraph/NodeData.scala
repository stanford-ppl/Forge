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
	val V = tpePar("V")
	val K = tpePar("K")
	val R = tpePar("R")
	val Tuple2 = lookupTpe("Tup2")
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
			
			infix ("get_raw_data") (Nil :: MArray(T)) implements single ${
				nd_raw_data($self)
			}

			infix("resize")(MInt :: MUnit, effect = write(0)) implements composite ${
				val data = nd_raw_data($self)
				val d = array_empty[T]($1)
				array_copy(data, 0, d, 0, $1)
				nd_set_raw_data($self, d.unsafeImmutable)
				nd_set_length($self,$1)
			}

			infix ("nd_length")(Nil :: MInt) implements getter(0,"_length")
			compiler ("nd_set_length")(MInt :: MUnit, effect = write(0)) implements setter(0, "_length",${$1})
			
			infix("apply")(MInt :: T) implements composite ${array_apply(nd_raw_data($self),$1)}
			
			infix("update")( (("id",MInt),("n",T)) :: MUnit, effect=write(0)) implements composite ${
				array_update(nd_raw_data($self),$id,$n)
			}

			compiler("nd_update")( (("id",MInt),("n",T)) :: MUnit, effect=write(0)) implements composite ${
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

	    compiler ("nd_apply") (MInt :: T) implements composite ${
				array_apply(nd_raw_data($self), $1)
	    }

			compiler ("nd_appendable") ((MInt,T) :: MBoolean) implements single("true")		

			compiler ("nd_append") ((MInt,T) :: MUnit, effect = write(0)) implements single ${
				 $self.nd_insert($self.nd_length, $2)
			}
			
			infix ("append") (T :: MUnit, effect = write(0)) implements single ${
				$self.nd_insert($self.nd_length, $1)
			}

			infix("nd_insert") ((MInt,T) :: MUnit, effect = write(0)) implements single ${
				nd_insertspace($self,$1,1)
				$self($1) = $2
			} 
			
			compiler ("nd_insertspace") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements single ${
				nd_ensureextra($self,$len)
				val data = nd_raw_data($self)
				array_copy(data,$pos,data,$pos+$len,$self.nd_length-$pos)
				nd_set_length($self,$self.nd_length+$len)
			}

			compiler ("nd_ensureextra") (("extra",MInt) :: MUnit, effect = write(0)) implements single ${
				val data = nd_raw_data($self)
				if (array_length(data) - $self.nd_length < $extra) {
				  nd_realloc($self, $self.nd_length+$extra)
				}
			}
			
			compiler ("nd_realloc") (("minLen",MInt) :: MUnit, effect = write(0)) implements single ${
				val data = nd_raw_data($self)
				var n = Math.max(4, array_length(data)*2).toInt
				while (n < $minLen) n = n*2
				val d = array_empty[T](n)
				array_copy(data, 0, d, 0, $self.nd_length)
				nd_set_raw_data($self, d.unsafeImmutable)
			}
			infix ("sum") (Nil :: T, TNumeric(T)) implements reduce(T, 0, ${numeric_zero[T]}, ${ (a,b) => a+b })

			infix ("zip") (NodeData(T) :: NodeData(T), TNumeric(T)) implements zip((T,T,T), (0,1), ${ (a,b) => a+b })
			infix ("zip_tuples") (NodeData(T) :: NodeData(Tuple2(T,T))) implements zip((T,T,Tuple2(T,T)), (0,1), ${ (a,b) => pack(a,b) })

			infix ("map") ((T ==> R) :: NodeData(R), addTpePars = R) implements map((T,R), 0, ${ e => $1(e) })


			//infix ("+=") (NodeData(T) :: NodeData(T), TNumeric(T)) implements composite ${
			//	val nodes = NodeView(nd_raw_data($self),$self.nd_length)
			//	nodes.map($self,$1) 
			//}
			//infix ("+=") (NodeData(T) :: NodeData(T), TNumeric(T)) implements map((T,T), 0, ${ e => $1(e) })
			//infix ("sum") (Nil :: T, A) implements reduce(T, 0, Z, ${ (a,b) => a+b })

			infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(T, 0, ${a => println("NodeData: " + a)})

			infix ("hashreduce") ((T ==> MBoolean,T ==> K,T ==> V,(V,V) ==> V) :: NodeData(V), TNumeric(V), addTpePars = (K,V)) implements hashFilterReduce((T,K,V), 0, ${e => $1(e)}, ${e => $2(e)}, ${e => $3(e)}, ${numeric_zero[V]}, ${(a,b) => $4(a,b)})
		  //infix ("filter") ( ((Tuple2(MInt,MInt) ==> MBoolean),(Tuple2(MInt,MInt) ==> MInt)) :: NodeData(MInt), addTpePars=K) implements filter((Tuple2(MInt,MInt),MInt), 0, ${w => $1(w)}, ${e => $2(e)})
		  infix ("filter") ( ((T ==> MBoolean),(T ==> MInt)) :: NodeData(MInt), addTpePars=K) implements filter((T,MInt), 0, ${w => $1(w)}, ${e => $2(e)})

	    //infix ("foreach_tuple") ((T ==> MUnit) :: MUnit) implements foreach(T, 0, ${ e => $1(e) })
	    infix ("foreach") ((T ==> MUnit) :: MUnit, effect = simple) implements foreach(T, 0, ${ e => $1(e) })

	    infix ("forloop") ((T ==> MUnit) :: MUnit) implements composite ${
	    	var i = 0
				while(i<$self.nd_length){
					$1($self(i))
					i = i+1
				}
	    }

			infix ("nd_print") (Nil :: MUnit, effect = simple) implements composite ${
				var i = 0
				while(i<$self.nd_length){
					println("NodeData -- Index: " + i + " Data: " + $self(i))
					i = i+1
				}
			}
			
			parallelize as ParallelCollectionBuffer(T,lookupOp("nd_raw_alloc"),lookupOp("nd_length"),lookupOp("nd_apply"),lookupOp("update"),lookupOp("nd_set_length"),lookupOp("nd_appendable"),lookupOp("nd_append"),lookupOp("nd_copy"))
		}	
  } 
}

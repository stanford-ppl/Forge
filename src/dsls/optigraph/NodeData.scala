/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: Stores data asscoicated with nodes in an array 
buffer indexed by internal node IDs
*///////////////////////////////////////////////////////////////

package ppl.dsl.forge
package	dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait NodeDataOps {
	this: OptiGraphDSL =>
	def importNodeDataOps() {
		val Tuple2 = lookupTpe("Tup2")
		val T = tpePar("T")
		val K = tpePar("K")
		val V = tpePar("V")
		val R = tpePar("R")
		val NodeData = tpe("NodeData", T) 

		data(NodeData,("_data",MArrayBuffer(T)))
		static(NodeData)("apply", T, MInt :: NodeData(T), effect = mutable) implements allocates(NodeData,${array_buffer_strict_empty[T]($0)})
		static(NodeData)("apply", T, MArray(T) :: NodeData(T)) implements allocates(NodeData,${array_buffer_new_imm($0)})
		static(NodeData)("apply", T, MArrayBuffer(T) :: NodeData(T)) implements allocates(NodeData,${$0})

		val NodeDataOps = withTpe(NodeData)
		NodeDataOps{	
			//////////////basic accessors//////////////////////////////
			infix("apply")(MInt :: T) implements composite ${array_buffer_apply(nd_raw_data($self),$1)}
			infix("update")( (("id",MInt),("n",T)) :: MUnit, effect=write(0)) implements composite ${array_buffer_update(nd_raw_data($self),$id,$n)}
			infix ("length")(Nil :: MInt) implements single ${array_buffer_length(nd_raw_data($self))}
			//the requirement of second argument (length) to the append in this case is useless but still nescessary
			//could probably be cleaned up in forge
			infix ("append") (T :: MUnit, effect = write(0)) implements composite ${nd_append($self,$self.length, $1)}
			//method to get an array of data to outside world
			infix ("getRawArray") (Nil :: MArray(T)) implements single ${array_buffer_result(nd_raw_data($self))}
			infix ("getRawArrayBuffer") (Nil :: MArrayBuffer(T)) implements single ${nd_raw_data($self)}

			//allows arrays to be set to proper size, useful in file I/O not necessary after groupby
			infix("resize")(MInt :: MUnit, effect = write(0)) implements composite ${
				val data = nd_raw_data($self)
				val d = array_buffer_empty[T]($1)
				array_buffer_copy(data, 0, d, 0, $1)
				nd_set_raw_data($self, d.unsafeImmutable)
				nd_set_length($self,$1)
			}
			infix("concat")(NodeData(T) :: NodeData(T)) implements composite ${
      	val result = array_empty[T]($0.length+$1.length)
      	array_copy($0.getRawArray,0,result,0,$0.length)
      	array_copy($1.getRawArray,0,result,$0.length,$1.length)
      	NodeData(result)
      }
			compiler ("nd_set_raw_data") (MArrayBuffer(T) :: MUnit, effect = write(0)) implements setter(0, "_data", quotedArg(1))

			///////////parallel operations////////////////////////////
			infix ("-") (NodeData(T) :: NodeData(T), TNumeric(T)) implements zip((T,T,T), (0,1), ${ (a,b) => a-b })
			infix ("+") (NodeData(T) :: NodeData(T), TNumeric(T)) implements zip((T,T,T), (0,1), ${ (a,b) => a+b })
			infix ("sum") (Nil :: T, TNumeric(T)) implements reduce(T, 0, ${numeric_zero[T]}, ${ (a,b) => a+b })
			infix ("map") ((T ==> R) :: NodeData(R), addTpePars = R) implements map((T,R), 0, ${ e => $1(e) })
			infix ("flatMap") ((T ==> NodeData(R)) :: NodeData(R), addTpePars = R) implements flatMap((T,R), 0, ${ e => $1(e) })
			infix ("filter") ( ((T ==> MBoolean),(T ==> MInt)) :: NodeData(MInt)) implements filter((T,MInt), 0, ${w => $1(w)}, ${e => $2(e)})
			infix ("foreach") ((T ==> MUnit) :: MUnit, effect = simple) implements foreach(T, 0, ${ e => $1(e) })
			infix ("reduce") (((T,T) ==> T) :: T, TNumeric(T)) implements reduce(T, 0, ${numeric_zero[T]}, ${ (a,b) => $1(a,b) })
			//FIXME figure out how to declare 0 so that this works
			infix ("reduceND") ( (((T,T) ==> T),R):: T,addTpePars=R) implements reduce(T, 0, ${$2.asInstanceOf[Rep[T]]}, ${
				(a,b) => $1(a,b)
			})
			infix ("groupBy") ((T ==> K,T ==> V) :: MHashMap(K, MArrayBuffer(V)), TNumeric(V), addTpePars = (K,V)) implements groupBy((T,K,V), 0, ${e => $1(e)}, ${e => $2(e)})
			infix ("groupByReduce") ((T ==> K,T ==> V,(V,V) ==> V) :: MHashMap(K, V), TNumeric(V), addTpePars = (K,V)) implements groupByReduce((T,K,V), 0, ${e => $1(e)}, ${e => $2(e)}, ${numeric_zero[V]}, ${(a,b) => $3(a,b)})


			/////////////////////////debug operations (print serial & parallel)///////////////////////
			infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(T, 0, ${a => println("NodeData: " + a)})
			infix ("forloop") ((T ==> MUnit) :: MUnit, effect = simple) implements composite ${
				var i = 0
				while(i<$self.length){
					$1($self(i))
					i = i+1
				}
			}
			infix ("print") (Nil :: MUnit, effect = simple) implements composite ${
				var i = 0
				while(i<$self.length){
					println("NodeData -- Index: " + i + " Data: " + $self(i))
					i = i+1
				}
			}

			///////////////methods for parallel collection buffer declaration/////////////////////////
			compiler ("nd_raw_data") (Nil :: MArrayBuffer(T)) implements getter(0, "_data")
			compiler("nd_raw_alloc")(MInt :: NodeData(R), addTpePars = R, effect=mutable) implements composite ${NodeData[R]($1)}
			compiler ("nd_apply") (MInt :: T) implements composite ${array_buffer_apply(nd_raw_data($self), $1)}
			compiler("nd_update")( (("id",MInt),("n",T)) :: MUnit, effect=write(0)) implements composite ${array_buffer_update(nd_raw_data($self),$id,$n)}
			compiler ("nd_set_length")(MInt :: MUnit, effect = write(0)) implements single ${array_buffer_set_length(nd_raw_data($self),$1)}
			compiler ("nd_appendable") ((MInt,T) :: MBoolean) implements single("true")	
			compiler ("nd_append") ((MInt,T) :: MUnit, effect = write(0)) implements composite ${array_buffer_append(nd_raw_data($self),$2)}
			compiler("nd_copy") ((MInt,NodeData(T),MInt,MInt) :: MUnit, effect = write(2)) implements single ${array_buffer_copy(nd_raw_data($self),$1,nd_raw_data($2),$3,$4)}

			parallelize as ParallelCollectionBuffer(T,lookupOp("nd_raw_alloc"),lookupOp("length"),lookupOp("nd_apply"),lookupOp("nd_update"),lookupOp("nd_set_length"),lookupOp("nd_appendable"),lookupOp("nd_append"),lookupOp("nd_copy"))
		}
	} 
}
/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: Stores data asscoicated with nodes in an array 
buffer indexed by internal node IDs
*///////////////////////////////////////////////////////////////

package ppl.dsl.forge
package dsls 
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
    static(NodeData)("apply", T, MInt :: NodeData(T)) implements allocates(NodeData,${array_buffer_strict_empty[T]($0)})
    static(NodeData)("apply", T, MArray(T) :: NodeData(T)) implements allocates(NodeData,${array_buffer_new_imm($0)})
    static(NodeData)("apply", T, MArrayBuffer(T) :: NodeData(T)) implements allocates(NodeData,${array_buffer_immutable($0)})
    static(NodeData)("fromFunction", T, (MInt,(MInt ==> T)) :: NodeData(T)) implements allocates(NodeData,${array_buffer_new_imm(array_fromfunction($0,$1))})

    val NodeDataOps = withTpe(NodeData)
    NodeDataOps{  
      //////////////basic accessors//////////////////////////////
      infix("apply")(MInt :: T) implements composite ${array_buffer_apply(nd_raw_data($self),$1)}
      infix("update")( (("id",MInt),("n",T)) :: MUnit, effect=write(0)) implements composite ${array_buffer_update(nd_raw_data($self),$id,$n)}
      infix ("length")(Nil :: MInt) implements single ${array_buffer_length(nd_raw_data($self))}
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
      //infix("sort")(Nil :: NodeData(T),TNumeric(T)) implements composite ${NodeData(array_sort($self.getRawArray))}

      ///////////parallel operations////////////////////////////
      infix ("-") (NodeData(T) :: NodeData(T), TNumeric(T)) implements zip((T,T,T), (0,1), ${ (a,b) => a-b })
      infix ("+") (NodeData(T) :: NodeData(T), TNumeric(T)) implements zip((T,T,T), (0,1), ${ (a,b) => a+b })
      infix ("map") ((T ==> R) :: NodeData(R), addTpePars = R) implements map((T,R), 0, ${ e => $1(e) })
      infix ("flatMap") ((T ==> NodeData(R)) :: NodeData(R), addTpePars = R) implements flatMap((T,R), 0, ${ e => $1(e) })
      infix ("filter") ( ((T ==> MBoolean),(T ==> R)) :: NodeData(R), addTpePars = R) implements filter((T,R), 0, ${w => $1(w)}, ${e => $2(e)})
      infix ("foreach") ((T ==> MUnit) :: MUnit, effect = simple) implements foreach(T, 0, ${ e => $1(e) })
      infix ("reduce") (((T,T) ==> T) :: T, TNumeric(T)) implements reduce(T, 0, ${numeric_zero[T]}, ${ (a,b) => $1(a,b) })
      infix ("reduceNested") ( (((T,T) ==> T),R):: T,addTpePars=R) implements reduce(T, 0, ${$2.asInstanceOf[Rep[T]]}, ${(a,b) => $1(a,b)})
      infix ("groupBy") ((T ==> K,T ==> V) :: MHashMap(K, MArrayBuffer(V)), addTpePars = (K,V)) implements groupBy((T,K,V), 0, ${e => $1(e)}, ${e => $2(e)})
      infix ("groupByReduce") ((T ==> K,T ==> V,(V,V) ==> V) :: MHashMap(K, V), TNumeric(V), addTpePars = (K,V)) implements groupByReduce((T,K,V), 0, ${e => $1(e)}, ${e => $2(e)}, ${numeric_zero[V]}, ${(a,b) => $3(a,b)})
      infix ("mapreduce") ( (T ==> R,(R,R) ==> R, T==>MBoolean) :: R, TNumeric(R), addTpePars=(R)) implements mapReduce((T,R), 0, ${e => $1(e)}, ${numeric_zero[R]}, ${(a,b) => $2(a,b)}, Some(${c => $3(c)}))
      infix ("distinct") (Nil :: NodeData(T)) implements composite ${NodeData(fhashmap_keys($0.groupByReduce[T,Int](e => e, e=>0,(a,b)=>0)))}
      infix("sort")(Nil :: NodeData(T),TNumeric(T)) implements composite ${NodeData(array_sort($self.getRawArray))}      
      infix ("sortBy") ((MInt ==> R) :: NodeData(T), TOrdering(R), addTpePars=R) implements composite ${
          NodeData[Int](array_sortIndices($self.length,$1)).map[T](i => $self(i))
      }
      infix ("sortIndicesBy") ((MInt ==> R) :: NodeData(MInt), TOrdering(R), addTpePars=R) implements single ${
          NodeData[Int](array_sortIndices($self.length,$1))
      }

      /////////////////////////debug operations (print serial & parallel)///////////////////////
      infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(T, 0, ${a => println("NodeData: " + a)})
      infix ("forindicies") ((MInt ==> MUnit) :: MUnit, effect = simple) implements composite ${
        array_buffer_forIndices($self.getRawArrayBuffer,$1)
      }
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
      compiler ("nd_set_raw_data") (MArrayBuffer(T) :: MUnit, effect = write(0)) implements setter(0, "_data", quotedArg(1))
      compiler ("nd_appendable") ((MInt,T) :: MBoolean) implements single("true") 
      compiler ("nd_append") ((MInt,T) :: MUnit, effect = write(0)) implements composite ${array_buffer_append(nd_raw_data($self),$2)}
      compiler("nd_copy") ((MInt,NodeData(T),MInt,MInt) :: MUnit, effect = write(2)) implements single ${array_buffer_copy(nd_raw_data($self),$1,nd_raw_data($2),$3,$4)}

      parallelize as ParallelCollectionBuffer(T,lookupOp("nd_raw_alloc"),lookupOp("length"),lookupOp("nd_apply"),lookupOp("nd_update"),lookupOp("nd_set_length"),lookupOp("nd_appendable"),lookupOp("nd_append"),lookupOp("nd_copy"))
    }
    direct(NodeData) ("sum", R, NodeData(R) :: R, TNumeric(R)) implements composite ${$0.reduce((a,b) => a+b)}
    direct(NodeData) ("sum", R, NodeData(NodeData(R)) :: NodeData(R), TFractional(R)) implements composite ${$0.reduceNested( ((a,b) => a+b),NodeData[R]($0.length))}
    compiler (NodeData) ("nd_fake_alloc", R, Nil :: NodeData(R)) implements single ${ NodeData[R](0) }
  } 
}
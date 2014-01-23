/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: Parallel input methods.  Output is currently done
serially.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait IOGraphOps {
  this: OptiGraphDSL =>
  def importIOGraphOps() {
    val IO = grp("GraphIO")
  	val Graph = lookupTpe("Graph")
  	val NodeData = lookupTpe("NodeData")
    val NodeIdView = lookupTpe("NodeIdView")

  	val T = tpePar("T")

    direct (IO) ("writeResults", T, (("path",MString),("graph",Graph),("data",NodeData(T))) :: MUnit, TNumeric(T), effect = simple) implements composite ${
    	val ids = $graph.getOrderedNodeIDs
    	writeGraphData($path,ids,data.getRawArray,$data.length)
    }
    compiler (IO) ("writeGraphData", T, (("path",MString),("ids",MArray(MString)),("data",MArray(T)),("length",MInt)) :: MUnit, TNumeric(T), effect = simple) implements codegen($cala, ${
      val xfs = new java.io.BufferedWriter(new java.io.FileWriter($path))
      xfs.write("#node id\\tdata\\n")
      for (i <- 0 until $length) {
        xfs.write($ids(i) + "\\t")
        xfs.write($data(i).toString + "\\n")
      }
      xfs.close()
    })

  	direct (IO) ("graphFromEdgeList", Nil, MString :: Graph) implements composite ${
      val input_edges = ForgeFileReader.readLines($0)({line =>
        //if(!line.startsWith("#")){
          val fields = line.fsplit(" ")
          pack(fields(0),fields(1)) 
        //} 
      })

      //contains the input tuples
      val edge_data = NodeData[Tup2[String,String]](input_edges)

      //concat source id's and destination id's then get distinct with groupbyreduce
      //get distinct src and dst for the upcoming flatmap on each
      val src_ids = NodeData(array_map[Tup2[String,String],String](input_edges, e => e._1))
      val dst_ids = NodeData(array_map[Tup2[String,String],String](input_edges, e => e._2))

      //get the overall disctinct could be repeats in src and dst
      val concat = src_ids.concat(dst_ids)
      val distinct_ids = NodeData(fhashmap_keys(concat.groupBy[String,String](e => e, e => e)))

      //set up the ID hash map
      val numNodes = distinct_ids.length
      val idView = NodeData(array_fromfunction(numNodes,{n => n}))
      val idHashMap = idView.groupByReduce[String,Int](n => distinct_ids(n), n => n, (a,b) => a)

      val src_groups = edge_data.groupBy(e => e._1, e => e._2)
      //must filter down the ids we want to flat map to just the distinct src ids we want
      //gets tricky because order of flatmap must match our internal id order other wise
      //the edge array gets screwed up
      //FIXME: support for string equality needs to be added to the contains
      val src_ids_ordered = NodeData(array_sort(fhashmap_keys(src_ids.groupBy[Int,Int](e => fhashmap_get(idHashMap,e),e => fhashmap_get(idHashMap,e)))))
      val src_edge_array = src_ids_ordered.flatMap{e => NodeData(src_groups(distinct_ids(e)))}.map{n => fhashmap_get(idHashMap,n)}
      val src_node_array = NodeData[Int](numNodes)

      val dst_groups = edge_data.groupBy(e => e._2, e => e._1)
      val dst_ids_ordered = NodeData(array_sort(fhashmap_keys(dst_ids.groupBy[Int,Int](e => fhashmap_get(idHashMap,e),e => fhashmap_get(idHashMap,e)))))
      val dst_edge_array = dst_ids_ordered.flatMap(e => NodeData(dst_groups(distinct_ids(e)))).map{n => fhashmap_get(idHashMap,n)}
      val dst_node_array = NodeData[Int](numNodes)

      var i = 0
      var src_array_index = 0
      var dst_array_index = 0
      while(i < numNodes-1){
        if(src_ids_ordered(src_array_index)==i){
          src_node_array(i+1) = array_buffer_length(fhashmap_get(src_groups,distinct_ids(i))) + src_node_array(i)
          if((src_array_index+1) < src_ids_ordered.length) src_array_index += 1
        }
        else{
          src_node_array(i+1) = src_node_array(i)
        }
        if(dst_ids_ordered(dst_array_index)==i){
          dst_node_array(i+1) = array_buffer_length(fhashmap_get(dst_groups,distinct_ids(i))) + dst_node_array(i)
          dst_array_index += 1
          if((dst_array_index+1) < dst_ids_ordered.length) dst_array_index += 1
        }
        else{
          dst_node_array(i+1) = dst_node_array(i)
        }
        i += 1
      }

      println("finished file I/O")
      Graph(true,numNodes,idHashMap,src_node_array.getRawArray,src_edge_array.getRawArray,dst_node_array.getRawArray,dst_edge_array.getRawArray)
    }
  }
}

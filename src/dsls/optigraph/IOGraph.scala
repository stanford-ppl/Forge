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
    compiler (IO) ("writeGraphData", T, (("path",MString),("ids",MArray(MInt)),("data",MArray(T)),("length",MInt)) :: MUnit, TNumeric(T), effect = simple) implements codegen($cala, ${
      val xfs = new java.io.BufferedWriter(new java.io.FileWriter($path))
      xfs.write("#node id\\tdata\\n")
      for (i <- 0 until $length) {
        xfs.write($ids(i).toString + "\\t")
        xfs.write($data(i).toString + "\\n")
      }
      xfs.close()
    })

  	direct (IO) ("graphFromEdgeList", Nil, MString :: Graph) implements composite ${
      val input_edges = ForgeFileReader.readLines($0)({line =>
        //if(!line.startsWith("#")){
          val fields = line.fsplit("\t")
          pack(fields(0).toInt,fields(1).toInt) 
        //} 
      })
      //contains the input tuples
      val edge_data = NodeData[Tup2[Int,Int]](input_edges)

      //concat source id's and destination id's then get distinct with groupbyreduce
      val src_ids = NodeData(array_map[Tup2[Int,Int],Int](input_edges, e => e._1))
      val dst_ids = NodeData(array_map[Tup2[Int,Int],Int](input_edges, e => e._2))
      val concat = src_ids.concat(dst_ids)
      val disct = fhashmap_keys(concat.groupByReduce[Int,Int](e => e, e => e, (a,b) => a))
      val distinct_ids = NodeData(disct)

      //set up the ID hash map
      val numNodes = distinct_ids.length
      val hm = SHashMap[Int,Int]()
      val idView = NodeIdView(distinct_ids.getRawArray,numNodes)
      idView.foreach{ id => hm(distinct_ids(id)) = id}
      val idHashMap = distinct_ids.groupByReduce[Int,Int](e => e, e => hm(e), (a,b) => a)

      val src_groups = edge_data.groupBy(e => e._1, e => e._2)
      val src_keys = NodeData(fhashmap_keys(src_groups))
      val src_edge_array = src_keys.flatMap(e => NodeData(src_groups(e))).map{n => fhashmap_get(idHashMap,n)}
      val src_node_array = NodeData[Int](numNodes)

      val dst_groups = edge_data.groupBy(e => e._2, e => e._1)
      val dst_keys = NodeData(fhashmap_keys(dst_groups))
      val dst_edge_array = dst_keys.flatMap(e => NodeData(dst_groups(e))).map{n => fhashmap_get(idHashMap,n)}
      val dst_node_array = NodeData[Int](numNodes)
      distinct_ids.foreach{ id =>
        if((fhashmap_get(idHashMap,id)+1)<(numNodes)){
          src_node_array(fhashmap_get(idHashMap,id)+1) = array_buffer_length(fhashmap_get(src_groups,id)) + src_node_array(fhashmap_get(idHashMap,id))
          dst_node_array(fhashmap_get(idHashMap,id)+1) = array_buffer_length(fhashmap_get(dst_groups,id)) + dst_node_array(fhashmap_get(idHashMap,id))
        }
      }

      println("finished file I/O")
      Graph(true,numNodes,idHashMap,src_node_array.getRawArray,src_edge_array.getRawArray,dst_node_array.getRawArray,dst_edge_array.getRawArray)
    }
  }
}
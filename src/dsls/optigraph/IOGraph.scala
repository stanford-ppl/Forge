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
    val DirectedGraph = lookupTpe("DirectedGraph")
    val UndirectedGraph = lookupTpe("UndirectedGraph")
    val NodeData = lookupTpe("NodeData")
    val NodeIdView = lookupTpe("NodeIdView")
    val K = tpePar("K")
    val V = tpePar("V")
    val Tuple2 = lookupTpe("Tup2")
    val Tuple3 = lookupTpe("Tup3")

    val SHashMap = tpe("scala.collection.mutable.HashMap", (K,V))

    val T = tpePar("T")

    direct (IO) ("writeResults", T, (("path",MString),("ids",MArray(MInt)),("data",NodeData(T))) :: MUnit, TNumeric(T), effect = simple) implements single ${
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

/////////////////////////////////////////////////////////////////////////////////////////////
////////General Loaders
/////////////////////////////////////////////////////////////////////////////////////////////
    direct (IO) ("loadUndirectedEdgeList", Nil, MString :: NodeData(Tuple2(MInt,MInt))) implements composite ${
      val input_edges =
        ForgeFileReader.readLinesFlattened($0)({line =>
          val fields = line.fsplit(" ")
          array_fromfunction(((array_length(fields)-1)*2),{n =>
            if(n==0) pack(fields(0).toInt,fields(1).toInt)
            else pack(fields(1).toInt,fields(0).toInt)
          })
        })
       NodeData[Tup2[Int,Int]](input_edges).distinct
    }
    direct (IO) ("loadDirectedEdgeList", Nil, MString :: NodeData(Tuple2(MInt,MInt))) implements composite ${
      val input_edges = ForgeFileReader.readLines($0)({line =>
          val fields = line.fsplit(" ")
          pack(fields(0).toInt,fields(1).toInt) 
      })
      NodeData[Tup2[Int,Int]](input_edges).distinct
    }
    direct (IO) ("createMeshEdgeList", Nil, MInt :: NodeData(Tuple2(MInt,MInt))) implements composite ${
      val meshSize = $0
      NodeData(array_fromfunction(meshSize,e => e)).flatMap{ e =>
        NodeData(array_fromfunction(meshSize,z => z)).map( z => pack(z,e))
      }.filter(e => e._1 != e._2, e => e).distinct
    }
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
////////Undirected CSR Loader
/////////////////////////////////////////////////////////////////////////////////////////////
    direct (IO) ("undirectedGraphFromCSR", Nil, ( (("nodes",MArray(MInt)),("edges",MArray(MInt))) :: UndirectedGraph)) implements composite ${
      UndirectedGraph(array_length(nodes),array_fromfunction[Int](array_length(edges),e=>e),nodes,edges,array_fromfunction[Double](array_length(edges),e=>1d))    
    }
    direct (IO) ("undirectedGraphFromEdgeList", Nil, ("edge_data",NodeData(Tuple2(MInt,MInt))) :: UndirectedGraph) implements composite ${
      val src_groups = edge_data.groupBy(e => e._1, e => e._2)

      //sort by degree, helps with skew for buckets of nodes
      val ids1 = NodeData(fhashmap_keys(src_groups))
      val ids = ids1.sortBy(a => ids1.length - array_buffer_length(fhashmap_get(src_groups,ids1(a)))) //reverse sort by degree

      val numNodes = ids.length
      val idView = NodeData(array_fromfunction(numNodes,{n => n}))
      val idHashMap = idView.groupByReduce[Int,Int](n => ids(n), n => n, (a,b) => a)
      
      val serial_out = assignUndirectedIndicies(numNodes,edge_data.length,ids,idHashMap,src_groups)

      UndirectedGraph(numNodes,ids.getRawArray,serial_out._1,serial_out._2,array_fromfunction[Double](edge_data.length,e=>1d))    
    }
    direct (IO) ("assignUndirectedIndicies", Nil, MethodSignature(List(("numNodes",MInt),("numEdges",MInt),("distinct_ids",NodeData(MInt)),("idHashMap",MHashMap(MInt,MInt)),("src_groups",MHashMap(MInt,MArrayBuffer(MInt)))),Tuple2(MArray(MInt),MArray(MInt)))) implements single ${
      val src_edge_array = NodeData[Int](numEdges)
      val src_node_array = NodeData[Int](numNodes)
      var i = 0
      var j = 0
      //I can do -1 here because I am pruning so the last node will never have any neighbors
      while(i < numNodes){
        val neighborhood = NodeData(fhashmap_get(src_groups,distinct_ids(i))).map(n =>fhashmap_get(idHashMap,n)).sort
        var k = 0
        while(k < neighborhood.length){
          src_edge_array(j) = neighborhood(k)
          j += 1
          k += 1
        }
        if(i < numNodes-1){
          src_node_array(i+1) = neighborhood.length + src_node_array(i)
        }
        i += 1
      }
      pack(src_node_array.getRawArray,src_edge_array.getRawArray)
    }
/////////////////////////////////////////////////////////////////////////////////////////////
////////Directed CSR Loader
/////////////////////////////////////////////////////////////////////////////////////////////
    direct (IO) ("directedGraphFromEdgeList", Nil, ("edge_data",NodeData(Tuple2(MInt,MInt))) :: DirectedGraph) implements composite ${
      val src_groups = edge_data.groupBy(e => e._1, e => e._2)
      val dst_groups = edge_data.groupBy(e => e._2, e => e._1)

      //go over src_ids and dst_ids map to degree
      //go over distinct add src_ids and dst_ids degree to hashmap
      //filter hashmap keys to those greater than sqrroot(n)
      val src_ids = NodeData(fhashmap_keys(src_groups))
      val dst_ids = NodeData(fhashmap_keys(dst_groups))
      val concat = NodeData(array_fromfunction(2,{n=>n})).flatMap[Int](e => if(e==0) src_ids else dst_ids)
      val distinct_ids = NodeData(fhashmap_keys(concat.groupBy[Int,Int](e => e, e => e)))

      //set up the ID hash map
      val numNodes = distinct_ids.length
      val idView = NodeData(array_fromfunction(numNodes,{n => n}))
      val idHashMap = idView.groupByReduce[Int,Int](n => distinct_ids(n), n => n, (a,b) => a)

      //must filter down the ids we want to flat map to just the distinct src ids we want
      //gets tricky because order of flatmap must match our internal id order other wise
      //the edge array gets screwed up
      val src_ids_ordered = NodeData(array_sort(array_map[Int,Int](src_ids.getRawArray,e => fhashmap_get(idHashMap,e))))
      val src_edge_array = src_ids_ordered.flatMap{e => NodeData(src_groups(distinct_ids(e))).map(n => fhashmap_get(idHashMap,n))}

      val dst_ids_ordered = NodeData(array_sort(array_map[Int,Int](dst_ids.getRawArray,e => fhashmap_get(idHashMap,e))))
      val dst_edge_array = dst_ids_ordered.flatMap(e => NodeData(dst_groups(distinct_ids(e))).map{n => fhashmap_get(idHashMap,n)})

      val serial_out = assignIndiciesSerialDirected(numNodes,distinct_ids,src_groups,src_ids_ordered,dst_groups,dst_ids_ordered)
      
      println("finished file I/O")
      DirectedGraph(numNodes,distinct_ids.getRawArray,serial_out(0).getRawArray,src_edge_array.getRawArray,serial_out(1).getRawArray,dst_edge_array.getRawArray)
    }
    direct (IO) ("assignIndiciesSerialDirected", Nil, MethodSignature(List(("numNodes",MInt),("distinct_ids",NodeData(MInt)),("src_groups",MHashMap(MInt,MArrayBuffer(MInt))),("src_ids_ordered",NodeData(MInt)),("dst_groups",MHashMap(MInt,MArrayBuffer(MInt))),("dst_ids_ordered",NodeData(MInt))), NodeData(NodeData(MInt)) )) implements single ${
      val src_node_array = NodeData[Int](numNodes)
      val dst_node_array = NodeData[Int](numNodes)

      var i = 0
      var src_array_index = 0
      var dst_array_index = 0
      while(i < numNodes-1){
        var degree = 0
        if(src_ids_ordered(src_array_index)==i){
          degree += array_buffer_length(fhashmap_get(src_groups,distinct_ids(i)))
          src_node_array(i+1) = array_buffer_length(fhashmap_get(src_groups,distinct_ids(i))) + src_node_array(i)
          if((src_array_index+1) < src_ids_ordered.length) src_array_index += 1
        }
        else src_node_array(i+1) = src_node_array(i)
        
        if(dst_ids_ordered(dst_array_index)==i){
          degree += array_buffer_length(fhashmap_get(dst_groups,distinct_ids(i)))
          dst_node_array(i+1) = array_buffer_length(fhashmap_get(dst_groups,distinct_ids(i))) + dst_node_array(i)
          if((dst_array_index+1) < dst_ids_ordered.length) dst_array_index += 1
        }
        else dst_node_array(i+1) = dst_node_array(i)
        
        i += 1
      }

      val result = NodeData[NodeData[Int]](2)
      result(0) = src_node_array
      result(1) = dst_node_array
      NodeData(result.getRawArray)

      result
    }    
  }
}

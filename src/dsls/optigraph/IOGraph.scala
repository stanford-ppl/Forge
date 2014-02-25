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
    val SpecUndirectedGraph = lookupTpe("SpecUndirectedGraph")
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
    /*
    //assume every edge is listed twice for undirected graphs
    direct (IO) ("undirectedGraphFromDirectedAdjList", Nil, (MString,MBoolean,MInt) :: UndirectedGraph) implements composite ${
      val input_edges = ForgeFileReader.readLinesFlattened($0)({line =>
        val fields = line.fsplit("\t")
        if(array_length(fields) <= 1){
          array_empty_imm[Tup2[Int,Int]](0)
        } 
        else{
          array_fromfunction(((array_length(fields)-1)*2),{n =>
            if(n < (array_length(fields)-1))
              pack(fields(0).toInt,fields(n+1).toInt)
            else 
              pack(fields((n+1)-(array_length(fields)-1)).toInt,fields(0).toInt)
          })
        }
      })
      val input_edges = ForgeFileReader.readLinesFlattened($0)({line =>
          val fields = line.fsplit("\t")
      })
      //contains either duplicate edges or not
      val edge_data = NodeData(input_edges).distinct

      val src_groups = edge_data.groupBy(e => e._1, e => e._2)
      val src_ids = NodeData(fhashmap_keys(src_groups))
      val distinct_ids = src_ids

      //set up the ID hash map
      val numNodes = distinct_ids.length
      val idView = NodeData(array_fromfunction(numNodes,{n => n}))
      val idHashMap = idView.groupByReduce[Int,Int](n => distinct_ids(n), n => n, (a,b) => a)

      //must filter down the ids we want to flat map to just the distinct src ids we want
      //gets tricky because order of flatmap must match our internal id order other wise
      //the edge array gets screwed up
      val src_ids_ordered = NodeData(array_sort(array_map[Int,Int](src_ids.getRawArray,e => fhashmap_get(idHashMap,e))))
      val src_edge_array = src_ids_ordered.flatMap{e => NodeData(src_groups(distinct_ids(e))).map(n => fhashmap_get(idHashMap,n))}

      val serial_out = assignIndiciesSerialUndirected($2,src_edge_array.length,numNodes,distinct_ids,src_groups,src_ids_ordered)
      
      println("finished file I/O. Edges: " + src_edge_array.length)
      UndirectedGraph(numNodes,serial_out._2,distinct_ids.getRawArray,serial_out._1,src_edge_array.getRawArray)
    }
    */
    //assume every edge is listed twice for undirected graphs
    direct (IO) ("specundirectedGraphFromEdgeList", Nil, (MString,MBoolean,MInt) :: SpecUndirectedGraph) implements composite ${
      val input_edges = if($1)
        ForgeFileReader.readLinesFlattened($0)({line =>
          val fields = line.fsplit(" ")
          array_fromfunction(((array_length(fields)-1)*2),{n =>
            if(n==0) pack(fields(0).toInt,fields(1).toInt)
            else pack(fields(1).toInt,fields(0).toInt)
          })
        })
      else
        ForgeFileReader.readLines($0)({line =>
          val fields = line.fsplit(" ")
          pack(fields(0).toInt,fields(1).toInt) 
        })
      //contains either duplicate edges or not
      val edge_data = NodeData[Tup2[Int,Int]](input_edges).distinct
      val src_groups = edge_data.groupBy(e => e._1, e => e._2)

      //sort by degree, helps with skew for buckets of nodes
      val src_id_degrees = edge_data.groupBy(e => array_buffer_length(fhashmap_get(src_groups,e._1)), e => e._1)
      val distinct_ids = (NodeData(fhashmap_keys(src_id_degrees)).sort).flatMap{e => NodeData(fhashmap_get(src_id_degrees,e)).distinct}

      //set up the ID hash map
      val numNodes = distinct_ids.length
      val idView = NodeData(array_fromfunction(numNodes,{n => n}))
      val idHashMap = idView.groupByReduce[Int,Int](n => distinct_ids(n), n => n, (a,b) => a)

      //val nbrHash = distinct_ids.map(e => NodeData(fhashmap_get(src_groups,e)).groupByReduce[Int,Int](e => fhashmap_get(idHashMap,e),e => 0,(a,b) => 0))
      val filtered_nbrs = distinct_ids.map(e => NodeData(src_groups(e)).filter(a => 
        fhashmap_get(idHashMap,a)>fhashmap_get(idHashMap,e),n =>fhashmap_get(idHashMap,n)).sort)
      val nbr_hash = filtered_nbrs.map(e => e.groupByReduce[Int,Int](k => k, v => 0, (a,b) => 0))
      val src_edge_array = idView.flatMap{e => filtered_nbrs(e)}

      val serial_out = assignIndiciesSerialUndirected($2,src_groups,src_edge_array.length,numNodes,distinct_ids,filtered_nbrs,distinct_ids)

      println("finished file I/O. Edges: " + src_edge_array.length)
      SpecUndirectedGraph(numNodes,serial_out._2,nbr_hash.getRawArray,distinct_ids.getRawArray,serial_out._1,src_edge_array.getRawArray)
    }
    direct (IO) ("assignIndiciesSerialUndirected", Nil, MethodSignature(List(("split",MInt),("degrees",MHashMap(MInt,MArrayBuffer(MInt))),("numEdges",MInt),("numNodes",MInt),("distinct_ids",NodeData(MInt)),("src_groups",NodeData(NodeData(MInt))),("src_ids_ordered",NodeData(MInt))),Tuple2(MArray(MInt),SHashMap(MInt,MInt)))) implements single ${
      val src_node_array = NodeData[Int](numNodes)
      val dst_node_array = NodeData[Int](numNodes)
      val sHash = SHashMap[Int,Int]()
      var i = 0
      var src_array_index = 0
      while(i < numNodes-1){
        val degree = array_buffer_length(fhashmap_get(degrees,distinct_ids(i)))
        src_node_array(i+1) = src_groups(i).length + src_node_array(i)
        if( (degree*degree) > numEdges) sHash.update(i,distinct_ids(i))
        i += 1
      }
      pack(src_node_array.getRawArray,sHash)
    }


    direct (IO) ("directedGraphFromEdgeList", Nil, MString :: DirectedGraph) implements composite ${
      val input_edges = ForgeFileReader.readLines($0)({line =>
          val fields = line.fsplit(" ")
          pack(fields(0).toInt,fields(1).toInt) 
      })

      //contains the input tuples
      val edge_data = NodeData[Tup2[Int,Int]](input_edges)

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
      val edge_arrays = serial_out._1
      
      println("finished file I/O")
      DirectedGraph(numNodes,serial_out._2,distinct_ids.getRawArray,edge_arrays(0).getRawArray,src_edge_array.getRawArray,edge_arrays(1).getRawArray,dst_edge_array.getRawArray)
    }
    direct (IO) ("assignIndiciesSerialDirected", Nil, MethodSignature(List(("numNodes",MInt),("distinct_ids",NodeData(MInt)),("src_groups",MHashMap(MInt,MArrayBuffer(MInt))),("src_ids_ordered",NodeData(MInt)),("dst_groups",MHashMap(MInt,MArrayBuffer(MInt))),("dst_ids_ordered",NodeData(MInt))), Tuple2(NodeData(NodeData(MInt)),SHashMap(MInt,MInt)) )) implements single ${
      val src_node_array = NodeData[Int](numNodes)
      val dst_node_array = NodeData[Int](numNodes)
      val sHash = SHashMap[Int,Int]()

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
        
        if(degree.toDouble > Math.sqrt(numNodes.toDouble)) sHash.update(i,distinct_ids(i))
        i += 1
      }

      val result = NodeData[NodeData[Int]](2)
      result(0) = src_node_array
      result(1) = dst_node_array
      NodeData(result.getRawArray)

      pack(result,sHash)
    }    
  }
}

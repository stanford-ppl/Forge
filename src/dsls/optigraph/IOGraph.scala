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

    val T = tpePar("T")
    val K = tpePar("K")
    val V = tpePar("V")
    val Tuple2 = lookupTpe("Tup2")
    val Tuple4 = lookupTpe("Tup4")


    val NodeIdView = lookupTpe("NodeIdView")
    val NodeCollection = lookupTpe("NodeCollection")
    val NodeData = lookupTpe("NodeData")
    val GraphBitSet = lookupTpe("GraphBitSet")
    val HashSet = lookupTpe("HashSet")
    val CSRDirectedGraph = lookupTpe("CSRDirectedGraph")
    val CSRUndirectedGraph = lookupTpe("CSRUndirectedGraph")
    val HABUndirectedGraph = lookupTpe("HABUndirectedGraph")

/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
////////Output methods (JAVA CodeGen)
/////////////////////////////////////////////////////////////////////////////////////////////
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
/////////////////////////////////////////////////////////////////////////////////////////////
////////CSR Directed Loaders
/////////////////////////////////////////////////////////////////////////////////////////////
    direct (IO) ("csrDirectedGraphFromEdgeList", Nil, MString :: CSRDirectedGraph) implements composite ${
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

      val edge_arrays = assignIndiciesSerialDirected(numNodes,distinct_ids,src_groups,src_ids_ordered,dst_groups,dst_ids_ordered)
      
      println("finished file I/O")
      CSRDirectedGraph(numNodes,distinct_ids.getRawArray,edge_arrays._1.getRawArray,src_edge_array.getRawArray,edge_arrays._2.getRawArray,dst_edge_array.getRawArray)
    }
    direct (IO) ("assignIndiciesSerialDirected", Nil, MethodSignature(List(("numNodes",MInt),("distinct_ids",NodeData(MInt)),("src_groups",MHashMap(MInt,MArrayBuffer(MInt))),("src_ids_ordered",NodeData(MInt)),("dst_groups",MHashMap(MInt,MArrayBuffer(MInt))),("dst_ids_ordered",NodeData(MInt))), Tuple2(NodeData(MInt),NodeData(MInt)) )) implements single ${
      val src_node_array = NodeData[Int](numNodes)
      val dst_node_array = NodeData[Int](numNodes)

      var i = 0
      var src_array_index = 0
      var dst_array_index = 0
      while(i < numNodes-1){
        if(src_ids_ordered(src_array_index)==i){
          src_node_array(i+1) = array_buffer_length(fhashmap_get(src_groups,distinct_ids(i))) + src_node_array(i)
          if((src_array_index+1) < src_ids_ordered.length) src_array_index += 1
        }
        else src_node_array(i+1) = src_node_array(i)
        
        if(dst_ids_ordered(dst_array_index)==i){
          dst_node_array(i+1) = array_buffer_length(fhashmap_get(dst_groups,distinct_ids(i))) + dst_node_array(i)
          if((dst_array_index+1) < dst_ids_ordered.length) dst_array_index += 1
        }
        else dst_node_array(i+1) = dst_node_array(i)
        
        i += 1
      }
      pack(src_node_array,dst_node_array)
    }
/////////////////////////////////////////////////////////////////////////////////////////////
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
    direct (IO) ("createMeshEdgeList", Nil, MInt :: NodeData(Tuple2(MInt,MInt))) implements composite ${
      val meshSize = $0
      NodeData(array_fromfunction(meshSize,e => e)).flatMap{ e =>
        NodeData(array_fromfunction(meshSize,z => z)).map( z => pack(z,e))
      }.distinct
    }
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
////////Undirected CSR Loader
/////////////////////////////////////////////////////////////////////////////////////////////
    direct (IO) ("csrPrunedUndirectedGraphFromEdgeList", Nil, ("edge_data",NodeData(Tuple2(MInt,MInt))) :: CSRUndirectedGraph) implements composite ${
      println("Edges: " + edge_data.length)
      val src_groups = edge_data.groupBy(e => e._1, e => e._2)

      //sort by degree, helps with skew for buckets of nodes
      val ids = NodeData(fhashmap_keys(src_groups))
      
      val distinct_ids = ids.sortBy({ (a,b) => 
        val aV = array_buffer_length(fhashmap_get(src_groups,ids(a)))
        val bV = array_buffer_length(fhashmap_get(src_groups,ids(b))) 
        if(aV < bV) -1
        else if(aV == bV) 0
        else 1
      })
    
      val numNodes = distinct_ids.length
      val idView = NodeData(array_fromfunction(numNodes,{n => n}))
      val idHashMap = idView.groupByReduce[Int,Int](n => distinct_ids(n), n => n, (a,b) => a)
      val serial_out = assignUndirectedIndicies(numNodes,edge_data.length/2,distinct_ids,idHashMap,src_groups)

      CSRUndirectedGraph(numNodes,distinct_ids.getRawArray,serial_out._1,serial_out._2)
    }
    direct (IO) ("assignUndirectedIndicies", Nil, MethodSignature(List(("numNodes",MInt),("numEdges",MInt),("distinct_ids",NodeData(MInt)),("idHashMap",MHashMap(MInt,MInt)),("src_groups",MHashMap(MInt,MArrayBuffer(MInt)))),Tuple2(MArray(MInt),MArray(MInt)))) implements single ${
      val src_edge_array = NodeData[Int](numEdges)
      val src_node_array = NodeData[Int](numNodes)
      var i = 0
      var j = 0
      //I can do -1 here because I am pruning so the last node will never have any neighbors
      while(i < numNodes-1){
        val neighborhood = NodeData(fhashmap_get(src_groups,distinct_ids(i))).filter(n => fhashmap_get(idHashMap,n) > i, n =>fhashmap_get(idHashMap,n)).sort
        var k = 0
        while(k < neighborhood.length){
          src_edge_array(j) = neighborhood(k)
          j += 1
          k += 1
        }
        src_node_array(i+1) = neighborhood.length + src_node_array(i)
        i += 1
      }
      pack(src_node_array.getRawArray,src_edge_array.getRawArray)
    }
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
////////Undirected HAB Loader
/////////////////////////////////////////////////////////////////////////////////////////////
    direct (IO) ("habPrunedUndirectedGraphFromEdgeList", Nil, (("edge_data",NodeData(Tuple2(MInt,MInt))),("underForHash",MInt),("bitSetMultiplier",MInt)) :: HABUndirectedGraph) implements composite ${
      //I can write this in about ten lines of code but the performance is horrible.
      val numEdges = edge_data.length
      val src_groups = edge_data.groupBy(e => e._1, e => e._2)

      //sort by degree, helps with skew for buckets of nodes
      val ids = NodeData(fhashmap_keys(src_groups))
      val distinct_ids = ids.sortBy({ (a,b) => 
        val aV = array_buffer_length(fhashmap_get(src_groups,ids(a)))
        val bV = array_buffer_length(fhashmap_get(src_groups,ids(b))) 
        if(aV > bV) -1
        else if(aV == bV) 0
        else 1
      })

      val numNodes = distinct_ids.length
      val idView = NodeData(array_fromfunction(numNodes,{n => n}))
      val idHashMap = idView.groupByReduce[Int,Int](n => distinct_ids(n), n => n, (a,b) => a)

      val filtered_nbrs = NodeData[NodeData[Int]](numNodes)
      var numBitSet = 0
      var numHash = 0
      var numCSRNodes = 0
      var numCSREdges = 0
      idView.foreach{ i =>
        filtered_nbrs(i) = NodeData(src_groups(distinct_ids(i))).filter(a => fhashmap_get(idHashMap,a) < i,n =>n)
        if(filtered_nbrs(i).length*bitSetMultiplier >= numNodes) numBitSet += 1
        else if((filtered_nbrs(i).length*bitSetMultiplier < numNodes) && (filtered_nbrs(i).length <= underForHash)) numHash += 1
        else {
          numCSRNodes += 1
          numCSREdges += filtered_nbrs(i).length
        }
      }
      
      /////////////////////////
      //These two ops are the overhead incurred for this special graph
      val distinct_ids2 = distinct_ids.sortBy({ (a,b) => 
        val aV = filtered_nbrs(fhashmap_get(idHashMap,distinct_ids(a))).length
        val bV = filtered_nbrs(fhashmap_get(idHashMap,distinct_ids(b))).length
        if(aV < bV) -1
        else if(aV == bV) 0
        else 1
      })
      val idHashMap2 = idView.groupByReduce[Int,Int](n => distinct_ids2(n), n => n, (a,b) => a)
      //////////////////////////

      println("NumHash: " + numHash  + " NumCSR: " + numCSRNodes +  " numBitSet: " + numBitSet)
 
      val serial_out = assignHABUndirectedIndicies(numNodes,distinct_ids,distinct_ids2,idHashMap,idHashMap2,numHash,numBitSet,numCSRNodes,numCSREdges,filtered_nbrs)
      val bitSetNeighborhoods = serial_out._2
      val hashNeighborhoods = serial_out._1
      val csrEdges = serial_out._4
      val csrNodes = serial_out._3

      HABUndirectedGraph(numNodes,numEdges,distinct_ids2.getRawArray,numHash,numCSRNodes,numBitSet,hashNeighborhoods.getRawArray,csrNodes.getRawArray,csrEdges.getRawArray,bitSetNeighborhoods.getRawArray)
    }
    direct (IO) ("assignHABUndirectedIndicies", Nil, MethodSignature(List(("numNodes",MInt),("distinct_ids",NodeData(MInt)),("distinct_ids2",NodeData(MInt)),("idHashMap",MHashMap(MInt,MInt)),("idHashMap2",MHashMap(MInt,MInt)),("numHash",MInt),("numBitSet",MInt),("numCSRNodes",MInt),("numCSREdges",MInt),("filtered_nbrs",NodeData(NodeData(MInt)))),Tuple4(NodeData(HashSet(MInt)),NodeData(GraphBitSet),NodeData(MInt),NodeData(MInt)))) implements single ${
      var ii = 0
      val bitSetNeighborhoods = NodeData[GraphBitSet](numBitSet)
      val hashNeighborhoods = NodeData[HashSet[Int]](numHash)

      var i = 0
      var j = 0
      val csrNodes = NodeData[Int](numCSRNodes)
      val csrEdges = NodeData[Int](numCSREdges)
      while(ii < numNodes){
        val data = filtered_nbrs(fhashmap_get(idHashMap,distinct_ids2(ii))).map(n => fhashmap_get(idHashMap2,n)).sort
        if(ii < numHash){
          hashNeighborhoods(ii) = HashSet(data.getRawArray)
        }
        else if(ii < (numCSRNodes + numHash)){
          val neighborhood = data
          var k = 0
          while(k < neighborhood.length){
            csrEdges(j) = neighborhood(k)
            j += 1
            k += 1
          }
          if(ii < (numCSRNodes+numHash-1)){
            csrNodes(i+1) = neighborhood.length + csrNodes(i)
            i += 1
          }
        }
        else{
          bitSetNeighborhoods( (ii - numCSRNodes - numHash) ) = GraphBitSet(data.getRawArray)
        }
        ii += 1
      }
      pack(hashNeighborhoods,bitSetNeighborhoods,csrNodes,csrEdges)
    }
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
////////Undirected Adjacency Loader
/////////////////////////////////////////////////////////////////////////////////////////////
    direct (IO) ("loadUndirectedAdjEdgeList", Nil, MString :: CSRUndirectedGraph) implements composite ${
      val input = NodeData(ForgeFileReader.readLines($0)({line =>
          val fields = line.fsplit("\t")
          NodeData[Int](array_map[String,Int](fields,e => e.toInt))
      }))
      val numNodes = input.length
      val idView = NodeData(array_fromfunction(numNodes,{n => n}))

      val sorted_input = input.sortBy({ (a,b) => 
        val aV = input(a).length
        val bV = input(b).length
        if(aV < bV) -1
        else if(aV == bV) 0
        else 1
      })

      val distinct_ids = sorted_input.map[Int]{nd => nd(0)}

      val nbrs = sorted_input.map{ nd =>
        NodeData.fromFunction(nd.length-1,a => a+1).map(a => nd(a)).sort
      }

      val numEdges = nbrs.mapreduce[Int](a => a.length, (a,b) => a+b, e=>true)
      val idHashMap = idView.groupByReduce[Int,Int](n => distinct_ids(n), n => n, (a,b) => a)
      val serial_out = assignAdjUndirectedIndicies(numNodes,numEdges,nbrs,distinct_ids,idHashMap)
      
      CSRUndirectedGraph(numNodes,distinct_ids.getRawArray,serial_out._1,serial_out._2)
    }
    direct (IO) ("assignAdjUndirectedIndicies", Nil, MethodSignature(List(("numNodes",MInt),("numEdges",MInt),("nbrs",NodeData(NodeData(MInt))),("distinct_ids",NodeData(MInt)),("idHashMap",MHashMap(MInt,MInt))),Tuple2(MArray(MInt),MArray(MInt)))) implements single ${
      val src_edge_array = NodeData[Int](numEdges/2)
      val src_node_array = NodeData[Int](numNodes)
      var i = 0
      var j = 0
      //I can do -1 here because I am pruning so the last node will never have any neighbors
      while(i < numNodes-1){
        val neighborhood = nbrs(i).filter(e => i < fhashmap_get(idHashMap,e), e => fhashmap_get(idHashMap,e)).sort
        var k = 0
        while(k < neighborhood.length){
          src_edge_array(j) = neighborhood(k)
          j += 1
          k += 1
        }
        src_node_array(i+1) = neighborhood.length + src_node_array(i)
        i += 1
      }
      pack(src_node_array.getRawArray,src_edge_array.getRawArray)
    }
/////////////////////////////////////////////////////////////////////////////////////////////
  }
}

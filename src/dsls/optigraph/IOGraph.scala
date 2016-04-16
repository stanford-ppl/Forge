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

    val writeGraphData = compiler (IO) ("writeGraphData", T, (("path",MString),("ids",MArray(MInt)),("data",MArray(T)),("length",MInt)) :: MUnit, TNumeric(T), effect = simple) 
        
    impl (writeGraphData) (codegen($cala, ${
      val xfs = new java.io.BufferedWriter(new java.io.FileWriter($path))
      xfs.write("#node id\\tdata\\n")
      for (i <- 0 until $length) {
        xfs.write($ids(i).toString + "\\t")
        xfs.write($data(i).toString + "\\n")
      }
      xfs.close()
    }))

    impl (writeGraphData) (codegen(cpp, ${
      std::ofstream xfs($path.c_str());
      xfs << "#node id \\tdata\\n";
      for (int64_t i=0; i < $length ; i++) {
        xfs << $ids->data[i] << "\\t";
        xfs << $data->data[i] << "\\n";
      }
      xfs.close();
    }))

    direct (IO) ("writeUndirectedGraphCSR", Nil, (("path",MString), ("graph", UndirectedGraph)) :: MUnit, effect = simple) implements composite ${
      val nodes = undirectedgraph_getcsrnodes(graph)
      val edges = undirectedgraph_getcsredges(graph)
      ForgeFileWriter.writeLines(path+".nodes", nodes.length){ i => ""+nodes(i) }
      ForgeFileWriter.writeLines(path+".edges", edges.length){ i => ""+edges(i) }
    }

    direct (IO) ("printGraph", Nil, MethodSignature(List(("pathin",MString),("level",MInt),("mod",MDouble),("newMod",MDouble),("numNodes",MInt),("numEdges",MInt),("nodes",MArray(MInt)),("edges",MArray(MInt)),("edgeWeights",MArray(MDouble))),MUnit), effect = simple) implements codegen($cala, ${
      val pin = $pathin
      val path = pin + "_" + $level.toString + ".txt"
      val xfs = new java.io.BufferedWriter(new java.io.FileWriter(path))
      xfs.write("number of nodes: " + $numNodes + " number of edges: " + $numEdges + "\\n")
      xfs.write("old mod: " + $mod + " new mod: " + $newMod + "\\n")

      for (i <- 0 until $nodes.length) {
        val src = i
        var start = $nodes(i)
        var end = if(i+1 < $nodes.length) $nodes(i+1) else $edges.length

        for(j <- start until end){
          xfs.write(i.toString + "\\t")
          xfs.write($edges(j).toString + "\\t")
          xfs.write($edgeWeights(j).toString + "\\n")
        }
      }
      xfs.close()
    })

/////////////////////////////////////////////////////////////////////////////////////////////
////////Edge List Loaders
/////////////////////////////////////////////////////////////////////////////////////////////
    direct (IO) ("loadEdgeList", Nil, (MString, MString) :: NodeData(Tuple2(MInt,MInt))) implements composite ${
      val input_edges = ForgeFileReader.readLines($0){line =>
        val fields = line.fsplit($1)
        pack(fields(0).toInt,fields(1).toInt)
      }
      NodeData[Tup2[Int,Int]](input_edges)
    }

    direct (IO) ("createMeshEdgeList", Nil, MInt :: NodeData(Tuple2(MInt,MInt))) implements composite ${
      val meshSize = $0
      NodeData(array_fromfunction(meshSize,e => e)).flatMap{ e =>
        NodeData(array_fromfunction(meshSize,z => z)).map( z => pack(z,e))
      }.filter(e => e._1 != e._2).distinct
    }

/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
////////Undirected CSR Loaders
/////////////////////////////////////////////////////////////////////////////////////////////
    direct (IO) ("loadUndirectedGraphFromCSR", Nil, ("path",MString) :: UndirectedGraph) implements composite ${
      val nodes = ForgeFileReader.readLines(path+".nodes"){ _.toInt }
      val edges = ForgeFileReader.readLines(path+".edges"){ _.toInt }
      undirectedGraphFromCSR(nodes, edges)      
    }

    direct (IO) ("undirectedGraphFromCSR", Nil, ( (("nodes",MArray(MInt)),("edges",MArray(MInt))) :: UndirectedGraph)) implements composite ${
      val numNodes = nodes.length - 1
      UndirectedGraph(numNodes,array_fromfunction[Int](numNodes,e=>e),nodes,edges,array_fromfunction[Double](edges.length,e=>1d))
    }

    //lazy val IBoolean = tpe("Boolean", stage = compile) //FIXME: Rep-less Boolean doesn't compile
    compiler (IO) ("sortEdges", Nil, (("edges",NodeData(Tuple2(MInt,MInt))), ("forward",CBoolean)) :: MArray(MInt)) implements composite ${
      array_sortIndices($0.length, (i,j) => {
        val x1 = if ($1) $0(i)._1 else $0(i)._2
        val x2 = if ($1) $0(i)._2 else $0(i)._1
        val y1 = if ($1) $0(j)._1 else $0(j)._2
        val y2 = if ($1) $0(j)._2 else $0(j)._1

        if (x1 == y1) { 
          if (x2 == y2) 0 
          else if (x2 > y2) 1
          else -1
        } 
        else if (x1 > y1) 1
        else -1
      })
    }

    direct (IO) ("undirectedGraphFromEdgeList", Nil, ("edge_data",NodeData(Tuple2(MInt,MInt))) :: UndirectedGraph) implements composite ${
      //println("edges: " + edge_data.length)
      val allEdges = array_fromfunction(edge_data.length * 2, i => { //reverse every edge
        if (i % 2 == 0) edge_data(i/2)
        else pack(edge_data(i/2)._2, edge_data(i/2)._1)
      })
      //println("alledges: " + allEdges.length)

      val sorted = sortEdges(NodeData(allEdges), forward = unit(true))


      //TODO: if provided node ids aren't dense we need to compute node ids as we build the graph
      val numNodes = allEdges(sorted(allEdges.length-1))._1 + 1
      //println("nodes: " + numNodes)

      val nodes = array_empty[Int](numNodes+1)
      val edges_buffer = array_buffer_empty[Int](allEdges.length) //internally allocate to max possible size

      var i = 0
      var currentNode = 0
      //nodes(0) = 0
      var numLocalEdges = 0
      var prevEdge_1 = -1; var prevEdge_2 = -1
      while (i < allEdges.length) {
        val currentEdge = allEdges(sorted(i))
        if (!(currentEdge._1 == prevEdge_1 && currentEdge._2 == prevEdge_2)) { //skip duplicate edges
          array_buffer_append(edges_buffer, currentEdge._2) //add unique edge
          if (currentEdge._1 == currentNode) { //same start node as before
            numLocalEdges += 1 //increment edge count for node
          } else { //new start node
            currentNode = currentEdge._1 //update to new start node
            nodes(currentNode) = nodes(currentNode-1) + numLocalEdges //add pointer to nodes array for end of prev node
            numLocalEdges = 1
          }

        }
        prevEdge_1 = currentEdge._1; prevEdge_2 = currentEdge._2
        i += 1
      }
      //println("final current node: " + currentNode)

      val edges = array_buffer_result(edges_buffer)
      nodes(numNodes) = edges.length

      val ids = array_fromfunction[Int](numNodes, i => i)
      val weights = array_empty[Double](0).unsafeImmutable //array_fromfunction(numEdges, i => 1.0)
      //println("undirected graph loaded, edges: " + edges.length)
      UndirectedGraph(numNodes, ids, nodes.unsafeImmutable, edges, weights)
    }

/////////////////////////////////////////////////////////////////////////////////////////////
////////Directed CSR Loaders
/////////////////////////////////////////////////////////////////////////////////////////////
    direct (IO) ("directedGraphFromEdgeList", Nil, ("edge_data",NodeData(Tuple2(MInt,MInt))) :: DirectedGraph) implements composite ${
      
      val sortedForward = sortEdges(edge_data, forward = unit(true))

      //TODO: if provided node ids aren't dense we need to compute these values as we build the graph
      val numNodes = edge_data(sortedForward(edge_data.length-1))._1 + 1
      val ids = array_fromfunction(numNodes, i => i)

      //TODO: if edges in list aren't distinct this needs to be computed as we build the graph
      val numEdges = edge_data.length

      val src_nodes = array_empty[Int](numNodes+1)
      val src_edges = array_empty[Int](numEdges)

      var edgeIdx = 0
      var currentNode = 0
      var numLocalEdges = 0
      while (edgeIdx < numEdges) {
        val currentEdge = edge_data(sortedForward(edgeIdx))
        //TODO: (duplicate edges): if (currentEdge._1 == prevEdge._1 && currentEdge._2 == prevEdge._2) skip duplicate
        src_edges(edgeIdx) = currentEdge._2 
        if (currentEdge._1 == currentNode) {
          numLocalEdges += 1
        } else {
          currentNode = currentEdge._1
          //TODO: (sparse ids): assign current node id to next dense id
          src_nodes(currentNode) = src_nodes(currentNode-1) + numLocalEdges
          numLocalEdges = 1
        }
        edgeIdx += 1
      }
      src_nodes(numNodes) = numEdges

      val sortedReverse = sortEdges(edge_data, forward = unit(false))
      val dst_nodes = array_empty[Int](numNodes+1)
      val dst_edges = array_empty[Int](numEdges)

      edgeIdx = 0
      currentNode = 0
      numLocalEdges = 0
      while (edgeIdx < numEdges) {
        val reverseEdge = edge_data(sortedReverse(edgeIdx))
        val currentEdge = pack(reverseEdge._2, reverseEdge._1)
        dst_edges(edgeIdx) = currentEdge._2 
        if (currentEdge._1 == currentNode) {
          numLocalEdges += 1
        } else {
          currentNode = currentEdge._1
          dst_nodes(currentNode) = dst_nodes(currentNode-1) + numLocalEdges
          numLocalEdges = 1
        }
        edgeIdx += 1
      }
      dst_nodes(numNodes) = numEdges

      DirectedGraph(numNodes, ids, src_nodes.unsafeImmutable, src_edges.unsafeImmutable, dst_nodes.unsafeImmutable, dst_edges.unsafeImmutable)
    }

/////////////////////////////////////////////////////////////////////////////////////////////
////////Undirected Adjacency Loaders
/////////////////////////////////////////////////////////////////////////////////////////////
    direct (IO) ("loadUndirectedAdjList", Nil, (MString, MString) :: NodeData(NodeData(MInt))) implements composite ${
      val input = NodeData(ForgeFileReader.readLines($0){line =>
          val fields = line.fsplit($1)
          NodeData[Int](array_map[String,Int](fields,e => e.toInt))
      })
      input
    }

    direct (IO) ("createICBUndirectedGraphFromAdjList", Nil, (NodeData(NodeData(MInt))) :: UndirectedGraph) implements composite ${
      val input = $0
      val numNodes = input.length
      val idView = NodeData(array_fromfunction(numNodes,{n => n}))
      var numEdges = 0l

      val csr = input.sortBy({ a => 
        numNodes - input(a).length
      })

      val distinct_ids = csr.map[Int]{nd => nd(0)}
      val idHashMap = idView.groupByReduce[Int,Int](n => distinct_ids(n), n => n, (a,b) => a)

      val csrNeighbors = csr.map[NodeData[Int]]{ nd =>
        NodeData.fromFunction(nd.length-1,a => a+1).map(a => fhashmap_get(idHashMap,nd(a))).sort
      }

      val serial_out = assignADJUndirectedIndicies(numNodes,numEdges.toInt,distinct_ids,idHashMap,csrNeighbors)
      UndirectedGraph(numNodes,distinct_ids.getRawArray,serial_out._1,serial_out._2,array_fromfunction[Double](numEdges.toInt,e=>1d))    
    }

    direct (IO) ("assignADJUndirectedIndicies", Nil, MethodSignature(List(("numNodes",MInt),("numEdges",MInt),("distinct_ids",NodeData(MInt)),("idHashMap",MHashMap(MInt,MInt)),("src_groups",NodeData(NodeData(MInt)))),Tuple2(MArray(MInt),MArray(MInt)))) implements single ${
      val src_edge_array = NodeData[Int](numEdges)
      val src_node_array = NodeData[Int](numNodes)
      var i = 0
      var j = 0
      //I can do -1 here because I am pruning so the last node will never have any neighbors
      while(i < numNodes){
        val neighborhood = src_groups(i)
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
  }
}

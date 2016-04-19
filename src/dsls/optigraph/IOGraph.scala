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

    // Generate a dense set of node ids from the ids in the given edge list, order preserving
    compiler (IO) ("densifyNodes", Nil, ("edges",NodeData(Tuple2(MInt,MInt))) :: Tuple2(MArray(MInt), MHashMap(MInt,MInt))) implements composite ${
      val distinctMap = SHashMap[Int,Boolean] // TODO: faster hashmap impl
      edges.forloop(e => {
        distinctMap(e._1) = true
        distinctMap(e._2) = true
      })

      val nodes = array_sort(distinctMap.keys)
      val nodeMap = array_groupByReduce(array_fromfunction(nodes.length, i => i), (i:Rep[Int]) => nodes(i), (i:Rep[Int]) => i, (a:Rep[Int],b:Rep[Int]) => a)
      pack(nodes, nodeMap)
    }

    compiler (IO) ("buildCSRFromEdgeList", Nil, (("numNodes", MInt), ("edges", NodeData(Tuple2(MInt,MInt))), ("nodeMap", MHashMap(MInt,MInt)), ("withLoops", MBoolean), ("forward", CBoolean)) :: Tuple2(MArray(MInt),MArray(MInt))) implements composite ${
      val sorted = sortEdges(edges, forward)

      val nodes = array_empty[Int](numNodes+1) // Pad nodes to make indexing neighborhoods simpler
      val edges_buffer = array_buffer_empty[Int](edges.length) // Internally allocate to max possible size

      var i = 0
      var currentNode = 0
      nodes(0) = 0
      var numLocalEdges = 0
      var prevEdge_1 = -1; var prevEdge_2 = -1
      while (i < edges.length) {
        val inputEdge = edges(sorted(i))
        val currentEdge_1 = nodeMap(if (forward) inputEdge._1 else inputEdge._2)
        val currentEdge_2 = nodeMap(if (forward) inputEdge._2 else inputEdge._1)
        
        // New start node, so complete previous node
        if (currentEdge_1 != currentNode) { 
          currentNode = currentEdge_1
          nodes(currentNode) = nodes(currentNode-1) + numLocalEdges // Add pointer to nodes array for end of prev node
          numLocalEdges = 0
        }

        // Check current edge for addition to graph
        if (currentEdge_1 == prevEdge_1 && currentEdge_2 == prevEdge_2) { } // Skip duplicate edges
        else if (!withLoops && currentEdge_1 == currentEdge_2) { } // Skip loop edge
        else {
          array_buffer_append(edges_buffer, currentEdge_2) // Add unique edge
          numLocalEdges += 1
        }
        prevEdge_1 = currentEdge_1; prevEdge_2 = currentEdge_2
        i += 1
      }
      //println("final current node: " + currentNode)

      val edges_array = array_buffer_result(edges_buffer)
      nodes(numNodes) = edges_array.length

      pack(nodes.unsafeImmutable, edges_array)
    }

    direct (IO) ("undirectedGraphFromEdgeList", Nil, MethodSignature(List(("edge_data",NodeData(Tuple2(MInt,MInt))), ("withLoops", MBoolean, "unit(true)")), UndirectedGraph)) implements composite ${
      //println("edges: " + edge_data.length)
      val allEdges = NodeData(array_fromfunction(edge_data.length * 2, i => { //reverse every edge
        if (i % 2 == 0) edge_data(i/2)
        else pack(edge_data(i/2)._2, edge_data(i/2)._1)
      }))
      //println("alledges: " + allEdges.length)

      val (ids, nodeMap) = unpack(densifyNodes(allEdges))
      val numNodes = ids.length
      //println("nodes: " + numNodes)

      val (nodes, edges) = unpack(buildCSRFromEdgeList(numNodes, allEdges, nodeMap, withLoops, forward = unit(true)))

      val weights = array_empty[Double](0).unsafeImmutable //array_fromfunction(numEdges, i => 1.0)
      //println("undirected graph loaded, edges: " + edges.length)
      UndirectedGraph(numNodes, ids, nodes.unsafeImmutable, edges, weights)
    }

/////////////////////////////////////////////////////////////////////////////////////////////
////////Directed CSR Loaders
/////////////////////////////////////////////////////////////////////////////////////////////
    direct (IO) ("directedGraphFromEdgeList", Nil, MethodSignature(List(("edge_data",NodeData(Tuple2(MInt,MInt))), ("withLoops", MBoolean, "unit(true)")), DirectedGraph)) implements composite ${
      val (ids, nodeMap) = unpack(densifyNodes(edge_data))
      val numNodes = ids.length

      val (out_nodes, out_edges) = unpack(buildCSRFromEdgeList(numNodes, edge_data, nodeMap, withLoops, forward = unit(true)))
      val (in_nodes, in_edges) = unpack(buildCSRFromEdgeList(numNodes, edge_data, nodeMap, withLoops, forward = unit(false)))

      DirectedGraph(numNodes, ids, out_nodes, out_edges, in_nodes, in_edges)
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

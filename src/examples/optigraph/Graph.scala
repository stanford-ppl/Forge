package ppl.dsl.forge
package examples
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait GraphOps {
/*
	myop implements codegen $cala ${
		new java.util.concurrent.atomic.AtomicIntegerArray
	}
*/
  this: OptiGraphDSL =>
   
  def importGraphOps() {
	val Node = lookupTpe("Node")
	val Edge = lookupTpe("Edge")
	val GraphCollection = lookupTpe("GraphCollection")
	val EdgeView = lookupTpe("EdgeView")

	//////////////////////////////////////////////////////////////////////////////
	// GRAPH DECLARATION
	//////////////////////////////////////////////////////////////////////////////
    
    val Graph = tpe("Graph") 
    val A = tpePar("T")
    data(Graph,("_numNodes",MInt),("_nodes",MArray(MInt)),("_numEdges",MInt),("_edges",MArray(MInt))) 
    //order between data and static allocates is implicit and must be the same
   
    //pass in a Int to create graph that will indicate # of nodes
    //edges is just *2 the # nodes, need to figure out dynamic allocation
    static(Graph)("apply", Nil, (MArray(MInt),MArray(MInt)) :: Graph, effect=mutable) implements allocates(Graph,${array_length($0)}, ${$0}, ${array_length($1)}, ${$1})
    static (Graph) ("fromFile", Nil, (MString,MString, MString ==> MInt) :: Graph ) implements single ${
      	val nodes = ForgeFileReader.readLines($0)($2)
      	val edges = ForgeFileReader.readLines($1)($2)
      	Graph(nodes,edges)
    }

    val GraphOps = withTpe(Graph)
    
    GraphOps{
    	
		infix ("node_neighbors") (Node :: EdgeView) implements composite ${
			val id = $1()
			
			val start = node_apply($self,id)
			val end = array_length(edge_raw_data($self))
			if(id < array_length(node_raw_data($self)) ) {
				end = node_apply($self,(id+1))
			}
			val neighbors = EdgeView(edge_raw_data($self),start,1,end-start)
			neighbors
		}
		infix ("inBFS") ((Node,Node)::MInt) implements composite ${
			//val frontier = GraphCollectionBuffer(1)
			//frontier(0) = $1() 
			
			val frontier = $self.node_neighbors($1)
			/*
			val visited = GraphCollection[Int]($self.getNumNodes())
			val depth = 0
			frontier.foreach{n => visited.gc_update(n,depth+1)}
			visited.print
			//frontier.pprint
			return visited(unit(0))
			*/
			return unit(1)
		}
		
    	
		compiler ("node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_nodes")
		compiler ("node_set_raw_data") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_nodes", quotedArg(1))
		compiler("node_update")( (("id",MInt),("n",MInt)) :: MUnit, effect=write(0)) implements single ${
			array_update(node_raw_data($self),$id,$n)
		}
		compiler("node_apply")(MInt :: MInt) implements composite ${array_apply(node_raw_data($self),$1)}

		infix ("getNumNodes")(Nil :: MInt) implements getter(0,"_numNodes")
		compiler ("setNumNodes")(MInt :: MUnit, effect = write(0)) implements setter(0, "_numNodes",${$1})
		
		compiler ("edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_edges")
		compiler ("edge_set_raw_data") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_edges", quotedArg(1))
		
		compiler ("getNumEdges")(Nil :: MInt) implements getter(0,"_numEdges")
		compiler ("setNumEdges")(MInt :: MUnit, effect = write(0)) implements setter(0, "_numEdges",${$1})

		/*
		infix("addNode") (Nil :: Node) implements composite ${
		    //NumNodes is set to 0, increase by 1 to add node
			//NumNodes will also serve as ID for this node
			val id = getNumNodes($self) + 1
			setNumNodes($self,id)	
			val n = Node(id)
			//append to the end of an array buffer	
            array_buffer_update(node_raw_data($self),id,n)
			n
		}

		compiler("node_apply")(MInt :: Node) implements composite ${array_buffer_apply(node_raw_data($self),$1)}
		compiler("node_copy")((MInt,MArrayBuffer(Node),MInt,MInt) :: MUnit, effect = write(2) ) implements composite ${
			val src = node_raw_data($self)
			val dest = node_raw_data($0) //fixme should be $2 but for some reason that won't work
			array_buffer_copy(src, $1, dest, $3, $4)
		}

		compiler ("node_appendable") ((MInt,Node) :: MBoolean) implements single("true")
		compiler("node_append")(Node :: MUnit, effect=write(0)) implements single ${
			array_buffer_append(node_raw_data($self),$1)
		}
		compiler("node_update")( (("id",MInt),("n",Node)) :: MUnit, effect=write(0)) implements single ${
			array_buffer_update(node_raw_data($self),$id,$n)
		}	
		compiler("graph_raw_alloc")(Nil :: Graph) implements single ${
			Graph()
		}
		parallelize as ParallelCollectionBuffer(Node,lookupOp("graph_raw_alloc"),lookupOp("getNumNodes"),lookupOp("node_apply"),lookupOp("node_update"),lookupOp("setNumNodes"),lookupOp("node_appendable"),lookupOp("node_append"),lookupOp("node_copy"))
		

		infix("addEdge") ((Node,Node) :: Edge) implements composite ${
			val e = Edge($1,$2)
			val id = getNumEdges($self)+1
			setNumEdges($self,id)
			array_buffer_update(edge_raw_data($self),id,e)
			e
		}
		
		//infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(Node, 0, ${a => println(a)})
		//infix("printNodes")(Nil ::  MUnit, effect=simple) implements foreach(Node, 0, {n=>println("i")})
    */
    }
  } 
}

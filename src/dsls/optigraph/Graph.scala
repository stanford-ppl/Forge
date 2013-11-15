package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait GraphOps{
  this: OptiGraphDSL =>
   
  def importGraphOps() {
	val Node = lookupTpe("Node")
	val Edge = lookupTpe("Edge")
	val NodeData = lookupTpe("NodeData")
	val GraphCollection = lookupTpe("GraphCollection")
	val ArrayView = lookupTpe("ArrayView")
	val NodeView = lookupTpe("NodeView")

	//////////////////////////////////////////////////////////////////////////////
	// GRAPH DECLARATION
	//////////////////////////////////////////////////////////////////////////////    
    val Graph = tpe("Graph") 
    val T = tpePar("T")
    data(Graph,("_numNodes",MInt),("_nodes",MArray(MInt)),("_numEdges",MInt),("_edges",MArray(MInt))) 

    static(Graph)("apply", Nil, (MArray(MInt),MArray(MInt)) :: Graph) implements allocates(Graph,${array_length($0)}, ${$0}, ${array_length($1)}, ${$1})
    static (Graph) ("fromFile", Nil, (MString,MString, MString ==> MInt) :: Graph ) implements composite ${
      	val nodes = ForgeFileReader.readLines($0)($2)
      	val edges = ForgeFileReader.readLines($1)($2)
      	Graph(nodes,edges)
    }
    
    val GraphOps = withTpe(Graph)     
    GraphOps{
    	infix ("node_neighbors") (Node :: ArrayView) implements composite ${
			val id = $1()
			//-1 implies no neighbors
			var start = node_apply($self,id)
			var end = array_length(edge_raw_data($self))
			if( (id+1) < array_length(node_raw_data($self)) ) {	
				end = node_apply($self,(id+1))
			}
			if(start == -1 || end == -1){
				start = 0
				end = 0
			}
			ArrayView(edge_raw_data($self),start,1,end-start)
		}
		infix ("inBFS") ( (Node, ((Node,NodeData(T)) ==> T) ) :: NodeData(T), addTpePars=T) implements composite ${
			val levelArray = GraphCollection[Int]( $self.get_num_nodes())
			val bitMap = AtomicIntArray( $self.get_num_nodes() )
			val nodes = NodeView(node_raw_data($self),$self.get_num_nodes) 
			val nd = NodeData[T]($self.get_num_nodes())

			levelArray($1()) = 1
			set(bitMap,$1(),1)
			var finished = false
			var level =	1
			while(!finished){
				finished = true
				nodes.foreach{n =>	
					if( levelArray(n) == level){
						nd(n) = $2(Node(n),nd)
						//println("Value for Node: " + n + " = " + myI)
						val neighbor = $self.node_neighbors(Node(n))
						neighbor.foreach{nghbr =>
							if(testAtomic(bitMap,nghbr,0)){
								if(testAndSetAtomic(bitMap,nghbr,0,1)){
									levelArray(nghbr) = level+1
									finished = false
						}}}//end nghbr for each	
					}}//end nodes for each
				level = level + 1
			}//end while	
			levelArray.pprint
			nd	
		}
		
		compiler ("node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_nodes")
		compiler("node_apply")(MInt :: MInt) implements composite ${array_apply(node_raw_data($self),$1)}
		
		infix("get_node_from_id")(MInt :: Node) implements composite ${
			//if($1 >= $self.get_num_nodes() || $1 < 0){
			//	throw new AssertionError("Node ID is not in current graph.  Out of bounds.")
			//}
			Node($1)
		}

		infix ("get_num_nodes")(Nil :: MInt) implements getter(0,"_numNodes")
		
		compiler ("edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_edges")		
		compiler ("get_num_edges")(Nil :: MInt) implements getter(0,"_numEdges")
    }
  } 
}

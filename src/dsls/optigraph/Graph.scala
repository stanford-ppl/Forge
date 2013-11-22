package ppl.dsl.forge
package	dsls 
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
    val R = tpePar("R")
    data(Graph,("_directed",MBoolean),("_numNodes",MInt),("_nodes",MArray(MInt)),("_numEdges",MInt),("_edges",MArray(MInt)),("_rNodes",MArray(MInt)),("_rEdges",MArray(MInt))) 

    static (Graph) ("fromFile", Nil, (MString,MString,MString,MString,MString ==> MInt) :: Graph ) implements composite ${
      	val nodes = ForgeFileReader.readLines($0)($4)
      	val edges = ForgeFileReader.readLines($1)($4)
      	val rnodes = ForgeFileReader.readLines($2)($4)
      	val redges = ForgeFileReader.readLines($3)($4)
      	Graph(true,nodes,edges,rnodes,redges)
    }    
    static (Graph) ("fromFile", Nil, (MString,MString, MString ==> MInt) :: Graph ) implements composite ${
      	val nodes = ForgeFileReader.readLines($0)($2)
      	val edges = ForgeFileReader.readLines($1)($2)
      	Graph(false,nodes,edges)
    }
    static(Graph)("apply", Nil, (MBoolean,MArray(MInt),MArray(MInt),MArray(MInt),MArray(MInt)) :: Graph) implements allocates(Graph,${$0},${array_length($1)}, ${$1}, ${array_length($2)}, ${$2},${$3},${$4})
    static(Graph)("apply", Nil, (MBoolean,MArray(MInt),MArray(MInt)) :: Graph) implements allocates(Graph,${$0},${array_length($1)}, ${$1}, ${array_length($2)}, ${$2},${ array_empty[Int](unit(0))},${array_empty[Int](unit(0))})

    
    val GraphOps = withTpe(Graph)     
    GraphOps{
    	infix ("is_directed") (Nil :: MBoolean) implements getter(0,"_directed") 

    	infix ("out_neighbors") (Node :: ArrayView(MInt)) implements composite ${
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
			ArrayView[Int](edge_raw_data($self),start,1,end-start)
		}
		infix ("in_neighbors") (Node :: ArrayView(MInt)) implements composite ${
			val id = $1()
			//-1 implies no neighbors
			var start = r_node_apply($self,id)
			var end = array_length(r_edge_raw_data($self))
			if( (id+1) < array_length(r_node_raw_data($self)) ) {	
				end = r_node_apply($self,(id+1))
			}
			if(start == -1 || end == -1){
				start = 0
				end = 0
			}
			ArrayView[Int](r_edge_raw_data($self),start,1,end-start)
		}
		//take in array view, filter it down to just nodes at a level down
    	infix ("level_neighbors") ( (ArrayView(MInt),GraphCollection(MInt),MInt) :: GraphCollection(MInt)) implements composite ${
			$1.filter{ e => $2(e)==$3 }
		}
		
		/*
		infix ("up_neighbors") ( (ArrayView(MInt),MInt) :: ) implements composite ${
			val id = $1()
			//-1 implies no neighbors
			var start = r_node_apply($self,id)
			var end = array_length(r_edge_raw_data($self))
			if( (id+1) < array_length(r_node_raw_data($self)) ) {	
				end = r_node_apply($self,(id+1))
			}
			if(start == -1 || end == -1){
				start = 0
				end = 0
			}
			ArrayView[Int](r_edge_raw_data($self),start,1,end-start)
		}
		*/
		infix ("sum") ((ArrayView(MInt),NodeData(R),MInt==>MBoolean) :: R, TFractional(R), addTpePars=R) implements composite ${
			$1.mapreduce[R]( e => $2(e), (a,b) => a+b, $3)
		}
		infix ("inBFS") ( (Node, ((Node,NodeData(R),GraphCollection(MInt)) ==> R), ((Node,NodeData(R),NodeData(R),GraphCollection(MInt)) ==> R) ) :: NodeData(R), TFractional(R), addTpePars=R, effect=simple) implements composite ${
			val levelArray = GraphCollection[Int]( $self.get_num_nodes())
			val bitMap = AtomicIntArray( $self.get_num_nodes() )
			val nodes = NodeView(node_raw_data($self),$self.get_num_nodes) 
			val sigma = NodeData[R]($self.get_num_nodes())
			val delta = NodeData[R]($self.get_num_nodes())

			levelArray($1()) = 1
			set(bitMap,$1(),1)
			var finished = false
			var level =	1
			while(!finished){
				finished = true
				nodes.foreach{n =>	
					if(levelArray(n) == level){
						println("Node Forward: " + n + " Level: " + level )
						val neighbor = $self.out_neighbors(Node(n))
						neighbor.foreach{nghbr =>
							println("neighbor: " + nghbr )
							if(testAtomic(bitMap,nghbr,0)){
								if(testAndSetAtomic(bitMap,nghbr,0,1)){
									levelArray(nghbr) = level+1
									finished = false
						}}}//end nghbr for each	
						//println("trapped in up neighbors")
						//var up_nghbrs = $self.level_neighbors(neighbor,levelArray,level+1)
						//up_nghbrs.mapreduce()
						//neighbor.mapreduce[R]( e => nd(e), (a,b) => a+b, f => true)

						sigma(n) = $2(Node(n),sigma,levelArray)
						//levelArray.gc_print
						//val ndR = $self.sum(neighbor,nd,levelArray,level)//(levelArray(e)==(level-1))})

						//println("node computation: " + nd(n) + " old comp: " + ndR)
						//up_nghbrs.gc_print()
					}
				}//end nodes for each
				level += 1
			}//end while
			levelArray.gc_print
			println("")
			println("Starting reverse")
			val rBFS = true
			///reverse BFS
			while( level>=1 ){
				nodes.foreach{n =>
					if(levelArray(n) == level){
						//perform computation
						println("Node Reverse: " + n + " Level: " + level )
						delta(n) = $3(Node(n),sigma,delta,levelArray)
					}
				}
				level -= 1
			}
			println("sigma")
			sigma.nd_print
			println("")
			delta
		}
		infix("get_node_from_id")(MInt :: Node) implements composite ${
			//if($1 >= $self.get_num_nodes() || $1 < 0){
			//	throw new RuntimeException("Node ID is not in current graph.  Out of bounds.")
			//}
			Node($1)
		}
		infix("nodes")(Nil :: NodeView) implements composite ${
			NodeView(node_raw_data($self),$self.get_num_nodes) 
		}

		infix ("get_num_nodes")(Nil :: MInt) implements getter(0,"_numNodes")
		compiler ("node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_nodes")
		compiler("node_apply")(MInt :: MInt) implements composite ${array_apply(node_raw_data($self),$1)}
		compiler ("r_node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_rNodes")
		compiler("r_node_apply")(MInt :: MInt) implements composite ${array_apply(r_node_raw_data($self),$1)}
		
		compiler ("get_num_edges")(Nil :: MInt) implements getter(0,"_numEdges")
		compiler ("edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_edges")	
		compiler ("r_edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_rEdges")		
		
    }
  } 
}

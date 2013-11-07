package ppl.dsl.forge
package examples
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait GraphOps{
  this: OptiGraphDSL =>
   
  def importGraphOps() {
	val Node = lookupTpe("Node")
	val Edge = lookupTpe("Edge")
	val GraphCollection = lookupTpe("GraphCollection")
	val ArrayView = lookupTpe("ArrayView")
	val NodeView = lookupTpe("NodeView")


	//////////////////////////////////////////////////////////////////////////////
	// GRAPH DECLARATION
	//////////////////////////////////////////////////////////////////////////////    
    val Graph = tpe("Graph") 
    val A = tpePar("T")
    data(Graph,("_numNodes",MInt),("_nodes",MArray(MInt)),("_numEdges",MInt),("_edges",MArray(MInt))) 

    static(Graph)("apply", Nil, (MArray(MInt),MArray(MInt)) :: Graph, effect=mutable) implements allocates(Graph,${array_length($0)}, ${$0}, ${array_length($1)}, ${$1})
    static (Graph) ("fromFile", Nil, (MString,MString,MString ==> MInt) :: Graph ) implements single ${
      	val nodes = ForgeFileReader.readLines($0)($2)
      	val edges = ForgeFileReader.readLines($1)($2)
      	Graph(nodes,edges)
    }
    
    val GraphOps = withTpe(Graph)     
    GraphOps{
    	infix ("node_neighbors") (MInt :: ArrayView) implements composite ${
			val id = $1
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
		infix ("inBFS") ((Node,Node)::MInt) implements composite ${
			val levelArray = GraphCollection[Int]( $self.get_num_nodes())
			val bitMap = AtomicIntArray( $self.get_num_nodes() )
			val nodes = NodeView(node_raw_data($self),$self.get_num_nodes) 
			gc_update(levelArray,$1(),1)
			testAndSetAtomic(bitMap,$1(),0,1)

			var finished = false
			var level =	1 
			while(!finished){
				finished = true
				nodes.foreach{n =>	
					if( levelArray(n) == level){
						val neighbor = $self.node_neighbors(n)
						neighbor.foreach{nghbr =>
							if(testAtomic(bitMap,nghbr,0)){
								if(testAndSetAtomic(bitMap,nghbr,0,1)){
									gc_update(levelArray,nghbr,level+1)
									finished = false 
						}}}//end nghbr for each	
					}}//end nodes for each
				level = level + 1
			}//end while	
			levelArray.pprint
			return unit(1)
		}
		
		compiler ("node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_nodes")
		compiler ("node_set_raw_data") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_nodes", quotedArg(1))
		compiler("node_update")( (("id",MInt),("n",MInt)) :: MUnit, effect=write(0)) implements single ${
			array_update(node_raw_data($self),$id,$n)
		}
		compiler("node_apply")(MInt :: MInt) implements composite ${array_apply(node_raw_data($self),$1)}

		infix ("get_num_nodes")(Nil :: MInt) implements getter(0,"_numNodes")
		compiler ("set_num_nodes")(MInt :: MUnit, effect = write(0)) implements setter(0, "_numNodes",${$1})
		
		compiler ("edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_edges")
		compiler ("edge_set_raw_data") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_edges", quotedArg(1))
		
		compiler ("get_num_edges")(Nil :: MInt) implements getter(0,"_numEdges")
		compiler ("set_num_edges")(MInt :: MUnit, effect = write(0)) implements setter(0, "_numEdges",${$1})
    }
  } 
}

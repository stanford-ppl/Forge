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
    val Tuple2 = lookupTpe("Tup2")

    //////////////////////////////////////////////////////////////////////////////
    // GRAPH DECLARATION
    //////////////////////////////////////////////////////////////////////////////    
    val Graph = tpe("Graph") 
    val Elem = tpePar("Elem")
    val T = tpePar("T")
    val R = tpePar("R")

    data(Graph,("_directed",MBoolean),("_numNodes",MInt),("_inputIDs",MArray(MInt)),("_outNodes",MArray(MInt)),("_outEdges",MArray(MInt)),("_inNodes",MArray(MInt)),("_inEdges",MArray(MInt))) 
    static(Graph)("apply", Nil, (MethodSignature(List( ("directed",MBoolean),("exID",MArray(MInt)),("outNodes",MArray(MInt)),("outEdges",MArray(MInt)),("inNodes",MArray(MInt)),("inEdges",MArray(MInt)) )  , Graph) ) ) implements allocates(Graph,${$directed},${array_length($exID)}, ${$exID}, ${$outNodes}, ${outEdges},${$inNodes},${$inEdges})
   
    direct(Graph) ("sum", R, (ArrayView(MInt), MInt==>R ,MInt==>MBoolean) :: R, TFractional(R)) implements composite ${
            $0.mapreduce[R]( e => $1(e), (a,b) => a+b, $2)
    }

    
    val GraphOps = withTpe(Graph)     
    GraphOps{
        infix ("is_directed") (Nil :: MBoolean) implements getter(0,"_directed") 
        
        infix ("out_neighbors") (Node :: ArrayView(MInt)) implements composite ${
            val id = $1.id
            //-1 implies no neighbors
            var start = out_node_apply($self,id)
            var end = array_length(out_edge_raw_data($self))
            if( (id+1) < array_length(out_node_raw_data($self)) ) { 
                end = out_node_apply($self,(id+1))
            }
            if(start == -1 || end == -1){
                start = 0
                end = 0
            }
            ArrayView[Int](out_edge_raw_data($self),start,1,end-start)
        }
           
        infix ("in_neighbors") (Node :: ArrayView(MInt)) implements composite ${
            val id = $1.id
            //-1 implies no neighbors
            var start = in_node_apply($self,id)
            var end = array_length(in_edge_raw_data($self))
            if( (id+1) < array_length(in_node_raw_data($self)) ) {   
                end = in_node_apply($self,(id+1))
            }
            if(start == -1 || end == -1){
                start = 0
                end = 0
            }
            ArrayView[Int](in_edge_raw_data($self),start,1,end-start)
        }
      
        //take in array view, filter it down to just nodes at a level down
        infix ("level_neighbors") ( (ArrayView(MInt),GraphCollection(MInt),MInt) :: GraphCollection(MInt)) implements composite ${
            $1.filter{ e => $2(e)==$3 }
        }
  
        infix ("inBFS") ( (Node, ((Node,NodeData(R),GraphCollection(MInt)) ==> R), ((Node,NodeData(R),NodeData(R),GraphCollection(MInt)) ==> R) ) :: NodeData(R), TFractional(R), addTpePars=R, effect=simple) implements composite ${
            val levelArray = GraphCollection[Int]($self.get_num_nodes())
            val bitMap = AtomicIntArray($self.get_num_nodes())
            val nodes = NodeView(input_id_raw_data($self),$self.get_num_nodes) 
            val sigma = NodeData[R]($self.get_num_nodes())
            val delta = NodeData[R]($self.get_num_nodes())

            levelArray($1.id) = 1
            set(bitMap,$1.id,1)
            var finished = false
            var level = 1
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
                        sigma(n) = $2(Node(n),sigma,levelArray)
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
            //  throw new RuntimeException("Node ID is not in current graph.  Out of bounds.")
            //}
            //FIXME: We need to throw in some sort of hash map structure here.
            Node($1)
        }

        infix("nodes")( ( ((NodeData(R),NodeData(R))==>NodeData(R)),(Node==>NodeData(R))) :: NodeData(R), TNumeric(R), addTpePars=R,effect=simple) implements composite ${
          val ndes = NodeView(input_id_raw_data($self),$self.get_num_nodes)
          var bc = NodeData[R]($self.get_num_nodes())
          ndes.foreach{n =>
                  bc = $1(bc,$2(Node(n)))
          }
          bc
        }

        infix ("get_num_nodes")(Nil :: MInt) implements getter(0,"_numNodes")
        compiler ("input_id_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_inputIDs")
        compiler("input_id_apply")(MInt :: MInt) implements composite ${array_apply(input_id_raw_data($self),$1)}
        
        compiler ("out_node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_outNodes")
        compiler("out_node_apply")(MInt :: MInt) implements composite ${array_apply(out_node_raw_data($self),$1)}
        compiler ("out_edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_outEdges")
        compiler("out_edge_apply")(MInt :: MInt) implements composite ${array_apply(out_edge_raw_data($self),$1)}

        compiler ("in_node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_inNodes")
        compiler("in_node_apply")(MInt :: MInt) implements composite ${array_apply(in_node_raw_data($self),$1)}
        compiler ("in_edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_inEdges")
        compiler("in_edge_apply")(MInt :: MInt) implements composite ${array_apply(in_edge_raw_data($self),$1)}
    }
    static (Graph) ("fromFile", Nil, MString :: Graph) implements composite ${
        val input_edges = ForgeFileReader.readLines($0)({line =>
             val fields = line.fsplit("\t")
             pack(fields(0).toInt,fields(1).toInt)
          })
        //contains the input tuples
        val edge_data = NodeData(input_edges)
        /////////////////////////////////////////////////////////////
        //first figure out how many nodes we have and grab them
        val nodes = NodeData[Int](edge_data.nd_length*2)
        var node_count = 0
        edge_data.forloop{ ed =>
          var found_tup1 = false
          var found_tup2 = false
          nodes.forloop{ n =>
            if(n==ed._1){
              found_tup1 = true
            }
            if(n==ed._2){
              found_tup2 = true
            }
          }
          if(!found_tup1){
            nodes(node_count) = ed._1
            node_count += 1
          }
          if(!found_tup2){
            nodes(node_count) = ed._2
            node_count += 1
          }
        }
        nodes.resize(node_count)
        println("Node input ID's")
        nodes.nd_print
        //////////////////////////////////////////////////////////////

        val src = getGroupInput(nodes,edge_data.nd_length,{nde => edge_data.filter({w => nde==w._1}, {e => e._2})})
        println("printing src node array")
        (src._1).nd_print 
        println("printing src edge array")
        (src._2).nd_print
        //////////////////////////////////////////////////////////
        val dst = getGroupInput(nodes,edge_data.nd_length,{nde => edge_data.filter({w => nde==w._2}, {e => e._1})})
        println("printing dst node array")
        (dst._1).nd_print 
        println("printing dst edge array")
        (dst._2).nd_print
        Graph(true,nodes.get_raw_data,(src._1).get_raw_data,(src._2).get_raw_data,(dst._1).get_raw_data,(dst._2).get_raw_data)
    }
    direct (Graph) ("getGroupInput", Nil, (NodeData(MInt),MInt,( MInt ==>NodeData(MInt) ) ) :: Tuple2(NodeData(MInt),NodeData(MInt)) ) implements composite ${
      var first_node = true
      var node_place = 0
      var edge_place = 0
      val node_array = NodeData[Int]($0.nd_length)
      val edge_array = NodeData[Int]($1)
      val visited_nodes = NodeData[Int]($0.nd_length)

      $0.forloop{ n =>
        println("node " + n)
        //loop through and see if we already processed this node
        var seen = false
        visited_nodes.forloop{ vs =>
          if(vs==n){
            seen = true
          }
        }
        if(!seen){
          val tmp = $2(n)
          println("tmp data")
          tmp.nd_print
          if(first_node){
            if(tmp.nd_length!=0){
              first_node = false
              node_array(node_place) = 0
              node_array(node_place+1) = tmp.nd_length
            }
          }
          else if( (tmp.nd_length==0)  &&  ((node_place+1)!=$0.nd_length) ){
            node_array(node_place+1) = node_array(node_place)
          }
          else if ((node_place+1)!=$0.nd_length){
            node_array(node_place+1) = node_array(node_place) + tmp.nd_length
          }
          tmp.forloop{ edge =>
            var i = 0
            var done = false
            while(!done){
              if(edge==$0(i)){
                done = true
              }
              else{i += 1}
            }
            edge_array(edge_place) = i
            edge_place += 1
          }
        
          visited_nodes(node_place) = n
          node_place += 1
        }
      }
      pack(node_array,edge_array)
    }

  } 
}

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

    data(Graph,("_directed",MBoolean),("_numNodes",MInt),("_inputIDs",MHashMap(MInt,MInt)),("_outNodes",MArray(MInt)),("_outEdges",MArray(MInt)),("_inNodes",MArray(MInt)),("_inEdges",MArray(MInt))) 
    static(Graph)("apply", Nil, (MethodSignature(List( ("directed",MBoolean),("count",MInt),("exID",MHashMap(MInt,MInt)),("outNodes",MArray(MInt)),("outEdges",MArray(MInt)),("inNodes",MArray(MInt)),("inEdges",MArray(MInt)) )  , Graph) ) ) implements allocates(Graph,${$directed},${count}, ${$exID}, ${$outNodes}, ${outEdges},${$inNodes},${$inEdges})
   
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

            println("Starting BFS on: " + internal_id_hash($self,$1.id) )
            levelArray($1.id) = 1
            set(bitMap,$1.id,1)
            var finished = AtomicBoolean(false)
            var level = 1
            while(!getAndSet(finished,true)){
                nodes.foreach{n =>  
                    if(levelArray(n) == level){
                        //println("n: " + internal_id_hash($self,$1.id)  + " Node Forward: " + internal_id_hash($self,n) + " Level: " + level )
                        val neighbor = $self.out_neighbors(Node(n))
                        neighbor.foreach{nghbr =>
                            //println("neighbor: " + internal_id_hash($self,nghbr))
                            if(testAtomic(bitMap,nghbr,0)){
                                if(testAndSetAtomic(bitMap,nghbr,0,1)){
                                    levelArray(nghbr) = level+1
                                    set(finished,false)
                        }}}//end nghbr for each 
                        sigma(n) = $2(Node(n),sigma,levelArray)
                    }
                }//end nodes for each
                level += 1
            }//end while
            //println("sigma")
            //sigma.nd_print
            //println("level")
            //levelArray.nd_print
            //println("")
            //println("Starting reverse")
            val rBFS = true
            ///reverse BFS
            while( level>=1 ){
                nodes.foreach{n =>
                    if(levelArray(n) == level){
                        //perform computation
                        //println("Node Reverse: " + internal_id_hash($self,n) + " Level: " + level )
                        delta(n) = $3(Node(n),sigma,delta,levelArray)
                    }
                }
                level -= 1
            }
            println("bfs finished")
            //sigma.nd_print
            //println("")
            delta
        }
     
        infix("get_node_from_id")(MInt :: Node) implements composite ${
            //if($1 >= $self.get_num_nodes() || $1 < 0){
            //  throw new RuntimeException("Node ID is not in current graph.  Out of bounds.")
            //}
            //FIXME: We need to throw in some sort of hash map structure here.
            Node(node_id_hash($self,$1))
        }

        infix("nodes")( ( ((NodeData(R),NodeData(R))==>NodeData(R)),(Node==>NodeData(R))) :: NodeData(R), TNumeric(R), addTpePars=R,effect=simple) implements composite ${
          val ndes = NodeView(input_id_raw_data($self),$self.get_num_nodes)
          var bc_real = NodeData[NodeData[R]]($self.get_num_nodes())

          ndes.foreach{n =>
            bc_real(n) = $2(Node(n))
          }
          bc_real.reduceND( ((a,b) => a.zip(b)),NodeData[R](0))
        }

        infix ("get_num_nodes")(Nil :: MInt) implements getter(0,"_numNodes")

        compiler ("input_id_hash_data") (Nil :: MHashMap(MInt,MInt)) implements getter(0, "_inputIDs")
        compiler ("input_id_raw_data") (Nil :: MArray(MInt)) implements composite ${
          (input_id_hash_data($self)).keys
        }
        compiler("node_id_hash")(MInt :: MInt) implements composite ${
          val elems = input_id_hash_data($self)
          elems($1)
        }
        compiler("internal_id_hash")(MInt :: MInt) implements composite ${
          val elems = input_id_hash_data($self)
          val key_array = input_id_raw_data($self)
          //why can't i do this? FIX Performance hit here
          //val pair = elems.find((A:MInt,B:MInt) => B==$1)
          //just doing sequentially for now need to fix
          var done = false
          var i = 0
          while(!done){
            if(elems(key_array(i))==$1){
              done = true
            }
            else{
              i += 1
            }
          }
          key_array(i)
        }
        
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
        val elems = FHashMap[Int,Int]()

        val src_buckets = NodeData[NodeData[Int]](edge_data.nd_length*2)
        val dst_buckets = NodeData[NodeData[Int]](edge_data.nd_length*2)
        var node_count = 0
        edge_data.forloop{ ed =>
          if(!elems.contains(ed._1)){
            elems(ed._1) = node_count
            src_buckets(node_count) = NodeData[Int](0)
            dst_buckets(node_count) = NodeData[Int](0)
            node_count += 1
          }
          if(!elems.contains(ed._2)){
            elems(ed._2) = node_count
            src_buckets(node_count) = NodeData[Int](0)
            dst_buckets(node_count) = NodeData[Int](0)
            node_count += 1
          }
          src_buckets(elems(ed._1)).append(ed._2)
          dst_buckets(elems(ed._2)).append(ed._1)
        }
        src_buckets.resize(node_count)
        dst_buckets.resize(node_count)

        var node_place = 0
        var src_edge_place = 0
        val src_node_array = NodeData[Int](node_count+1)
        val src_edge_array = NodeData[Int](edge_data.nd_length)

        var dst_edge_place = 0
        val dst_node_array = NodeData[Int](node_count+1)
        val dst_edge_array = NodeData[Int](edge_data.nd_length)
        //loops over all node ID's in hash map
        while(node_place < node_count){
          //////////////
          val src_tmp = src_buckets(node_place).map({e => elems(e)})
          src_node_array(node_place+1) = (src_node_array(node_place) + src_tmp.nd_length)
          src_tmp.forloop{ edge =>
            src_edge_array(src_edge_place) = edge
            src_edge_place += 1
          }
          //
          //Forge error?  it seems to never create a symbol for dst_tmp here which is annoying
          val dst_tmp = dst_buckets(node_place).map({e => elems(e)})
          dst_node_array(node_place+1) = dst_buckets(node_place).map({e => elems(e)}).nd_length + dst_node_array(node_place)
          dst_buckets(node_place).map({e => elems(e)}).forloop{ edge =>
            dst_edge_array(dst_edge_place) = edge
            dst_edge_place += 1
          }  
          ////////////////
          node_place += 1
        }
        src_node_array.resize(node_count)
        dst_node_array.resize(node_count)

        println("finished file I/O")
        Graph(true,node_count,elems,src_node_array.get_raw_data,src_edge_array.get_raw_data,dst_node_array.get_raw_data,dst_edge_array.get_raw_data)
    }

  } 
}

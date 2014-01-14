/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all graph operations.  Glues 
togther all structures and declares graph operations visible
to user.

Data is stored as follows.  Internal ID #'s map to external ID's
in the hashmap that is stored.  Internal ID's are 0 to # of nodes
so that data can be mapped in an array effeciently.  No restrictions
on external ID"s except they cannot be 0.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait GraphOps{
  this: OptiGraphDSL =>

  def importGraphOps() {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeIdView = lookupTpe("NodeIdView")

    //Actual graph declaration
    val Graph = tpe("Graph") 
    val T = tpePar("T")
    val R = tpePar("R")

    data(Graph,("_directed",MBoolean),("_numNodes",MInt),("_IDhash",MHashMap(MInt,MInt)),("_outNodes",MArray(MInt)),("_outEdges",MArray(MInt)),("_inNodes",MArray(MInt)),("_inEdges",MArray(MInt))) 
    static(Graph)("apply", Nil, (MethodSignature(List( ("directed",MBoolean),("count",MInt),("exID",MHashMap(MInt,MInt)),("outNodes",MArray(MInt)),("outEdges",MArray(MInt)),("inNodes",MArray(MInt)),("inEdges",MArray(MInt)) )  , Graph) ) ) implements allocates(Graph,${$directed},${count}, ${$exID}, ${$outNodes}, ${outEdges},${$inNodes},${$inEdges})

    val GraphOps = withTpe(Graph)     
    GraphOps{
      //graph directed or not?
      infix ("isDirected") (Nil :: MBoolean) implements getter(0,"_directed") 
      //given an ID return a node
      infix("getNodeFromID")(MInt :: Node) implements composite ${
          val internalID = getInternalID($self,$1)
          if(internalID >= $self.numNodes() || internalID < 0) fatal("ERROR. ID: " + $1 + " does not exist in this graph!")
          Node(internalID)
      }
      infix ("numNodes")(Nil :: MInt) implements getter(0,"_numNodes")

      //overloaded this method for pagerank, gets funky when you have NodeData(NodeData) like above
      infix("nodes")( (Node==>R) :: NodeData(R), addTpePars=R,effect=simple) implements composite ${
        val ndes = NodeIdView(getHashMapKeys($self),$self.numNodes)
        var node_comp = NodeData[R]($self.numNodes())
        ndes.foreach{n =>
          node_comp(n) = $1(Node(n))
        }
        node_comp
      }

      //If i do just up neighbors I can't use a view and it will be more expensive
      //cannot perform a filter on a view class for some reason
      //I see good reason to not split this up here
      infix ("sumUpNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        val inNbrs = $self.inNbrs(n)
        //only sum the outNeighbors a level up
        sum(inNbrs)(data){e => level(e)==(level(n.id)-1)}
      }
      //FIXME: hardcoded in not to sum the root
      infix ("sumDownNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        val outNbrs = $self.outNbrs(n)
        //only sum the outNeighbors a level up
        sum(outNbrs)(data){e => (level(e)==(level(n.id)+1))}
      }
      infix ("outDegree") (Node :: MInt) implements single ${
        val id = $1.id
        var end = array_length(out_edge_raw_data($self))
        var start = out_node_apply($self,id)
        if( (id+1) < array_length(out_node_raw_data($self)) ) {
          end = out_node_apply($self,(id+1))
        }
        end - start 
      }
      infix ("inDegree") (Node :: MInt) implements single ${
        val id = $1.id
        var end = array_length(in_edge_raw_data($self))
        var start = in_node_apply($self,id)
        if( (id+1) < array_length(in_node_raw_data($self)) ) {
          end = in_node_apply($self,(id+1))
        }
        end - start 
      }
      //get out neighbors
      infix ("outNbrs") (Node :: NodeDataView(MInt)) implements composite ${
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
          NodeDataView[Int](out_edge_raw_data($self),start,1,end-start)
      }
      
      //get in neighbors   
      infix ("inNbrs") (Node :: NodeDataView(MInt)) implements composite ${
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
          NodeDataView[Int](in_edge_raw_data($self),start,1,end-start)
      }

      //perform BF traversal
      infix ("inBFOrder") ( CurriedMethodSignature(List(Node,((Node,NodeData(R),NodeData(MInt)) ==> R),((Node,NodeData(R),NodeData(R),NodeData(MInt)) ==> R)),NodeData(R)), TFractional(R), addTpePars=R, effect=simple) implements composite ${
        val levelArray = NodeData[Int]($self.numNodes)
        val bitMap = AtomicIntArray($self.numNodes)
        val nodes = NodeIdView(getHashMapKeys($self),$self.numNodes) 
        val forwardComp = NodeData[R]($self.numNodes)
        val reverseComp = NodeData[R]($self.numNodes)

        levelArray($1.id) = 1
        set(bitMap,$1.id,1)
        var finished = AtomicBoolean(false)
        var level = 1

        while(!getAndSet(finished,true)){
            nodes.foreach{n =>  
                if(levelArray(n) == level){
                  val neighbor = $self.outNbrs(Node(n))
                  neighbor.foreach{nghbr =>
                      if(testAtomic(bitMap,nghbr,0)){
                          if(testAndSetAtomic(bitMap,nghbr,0,1)){
                              levelArray(nghbr) = level+1
                              set(finished,false)
                  }}}//end nghbr for each 
                  forwardComp(n) = $2(Node(n),forwardComp,levelArray)
                }
            }//end nodes for each
            level += 1
        }//end while
        val rBFS = true
        ///reverse BFS
        while( level>=1 ){
            nodes.foreach{n =>
                if(levelArray(n) == level){
                    reverseComp(n) = $3(Node(n),forwardComp,reverseComp,levelArray)
                }
            }
            level -= 1
        }
        reverseComp
      }

      compiler ("getIDHashMap") (Nil :: MHashMap(MInt,MInt)) implements getter(0, "_IDhash")
      //sorts it by internal place, essentially reverses the hashmap
      infix ("getOrderedNodeIDs") (Nil :: MArray(MInt)) implements composite ${
        var i = 0
        val ordered_ids = NodeData[Int]($self.numNodes)
        val keys = getHashMapKeys($self)
        val hash = getIDHashMap($self)
        while(i < $self.numNodes){
          ordered_ids(hash(keys(i))) = keys(i)
          i += 1
        }
        ordered_ids.getRawDataArray
      }
      //gets the hash map stored
      compiler ("getHashMapKeys") (Nil :: MArray(MInt)) implements composite ${
        fhashmap_keys[Int,Int](getIDHashMap($self))
      }
      //normal hash
      compiler("getInternalID")(MInt :: MInt) implements composite ${
        val elems = getIDHashMap($self)
        elems($1)
      }
      //only needed for debug purposes
      compiler("getExternalID")(MInt :: MInt) implements composite ${
        val elems = getIDHashMap($self)
        val key_array = getHashMapKeys($self)
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
    //math_object_abs only works for a type of Double
    direct(Graph) ("abs", Nil, MDouble :: MDouble) implements single ${math_object_abs($0)}
    direct(Graph) ("abs", Nil, NodeData(MDouble) :: NodeData(MDouble)) implements single ${$0.map(e => math_object_abs(e))}

    //a couple of sum methods
    direct(Graph) ("sum", R, NodeData(R) :: R, TNumeric(R)) implements single ${$0.reduce((a,b) => a+b)}
    direct(Graph) ("sum", R, CurriedMethodSignature(List(("nd_view",NodeDataView(MInt)), ("data",MInt==>R) ,("cond",MInt==>MBoolean,"e => unit(true)")),R), TNumeric(R)) implements single ${nd_view.mapreduce[R]( e => data(e), (a,b) => a+b, cond)}
    
    direct(Graph) ("sum", R, NodeData(NodeData(R)) :: NodeData(R), TFractional(R)) implements composite ${
      //FIXME: HACK
      //this does not work in library but we knew that.
      //val result = $0.reduceND( ((a,b) => a.zip(b)),NodeData[R](0))
      var result = $0(0)
      var i = 1
      while(i<$0.length){
        result = result+($0(i))
        i += 1
      }
      result
    }
    // "block" should not mutate the input, but always produce a new copy. in this version, block can change the structure of the input across iterations (e.g. increase its size)
    direct (Graph) ("untilconverged", T, CurriedMethodSignature(List(List(("x", T), ("tol", MDouble, ".001"), ("minIter", MInt, "1"), ("maxIter", MInt, "1000")), ("block", T ==> T), ("diff", (T,T) ==> MDouble)), T)) implements composite ${
      var delta = scala.Double.MaxValue
      var cur = x
      var iter = 0

      while ((math_object_abs(delta) > tol && iter < maxIter) || iter < minIter) {
        val prev = cur
        val next = block(cur)
        iter += 1
        delta = diff(prev,next)
        cur = next
      }

      if (iter == maxIter){
        println("Maximum iterations exceeded")
      }

      cur
    }

  } 
}

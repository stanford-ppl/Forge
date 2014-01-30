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

    data(Graph,("_directed",MBoolean),("_numNodes",MInt),("_externalIDs",MArray(MInt)),("_outNodes",MArray(MInt)),("_outEdges",MArray(MInt)),("_inNodes",MArray(MInt)),("_inEdges",MArray(MInt))) 
    static(Graph)("apply", Nil, (MethodSignature(List( ("directed",MBoolean),("count",MInt),("exID",MArray(MInt)),("outNodes",MArray(MInt)),("outEdges",MArray(MInt)),("inNodes",MArray(MInt)),("inEdges",MArray(MInt)) )  , Graph) ) ) implements allocates(Graph,${$directed},${count}, ${$exID}, ${$outNodes}, ${outEdges},${$inNodes},${$inEdges})

    val GraphOps = withTpe(Graph)     
    GraphOps{
      //graph directed or not?
      infix ("isDirected") (Nil :: MBoolean) implements getter(0,"_directed") 
      //given an ID return a node
      infix("getNodeFromID")(MInt :: Node) implements composite ${
        val exIDs = NodeData($self.getExternalIDs)
        var i = 0
        var result = -1
        while( i < $self.numNodes ){
          if( exIDs(i) ==  $1 ){
            result = i
            i = $self.numNodes
          } 
          else i += 1
        }
        if(result >= $self.numNodes() || result < 0) fatal("ERROR. ID: " + $1 + " does not exist in this graph!")
        Node(result)
      }
      infix ("numNodes")(Nil :: MInt) implements getter(0,"_numNodes")

      //overloaded this method for pagerank, gets funky when you have NodeData(NodeData) like above
      infix("nodes")( (Node==>R) :: NodeData(R), addTpePars=R) implements composite ${
        NodeData[R](array_map[Int,R](array_fromfunction($self.numNodes,{n => n}), {n => $1(Node(n))}))
      }

      //If i do just up neighbors I can't use a view and it will be more expensive
      //cannot perform a filter on a view class for some reason
      //I see good reason to not split this up here
      infix ("sumUpNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        //only sum in neighbors a level up
        sum($self.inNbrs(n))(data){e => level(e)==(level(n.id)-1)}
      }
      //FIXME: hardcoded in not to sum the root
      infix ("sumDownNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        //only sum the outNeighbors a level down
        sum($self.outNbrs(n))(data){e => (level(e)==(level(n.id)+1))}
      }
      infix ("outDegree") (Node :: MInt) implements single ${
        val end  = if( ($1.id+1) < array_length(out_node_raw_data($self)) ) out_node_apply($self,($1.id+1)) else array_length(out_edge_raw_data($self))
        end - out_node_apply($self,$1.id) 
      }
      infix ("inDegree") (Node :: MInt) implements single ${
        val end = if( ($1.id+1) < array_length(in_node_raw_data($self)) ) in_node_apply($self,($1.id+1)) else array_length(in_edge_raw_data($self))
        end - in_node_apply($self,$1.id)
      }
      //get out neighbors
      infix ("outNbrs") (Node :: NodeDataView(MInt)) implements single ${
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
      infix ("inNbrs") (Node :: NodeDataView(MInt)) implements single ${
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
        val nodes = NodeIdView($self.getExternalIDs,$self.numNodes) 
        val forwardComp = NodeData[R]($self.numNodes)
        val reverseComp = NodeData[R]($self.numNodes)

        levelArray($1.id) = 1
        set(bitMap,$1.id,1)
        
        //error: illegal sharing of mutable objects Sym(2472 at Sym(2473)=Reflect(NewVar(Sym(2472)),Summary(false,false,false,false,true,false,List(Sym(2472)),List(Sym(2472)),List(),List()),List(Sym(2472)))
        //var finished = AtomicBoolean(false)
        var finished = false

        var level = 1
 
        while(!finished){//!getAndSet(finished,true)){
          finished = true
          nodes.foreach{n =>  
            if(levelArray(n) == level){
              val neighbor = $self.outNbrs(Node(n))
              neighbor.foreach{nghbr =>
                if(testAtomic(bitMap,nghbr,0)){
                  if(testAndSetAtomic(bitMap,nghbr,0,1)){
                    levelArray(nghbr) = level+1
                    finished = false//set(finished,false)
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
        NodeData(reverseComp.getRawArrayBuffer)
      }

      infix ("getExternalIDs") (Nil :: MArray(MInt)) implements getter(0, "_externalIDs")
      
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
    direct(Graph) ("abs", Nil, MFloat :: MFloat) implements single ${if($0 > 0) $0 else $0 * -1}
    direct(Graph) ("abs", Nil, NodeData(MFloat) :: NodeData(MFloat)) implements single ${$0.map(e => abs(e))}

    //a couple of sum methods
    direct(Graph) ("sum", R, NodeData(R) :: R, TNumeric(R)) implements single ${$0.reduce((a,b) => a+b)}
    direct(Graph) ("sum", R, CurriedMethodSignature(List(("nd_view",NodeDataView(MInt)), ("data",MInt==>R) ,("cond",MInt==>MBoolean)),R), TNumeric(R)) implements composite ${nd_view.mapreduce[R]( e => data(e), (a,b) => a+b, cond)}
    direct(Graph) ("sum", R, NodeData(NodeData(R)) :: NodeData(R), TFractional(R)) implements composite ${
      //FIXME: HACK
      //this does not work in library but we knew that.
      //val result = $0.reduceND( ((a,b) => a.+(b)),NodeData[R](0))
      
      var result = $0(0)
      var i = 1
      while(i<$0.length){
        result = result+($0(i))
        i += 1
      }
      result
    }
    // "block" should not mutate the input, but always produce a new copy. in this version, block can change the structure of the input across iterations (e.g. increase its size)
    direct (Graph) ("untilconverged", T, CurriedMethodSignature(List(List(("x", T), ("tol", MDouble, ".0001"), ("minIter", MInt, "1"), ("maxIter", MInt, "100")), ("block", T ==> T), ("diff", (T,T) ==> MDouble)), T)) implements composite ${
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
      println("Number of Iterations: " + iter)
      if (iter == maxIter){
        println("Maximum iterations exceeded")
      }
      cur
    }
  } 
}

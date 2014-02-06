/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all DirectedGraph operations.  Glues 
togther all structures and declares DirectedGraph operations visible
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

trait DirectedGraphOps{
  this: OptiGraphDSL =>

  def importDirectedGraphOps() {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeIdView = lookupTpe("NodeIdView")
    val NodeSHash = lookupTpe("NodeSHash")
    //Actual DirectedGraph declaration
    val DirectedGraph = tpe("DirectedGraph") 
    val T = tpePar("T")
    val R = tpePar("R")

    data(DirectedGraph,("_numNodes",MInt),("_externalIDs",MArray(MInt)),("_outNodes",MArray(MInt)),("_outEdges",MArray(MInt)),("_inNodes",MArray(MInt)),("_inEdges",MArray(MInt))) 
    static(DirectedGraph)("apply", Nil, (MethodSignature(List(("count",MInt),("exID",MArray(MInt)),("outNodes",MArray(MInt)),("outEdges",MArray(MInt)),("inNodes",MArray(MInt)),("inEdges",MArray(MInt))), DirectedGraph))) implements allocates(DirectedGraph,${count}, ${$exID}, ${$outNodes}, ${outEdges},${$inNodes},${$inEdges})

    val DirectedGraphOps = withTpe(DirectedGraph)     
    DirectedGraphOps{
      infix ("isDirected") (Nil :: MBoolean) implements single ${true}
      infix ("neighbors") (MInt :: NodeSHash(MInt,MInt), effect = simple) implements composite ${$self.neighbors(Node($1))}
      infix ("neighbors") (Node :: NodeSHash(MInt,MInt), effect = simple) implements composite ${
        val hash = NodeSHash[Int,Int]
        $self.inNbrs($1).serialForEach{n => hash.add(n,n)}
        $self.outNbrs($1).serialForEach{n => hash.add(n,n)}
        hash
      }
      //get out neighbors
      infix ("outNbrs") (Node :: NodeDataView(MInt)) implements single ${
        val start = out_node_apply($self,$1.id)
        val end = if( ($1.id+1) < array_length(out_node_raw_data($self)) ) out_node_apply($self,($1.id+1))
          else array_length(out_edge_raw_data($self))
        NodeDataView[Int](out_edge_raw_data($self),start,end-start)
      }
      //get in neighbors   
      infix ("inNbrs") (Node :: NodeDataView(MInt)) implements single ${
        val start = in_node_apply($self,$1.id)
        val end = if( ($1.id+1) < array_length(in_node_raw_data($self)) ) in_node_apply($self,($1.id+1)) 
            else array_length(in_edge_raw_data($self)) 
        NodeDataView[Int](in_edge_raw_data($self),start,end-start)
      }
      infix ("outDegree") (Node :: MInt) implements single ${
        val end  = if( ($1.id+1) < array_length(out_node_raw_data($self)) ) out_node_apply($self,($1.id+1)) 
          else array_length(out_edge_raw_data($self))
        end - out_node_apply($self,$1.id) 
      }
      infix ("inDegree") (Node :: MInt) implements single ${
        val end = if( ($1.id+1) < array_length(in_node_raw_data($self)) ) in_node_apply($self,($1.id+1)) 
            else array_length(in_edge_raw_data($self))
        end - in_node_apply($self,$1.id)
      }
      //If i do just up neighbors I can't use a view and it will be more expensive
      //cannot perform a filter on a view class for some reason
      //I see good reason to not split this up here
      infix ("sumDownNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        //only sum in neighbors a level up
        sum($self.outNbrs(n))(data){e => (level(e)==(level(n.id)+1))}
      }
      infix ("sumUpNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        //only sum in neighbors a level up
        sum($self.inNbrs(n))(data){e => level(e)==(level(n.id)-1)}
      }
      /*
      //Input node ids
      infix ("hasEdge") ((MInt,MInt) :: MBoolean) implements composite ${$self.hasEdge(Node($1),Node($2))}
      infix ("hasEdge") ((Node,Node) :: MBoolean) implements composite ${
        val inNbrs = NodeData($self.inNbrs($1).getRawArray).groupByReduce[Int,Int](e => e, e => e, (a,b) => a)
        val outNbrs = NodeData($self.outNbrs($1).getRawArray).groupByReduce[Int,Int](e => e, e => e, (a,b) => a)
        if(fhashmap_contains[Int,Int](inNbrs,$2.id) || fhashmap_contains[Int,Int](outNbrs,$2.id)) true 
        else false
      }
      */
      //Out Node Accessors
      compiler ("out_node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_outNodes")
      compiler("out_node_apply")(MInt :: MInt) implements single ${array_apply(out_node_raw_data($self),$1)}
      compiler ("out_edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_outEdges")
      compiler("out_edge_apply")(MInt :: MInt) implements single ${array_apply(out_edge_raw_data($self),$1)}

      //In Node Accessors
      compiler ("in_node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_inNodes")
      compiler("in_node_apply")(MInt :: MInt) implements single ${array_apply(in_node_raw_data($self),$1)}
      compiler ("in_edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_inEdges")
      compiler("in_edge_apply")(MInt :: MInt) implements single ${array_apply(in_edge_raw_data($self),$1)}
    }
    addGraphCommonOps(DirectedGraph)
  } 
}

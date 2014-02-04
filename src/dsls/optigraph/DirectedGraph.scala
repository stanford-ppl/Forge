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

    //Actual DirectedGraph declaration
    val DirectedGraph = tpe("DirectedGraph") 
    val T = tpePar("T")
    val R = tpePar("R")

    data(DirectedGraph,("_numNodes",MInt),("_externalIDs",MArray(MInt)),("_outNodes",MArray(MInt)),("_outEdges",MArray(MInt)),("_inNodes",MArray(MInt)),("_inEdges",MArray(MInt))) 
    static(DirectedGraph)("apply", Nil, (MethodSignature(List(("count",MInt),("exID",MArray(MInt)),("outNodes",MArray(MInt)),("outEdges",MArray(MInt)),("inNodes",MArray(MInt)),("inEdges",MArray(MInt))), DirectedGraph))) implements allocates(DirectedGraph,${count}, ${$exID}, ${$outNodes}, ${outEdges},${$inNodes},${$inEdges})

    val DirectedGraphOps = withTpe(DirectedGraph)     
    DirectedGraphOps{
      infix ("isDirected") (Nil :: MBoolean) implements single ${true}
      //If i do just up neighbors I can't use a view and it will be more expensive
      //cannot perform a filter on a view class for some reason
      //I see good reason to not split this up here
      infix ("sumDownNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        //only sum in neighbors a level up
        sum($self.outNbrs(n))(data){e => (level(e)==(level(n.id)+1))}
      }
      infix ("outDegree") (Node :: MInt) implements single ${
        val end  = if( ($1.id+1) < array_length(out_node_raw_data($self)) ) out_node_apply($self,($1.id+1)) 
          else array_length(out_edge_raw_data($self))
        end - out_node_apply($self,$1.id) 
      }
      //get out neighbors
      infix ("outNbrs") (Node :: NodeDataView(MInt)) implements single ${
        val start = out_node_apply($self,$1.id)
        val end = if( ($1.id+1) < array_length(out_node_raw_data($self)) ) out_node_apply($self,($1.id+1))
          else array_length(out_edge_raw_data($self))
        NodeDataView[Int](out_edge_raw_data($self),start,end-start)
      }
      infix ("sumUpNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        //only sum in neighbors a level up
        sum($self.inNbrs(n))(data){e => level(e)==(level(n.id)-1)}
      }
      infix ("inDegree") (Node :: MInt) implements single ${
        val end = if( ($1.id+1) < array_length(in_node_raw_data($self)) ) in_node_apply($self,($1.id+1)) 
            else array_length(in_edge_raw_data($self))
        end - in_node_apply($self,$1.id)
      }
      //get in neighbors   
      infix ("inNbrs") (Node :: NodeDataView(MInt)) implements single ${
        val start = in_node_apply($self,$1.id)
        val end = if( ($1.id+1) < array_length(in_node_raw_data($self)) ) in_node_apply($self,($1.id+1)) 
            else array_length(in_edge_raw_data($self)) 
        NodeDataView[Int](in_edge_raw_data($self),start,end-start)
      }

      infix ("getExternalIDs") (Nil :: MArray(MInt)) implements getter(0, "_externalIDs")
      infix ("getExternalID") (MInt :: MInt) implements single ${array_apply($self.getExternalIDs,$1)}
      
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

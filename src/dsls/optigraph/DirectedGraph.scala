/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all DirectedGraph operations.  Glues 
togther all structures and declares DirectedGraph operations visible
to user.  Inherits graph common ups.

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
    val NeighborView = lookupTpe("NeighborView")
    val NodeIdView = lookupTpe("NodeIdView")
    //Actual DirectedGraph declaration
    val DirectedGraph = tpe("DirectedGraph") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")
    val SHashMap = tpe("scala.collection.mutable.HashMap", (K,V))

    data(DirectedGraph,("_numNodes",MInt),("_externalIDs",MArray(MInt)),("_outNodes",MArray(MInt)),("_outEdges",MArray(MInt)),("_inNodes",MArray(MInt)),("_inEdges",MArray(MInt))) 
    static(DirectedGraph)("apply", Nil, (MethodSignature(List(("numNodes",MInt),("exID",MArray(MInt)),("outNodes",MArray(MInt)),("outEdges",MArray(MInt)),("inNodes",MArray(MInt)),("inEdges",MArray(MInt))), DirectedGraph))) implements allocates(DirectedGraph,${$numNodes},${$exID}, ${$outNodes}, ${outEdges},${$inNodes},${$inEdges})

    val DirectedGraphOps = withTpe(DirectedGraph)     
    DirectedGraphOps{
      infix ("numEdges")(Nil :: MInt) implements composite ${array_length(in_edge_raw_data($self)) + array_length(out_edge_raw_data($self))}
      
      infix ("isDirected") (Nil :: MBoolean) implements composite ${true}

      infix ("outDegree") (Node :: MInt) implements composite ${
        out_node_apply($self,($1.id+1)) - out_node_apply($self,$1.id) 
      }
      infix ("inDegree") (Node :: MInt) implements composite ${
        in_node_apply($self,($1.id+1)) - in_node_apply($self,$1.id)
      }

      infix ("outNeighbors") (Node :: NeighborView(MInt)) implements composite ${
        val start = out_node_apply($self,$1.id)
        val end = out_node_apply($self,($1.id+1))
        NeighborView[Int](out_edge_raw_data($self),start,end-start)
      }
      infix ("inNeighbors") (Node :: NeighborView(MInt)) implements composite ${
        val start = in_node_apply($self,$1.id)
        val end = in_node_apply($self,($1.id+1)) 
        NeighborView[Int](in_edge_raw_data($self),start,end-start)
      }

      infix ("sumDownNeighbors") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",Node==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        //only sum in neighbors a level up
        sumOverNeighborsC($self.outNeighbors(n))(data){e => (level(e.id)==(level(n.id)+1))}
      }
      infix ("sumUpNeighbors") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",Node==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        //only sum in neighbors a level up
        sumOverNeighborsC($self.inNeighbors(n))(data){e => level(e.id)==(level(n.id)-1)}
      }
      //Input node ids
      infix ("hasEdge") ((MInt,MInt) :: MBoolean) implements composite ${$self.hasEdge(Node($1),Node($2))}
      infix ("hasEdge") ((Node,Node) :: MBoolean) implements composite ${
        val inNeighbors = NodeData($self.inNeighbors($1).getRawArray).groupByReduce[Int,Int](e => e, e => e, (a,b) => a)
        val outNeighbors = NodeData($self.outNeighbors($1).getRawArray).groupByReduce[Int,Int](e => e, e => e, (a,b) => a)
        if(fhashmap_contains[Int,Int](inNeighbors,$2.id) || fhashmap_contains[Int,Int](outNeighbors,$2.id)) true 
        else false
      }
      //Out Node Accessors
      compiler ("out_node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_outNodes")
      compiler("out_node_apply")(MInt :: MInt) implements composite ${array_apply(out_node_raw_data($self),$1)}
      compiler ("out_edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_outEdges")
      compiler("out_edge_apply")(MInt :: MInt) implements composite ${array_apply(out_edge_raw_data($self),$1)}

      //In Node Accessors
      compiler ("in_node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_inNodes")
      compiler("in_node_apply")(MInt :: MInt) implements composite ${array_apply(in_node_raw_data($self),$1)}
      compiler ("in_edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_inEdges")
      compiler("in_edge_apply")(MInt :: MInt) implements composite ${array_apply(in_edge_raw_data($self),$1)}
    }
    addGraphCommonOps(DirectedGraph)
  } 
}

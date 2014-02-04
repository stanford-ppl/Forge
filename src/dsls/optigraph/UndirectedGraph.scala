/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all UndirectedGraph operations.  Glues 
togther all structures and declares UndirectedGraph operations visible
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

trait UndirectedGraphOps{
  this: OptiGraphDSL =>

  def importUndirectedGraphOps() {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeIdView = lookupTpe("NodeIdView")

    //Actual UndirectedGraph declaration
    val UndirectedGraph = tpe("UndirectedGraph") 
    val T = tpePar("T")
    val R = tpePar("R")

    data(UndirectedGraph,("_numNodes",MInt),("_externalIDs",MArray(MInt)),("_nodes",MArray(MInt)),("_edges",MArray(MInt))) 
    static(UndirectedGraph)("apply", Nil, (MethodSignature(List(("count",MInt),("exID",MArray(MInt)),("outNodes",MArray(MInt)),("outEdges",MArray(MInt))), UndirectedGraph))) implements allocates(UndirectedGraph,${count},${$exID},${$outNodes},${outEdges})

    val UndirectedGraphOps = withTpe(UndirectedGraph)     
    UndirectedGraphOps{
      //UndirectedGraph directed or not?
      infix ("isDirected") (Nil :: MBoolean) implements single ${false}
      infix ("sumDownNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        //only sum in neighbors a level up
        sum($self.outNbrs(n))(data){e => (level(e)==(level(n.id)+1))}
      }
      infix ("sumUpNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        sum($self.inNbrs(n))(data){e => (level(e)==(level(n.id)-1))}
      }
      infix ("outDegree") (Node :: MInt) implements single ${
        val end  = if( ($1.id+1) < array_length(node_raw_data($self)) ) node_apply($self,($1.id+1)) 
          else array_length(edge_raw_data($self))
        end - node_apply($self,$1.id) 
      }
      infix ("inDegree") (Node :: MInt) implements single ${$self.outDegree($1)}
      //get out neighbors
      infix ("outNbrs") (Node :: NodeDataView(MInt)) implements single ${
        val start = node_apply($self,$1.id)
        val end = if( ($1.id+1) < array_length(node_raw_data($self)) ) node_apply($self,($1.id+1))
          else array_length(edge_raw_data($self))
        NodeDataView[Int](edge_raw_data($self),start,end-start)
      }
      //get in neighbors   
      infix ("inNbrs") (Node :: NodeDataView(MInt)) implements single ${$self.outNbrs($1)}
      infix ("neighbors") (Node :: NodeData(MInt)) implements single ${NodeData($self.outNbrs($1).getRawArray)}
      infix ("hasEdge") ((MInt,MInt) :: MBoolean) implements composite ${$self.hasEdge(Node($1),Node($2))}
      infix ("hasEdge") ((Node,Node) :: MBoolean) implements composite ${
        val outNbrs = NodeData($self.outNbrs($1).getRawArray).groupByReduce[Int,Int](e => e, e => e, (a,b) => a)
        if(fhashmap_contains[Int,Int](outNbrs,$2.id)) true else false
      }

      compiler ("node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_nodes")
      compiler("node_apply")(MInt :: MInt) implements single ${array_apply(node_raw_data($self),$1)}
      compiler ("edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_edges")
      compiler("edge_apply")(MInt :: MInt) implements single ${array_apply(edge_raw_data($self),$1)}
    }
    addGraphCommonOps(UndirectedGraph) 
  } 
}

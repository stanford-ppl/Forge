/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all HABUndirectedGraph operations.  Glues 
togther all structures and declares HABUndirectedGraph operations visible
to user. Inherits from Graph.scala

Data is stored the same as in a directed graph but we only store
out edges. In an undirected graph in=out edges.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}
trait HABUndirectedGraphOps{
  this: OptiGraphDSL =>
  def importHABUndirectedGraphOps() {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeIdView = lookupTpe("NodeIdView")
    val GraphBitSet = lookupTpe("GraphBitSet")
    val NodeCollection = lookupTpe("NodeCollection")
    //Actual HABUndirectedGraph declaration
    val HABUndirectedGraph = tpe("HABUndirectedGraph") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")
    val Tuple2 = lookupTpe("Tup2")

    data(HABUndirectedGraph,("_numNodes",MInt),("_numEdges",MInt),("_externalIDs",MArray(MInt)),("_numHash",MInt),("_numArray",MInt),("_numBitSet",MInt),("_nodes",MArray(NodeCollection))) 
    static(HABUndirectedGraph)("apply", Nil, (MethodSignature(List(("numNodes",MInt),("numEdges",MInt),("externalIDs",MArray(MInt)),("numHash",MInt),("numArray",MInt),("numBitSet",MInt),("nodes",MArray(NodeCollection))), HABUndirectedGraph))) implements allocates(HABUndirectedGraph,${$numNodes},${$numEdges},${$externalIDs},${$numHash},${$numArray},${numBitSet},${nodes})

    val HABUndirectedGraphOps = withTpe(HABUndirectedGraph)     
    HABUndirectedGraphOps{
      infix ("isDirected") (Nil :: MBoolean) implements single ${false}
      infix ("numEdges")(Nil :: MInt) implements getter(0,"_numEdges")
      infix ("numArray")(Nil :: MInt) implements getter(0,"_numArray")
      infix ("numHash")(Nil :: MInt) implements getter(0,"_numHash")
      infix ("numBitSet")(Nil :: MInt) implements getter(0,"_numBitSet")

      //needed for graph common ops
      infix ("outNbrs") (Node :: NodeCollection) implements single ${$self.neighbors($1.id)}
      infix ("inNbrs") (Node :: NodeCollection) implements single ${$self.neighbors($1.id)}
      infix ("neighbors") (Node :: NodeCollection) implements single ${$self.neighbors($1.id)} 
      infix ("neighbors") (MInt :: NodeCollection) implements single ${array_apply(hab_get_nodes($self),$1)}

      compiler ("hab_get_nodes") (Nil :: MArray(NodeCollection)) implements getter(0, "_nodes")
    }
    addGraphCommonOps(HABUndirectedGraph)
  } 
}
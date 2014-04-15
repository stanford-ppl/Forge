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
    val HashSet = lookupTpe("HashSet")
    val NodeCollection = lookupTpe("NodeCollection")
    //Actual HABUndirectedGraph declaration
    val HABUndirectedGraph = tpe("HABUndirectedGraph") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")
    val Tuple2 = lookupTpe("Tup2")

    data(HABUndirectedGraph,("_numNodes",MInt),("_numEdges",MInt),("_externalIDs",MArray(MInt)),("_numHash",MInt),("_numCSR",MInt),("_numBitSet",MInt),("_hashNeighbors",MArray(HashSet(MInt))),("_csrNodes",MArray(MInt)),("_csrEdges",MArray(MInt)),("_bsNeighbors",MArray(GraphBitSet))) 
    static(HABUndirectedGraph)("apply", Nil, (MethodSignature(List(("numNodes",MInt),("numEdges",MInt),("externalIDs",MArray(MInt)),("numHash",MInt),("numCSR",MInt),("numBitSet",MInt),("hashNeighbors",MArray(HashSet(MInt))),("csrNodes",MArray(MInt)),("csrEdges",MArray(MInt)),("bsNeighbors",MArray(GraphBitSet))), HABUndirectedGraph))) implements allocates(HABUndirectedGraph,${numNodes},${numEdges},${externalIDs},${numHash},${numCSR},${numBitSet},${hashNeighbors},${csrNodes},${csrEdges},${bsNeighbors})

    val HABUndirectedGraphOps = withTpe(HABUndirectedGraph)     
    HABUndirectedGraphOps{
      infix ("isDirected") (Nil :: MBoolean) implements single ${false}
      infix ("numEdges")(Nil :: MInt) implements getter(0,"_numEdges")
      infix ("numCSR")(Nil :: MInt) implements getter(0,"_numCSR")
      infix ("numHash")(Nil :: MInt) implements getter(0,"_numHash")
      infix ("numBitSet")(Nil :: MInt) implements getter(0,"_numBitSet")
    
      //needed for graph common ops
      infix ("outNbrs") (Node :: NodeCollection) implements single ${hab_get_neighbors($self,$1.id)}
      infix ("inNbrs") (Node :: NodeCollection) implements single ${hab_get_neighbors($self,$1.id)}
      infix ("neighbors") (Node :: NodeCollection) implements single ${hab_get_neighbors($self,$1.id)} 
      infix ("neighbors") (MInt :: NodeCollection) implements single ${hab_get_neighbors($self,$1)}
      compiler("hab_get_neighbors")(MInt :: NodeCollection) implements single ${
        if($1 < $self.numHash) NodeCollection(hab_hash_apply($self,$1))
        else if($1 < ($self.numHash+$self.numCSR)){
         NodeCollection(hab_get_csr_nbrs($self,$1-$self.numHash))
        }
        else NodeCollection(hab_bs_apply($self,$1-($self.numHash+$self.numCSR)))
      }
      compiler ("hab_get_csr_nbrs") (MInt :: NodeDataView(MInt)) implements single ${
        val start = hab_node_apply($self,$1)
        val end = if( ($1+1) < array_length(hab_node_raw_data($self)) ) hab_node_apply($self,($1+1))
          else array_length(hab_edge_raw_data($self))
        NodeDataView[Int](hab_edge_raw_data($self),start,end-start)
      }
      compiler ("hab_node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_csrNodes")
      compiler("hab_node_apply")(MInt :: MInt) implements single ${array_apply(hab_node_raw_data($self),$1)}
      compiler ("hab_edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_csrEdges")
      compiler("hab_edge_apply")(MInt :: MInt) implements single ${array_apply(hab_edge_raw_data($self),$1)}
      compiler ("hab_hash_neighbors") (Nil :: MArray(HashSet(MInt))) implements getter(0, "_hashNeighbors")
      compiler("hab_hash_apply")(MInt :: HashSet(MInt)) implements single ${array_apply(hab_hash_neighbors($self),$1)}
      compiler ("hab_bs_neighbors") (Nil :: MArray(GraphBitSet)) implements getter(0, "_bsNeighbors")
      compiler("hab_bs_apply")(MInt :: GraphBitSet) implements single ${array_apply(hab_bs_neighbors($self),$1)}
    }
    addGraphCommonOps(HABUndirectedGraph)
  } 
}
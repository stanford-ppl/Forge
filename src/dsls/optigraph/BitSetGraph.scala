/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all BitSetGraph operations.  Glues 
togther all structures and declares BitSetGraph operations visible
to user. Inherits from Graph.scala

Data is stored the same as in a directed graph but we only store
out edges. In an undirected graph in=out edges.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}
trait BitSetGraphOps{
  this: OptiGraphDSL =>
  def importBitSetGraphOps() {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeIdView = lookupTpe("NodeIdView")
    val ParBitSet = lookupTpe("ParBitSet")
    //Actual BitSetGraph declaration
    val BitSetGraph = tpe("BitSetGraph") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")
    val Tuple2 = lookupTpe("Tup2")

    data(BitSetGraph,("_numNodes",MInt),("_numEdges",MInt),("_externalIDs",MArray(MInt)),("_numHeavy",MInt),("_heavyNodes",MArray(ParBitSet)),("_numLight",MInt),("_nodes",MArray(MInt)),("_edges",MArray(MInt))) 
    static(BitSetGraph)("apply", Nil, (MethodSignature(List(("numNodes",MInt),("numEdges",MInt),("externalIDs",MArray(MInt)),("numHeavy",MInt),("heavyNodes",MArray(ParBitSet)),("numLight",MInt),("nodes",MArray(MInt)),("edges",MArray(MInt))), BitSetGraph))) implements allocates(BitSetGraph,${$numNodes},${$numEdges},${$externalIDs},${$numHeavy},${$heavyNodes},${numLight},${nodes},${edges})

    val BitSetGraphOps = withTpe(BitSetGraph)     
    BitSetGraphOps{
      //BitSetGraph directed or not?
      infix ("numEdges")(Nil :: MInt) implements getter(0,"_numEdges")
      infix ("numNodes")(Nil :: MInt) implements getter(0,"_numNodes")
      infix ("numHeavy")(Nil :: MInt) implements getter(0,"_numHeavy")
      infix ("numLight")(Nil :: MInt) implements getter(0,"_numLight")

      infix ("isDirected") (Nil :: MBoolean) implements single ${false}  
    
      /*
      //Perform a sum over the neighbors
      infix ("sumOverNbrs") ( CurriedMethodSignature(List(("n",Node),("data",MInt==>R),("cond",MInt==>MBoolean)),R), TNumeric(R), addTpePars=R) implements composite ${
        sum($self.neighbors(n))(data)(cond)
      }
            //Perform a sum over the neighbors
      infix ("sumOverNbrs") ( CurriedMethodSignature(List(("n",MInt),("data",MInt==>R),("cond",MInt==>MBoolean)),R), TNumeric(R), addTpePars=R) implements composite ${
        sum($self.neighbors(n))(data)(cond)
      }
      infix ("sumDownNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        //only sum in neighbors a level up
        sum($self.neighbors(n))(data){e => (level(e)==(level(n.id)+1))}
      }
      infix ("sumUpNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        sum($self.inNbrs(n))(data){e => (level(e)==(level(n.id)-1))}
      }
      infix ("outDegree") (Node :: MInt) implements single ${
        val end  = if( ($1.id+1) < array_length(node_raw_data2($self)) ) node_apply2($self,($1.id+1)) 
          else array_length(edge_raw_data2($self))
        end - node_apply2($self,$1.id) 
      }
      infix ("inDegree") (Node :: MInt) implements single ${$self.outDegree($1)}
      //get out neighbors
      infix ("outNbrs") (Node :: NodeDataView(MInt)) implements single ${get_specNbrs($self,$1)} 
      infix ("inNbrs") (Node :: NodeDataView(MInt)) implements single ${get_specNbrs($self,$1)}
      infix ("neighbors") (MInt :: NodeDataView(MInt)) implements single ${get_specNbrs($self,Node($1))}
      infix ("neighbors") (Node :: NodeDataView(MInt)) implements single ${get_specNbrs($self,$1)}
      compiler ("get_specNbrs") (Node :: NodeDataView(MInt)) implements single ${
        val start = light_node_index_apply($self,$1.id)
        val end = if( ($1.id+1) < array_length(light_node_index_raw_data($self)) ) light_node_index_apply($self,($1.id+1))
          else array_length(light_edges_raw_data($self))
        NodeDataView[Int](light_edges_raw_data($self),start,end-start)
      }
      */
      compiler ("getHeavyEdges") (Nil :: MArray(ParBitSet)) implements getter(0, "_heavyNodes")
      compiler ("getExternalIDs") (Nil :: MArray(MInt)) implements getter(0, "_externalIDs")
      compiler ("getExternalID") (MInt :: MInt) implements single ${array_apply(getExternalIDs($self),$1)}
      compiler ("light_node_index_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_nodes")
      compiler("light_node_index_apply")(MInt :: MInt) implements single ${array_apply(light_node_index_raw_data($self),$1)}
      compiler ("light_edges_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_edges")
      compiler("light_edges_apply")(MInt :: MInt) implements single ${array_apply(light_edges_raw_data($self),$1)}
    }
  } 
}
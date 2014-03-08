/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all AOHashSetGraph operations.  Glues 
togther all structures and declares AOHashSetGraph operations visible
to user. Inherits from Graph.scala

Data is stored the same as in a directed graph but we only store
out edges. In an undirected graph in=out edges.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait AOHashSetGraphOps{
  this: OptiGraphDSL =>

  def importAOHashSetGraphOps() {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeIdView = lookupTpe("NodeIdView")
    val NodeSHash = lookupTpe("NodeSHash")

    //Actual AOHashSetGraph declaration
    val AOHashSetGraph = tpe("AOHashSetGraph") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")
    val Tuple2 = lookupTpe("Tup2")
    val SHashMap = tpe("scala.collection.mutable.HashMap", (K,V))

    data(AOHashSetGraph,("_numNodes",MInt),("_numEdges",MInt),("_externalIDs",MArray(MInt)),("_nbrHash",MArray(MHashMap(MInt,MInt)))) 
    static(AOHashSetGraph)("apply", Nil, (MethodSignature(List(("count",MInt),("edges",MInt),("exID",MArray(MInt)),("outNodes",MArray(MHashMap(MInt,MInt)))), AOHashSetGraph))) implements allocates(AOHashSetGraph,${$count},${$edges},${$exID},${$outNodes})

    val AOHashSetGraphOps = withTpe(AOHashSetGraph)     
    AOHashSetGraphOps{
      //AOHashSetGraph directed or not?
      infix ("numEdges")(Nil :: MInt) implements getter(0,"_numEdges")

      infix ("isDirected") (Nil :: MBoolean) implements single ${false}
      
      //pulled from graph common ops
      infix ("numNodes")(Nil :: MInt) implements getter(0,"_numNodes")
      infix("sumOverNodes")( (Node==>R) :: R, TNumeric(R), addTpePars=R) implements composite ${
        NodeIdView($self.numNodes).mapreduce[R]( e => $1(Node(e)), (a,b) => a+b, e => true)
      }
      infix ("intersectSets") (Node :: MInt) implements composite ${
        val nbrs = $self.neighbors($1)
        nbrs.mapreduce[Int]({ nbr => 
          if($1.id > nbr) 0
          else{ 
            val nbrsOfNbrs = $self.neighbors(nbr)
            val nbrHash = node_applyAOHashSet($self,nbr)
            nbrsOfNbrs.mapreduce[Int]({ nbrOfNbr => 
              if(fhashmap_contains(nbrHash,nbrOfNbr)) 1
              else 0
            },(a,b) => a+b, e => true)
          }
        },(a,b) => a+b, e => true)
      }
      
      infix ("neighbors") (MInt :: NodeDataView(MInt)) implements single ${get_nbrsAOHashSet($self,Node($1))}
      infix ("neighbors") (Node :: NodeDataView(MInt)) implements single ${get_nbrsAOHashSet($self,$1)}
      compiler ("get_nbrsAOHashSet") (Node :: NodeDataView(MInt)) implements single ${
        NodeDataView[Int](fhashmap_keys(node_applyAOHashSet($self,$1.id)),0,array_length(fhashmap_keys(node_applyAOHashSet($self,$1.id))))
      }

      compiler ("node_raw_dataAOHashSet") (Nil :: MArray(MHashMap(MInt,MInt))) implements getter(0, "_nbrHash")
      compiler("node_applyAOHashSet")(MInt :: MHashMap(MInt,MInt)) implements single ${array_apply(node_raw_dataAOHashSet($self),$1)}
    }
    //addGraphCommonOps(AOHashSetGraph) 
  } 
}

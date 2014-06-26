/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all UndirectedGraph operations.  Glues 
togther all structures and declares UndirectedGraph operations visible
to user. Inherits from Graph.scala

Data is stored the same as in a directed graph but we only store
out edges. In an undirected graph in=out edges.
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
    val NeighborView = lookupTpe("NeighborView")
    val NodeIdView = lookupTpe("NodeIdView")
    
    //Actual UndirectedGraph declaration
    val UndirectedGraph = tpe("UndirectedGraph") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")
    val Tuple2 = lookupTpe("Tup2")

    data(UndirectedGraph,("_numNodes",MInt),("_externalIDs",MArray(MInt)),("_nodes",MArray(MInt)),("_edges",MArray(MInt)),("_weights",MArray(MDouble)))
    static(UndirectedGraph)("apply", Nil, (MethodSignature(List(("count",MInt),("exID",MArray(MInt)),("outNodes",MArray(MInt)),("outEdges",MArray(MInt))), UndirectedGraph))) implements allocates(UndirectedGraph,${$count},${$exID},${$outNodes},${outEdges},${array_empty[Double](unit(0))})
    static(UndirectedGraph)("apply", Nil, (MethodSignature(List(("count",MInt),("exID",MArray(MInt)),("outNodes",MArray(MInt)),("outEdges",MArray(MInt)),("weights",MArray(MDouble))), UndirectedGraph))) implements allocates(UndirectedGraph,${$count},${$exID},${$outNodes},${outEdges},${weights})

    val UndirectedGraphOps = withTpe(UndirectedGraph)     
    UndirectedGraphOps{
      infix ("numEdges")(Nil :: MInt) implements composite ${array_length($self.getEdges)}

      //UndirectedGraph directed or not?
      infix ("isDirected") (Nil :: MBoolean) implements composite ${false}
      
      infix ("sumOverNbrs") ( CurriedMethodSignature(List(("n",Node),("data",MInt==>R),("cond",MInt==>MBoolean)),R), TNumeric(R), addTpePars=R) implements composite ${
        sumOverCollection($self.neighbors(n))(data)(cond)
      }
      //Perform a sum over the neighbors
      infix ("sumOverNbrs") ( CurriedMethodSignature(List(("n",MInt),("data",MInt==>R),("cond",MInt==>MBoolean)),R), TNumeric(R), addTpePars=R) implements composite ${
        sumOverCollection($self.neighbors(n))(data)(cond)
      }
      infix ("sumDownNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        //only sum in neighbors a level up
        sumOverCollection($self.outNbrs(n))(data){e => (level(e)==(level(n.id)+1))}
      }
      infix ("sumUpNbrs") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",MInt==>R)),R), TFractional(R), addTpePars=R) implements composite ${
        sumOverCollection($self.inNbrs(n))(data){e => (level(e)==(level(n.id)-1))}
      }
      infix ("totalWeight") (Nil :: MDouble) implements composite ${
        array_reduce[Double](edge_weights($self),{(a,b) => a+b},unit(0d))
      }
      infix ("weightedDegree") (MInt :: MDouble) implements composite ${
        get_edge_weights($self,Node($1)).reduce({(a,b)=>a+b})
      }
      infix ("numSelfLoops") (MInt :: MDouble) implements composite ${
        ///it seems like a perf hit to do it this way.
        var degree = 0d
        var i = 0
        val start = node_apply($self,$1)
        val nbrs = $self.neighbors($1)
        while(i < nbrs.length){
          if(nbrs(i) == $1){
            degree = array_apply(edge_weights($self),start+i)
            i = nbrs.length
          }
          i += 1
        }
        degree
      }
      infix ("degree") (Node :: MInt) implements composite ${
        val end  = if( ($1.id+1) < array_length($self.getNodes) ) node_apply($self,($1.id+1)) 
          else array_length($self.getEdges)
        end - node_apply($self,$1.id) 
      }
      infix ("outDegree") (Node :: MInt) implements composite ${
        val end  = if( ($1.id+1) < array_length($self.getNodes) ) node_apply($self,($1.id+1)) 
          else array_length($self.getEdges)
        end - node_apply($self,$1.id) 
      }
      infix ("getNeighborsAndWeights") (Node :: Tuple2(NeighborView(MInt),NeighborView(MDouble))) implements composite ${
        val start = node_apply($self,$1.id)
        val end = if( ($1.id+1) < array_length($self.getNodes) ) node_apply($self,($1.id+1))
          else array_length($self.getEdges)
        pack(NeighborView[Int]($self.getEdges,start,end-start),NeighborView[Double](edge_weights($self),start,end-start))
      }
      infix ("inDegree") (Node :: MInt) implements composite ${$self.outDegree($1)}
      infix ("outNbrs") (Node :: NeighborView(MInt)) implements composite ${get_nbrs($self,$1)} 
      infix ("inNbrs") (Node :: NeighborView(MInt)) implements composite ${get_nbrs($self,$1)}
      infix ("neighbors") (MInt :: NeighborView(MInt)) implements composite ${get_nbrs($self,Node($1))}
      infix ("neighbors") (Node :: NeighborView(MInt)) implements composite ${get_nbrs($self,$1)}
      compiler ("get_nbrs") (Node :: NeighborView(MInt)) implements composite ${
        val start = node_apply($self,$1.id)
        val end = if( ($1.id+1) < array_length($self.getNodes) ) node_apply($self,($1.id+1))
          else array_length($self.getEdges)
        NeighborView[Int]($self.getEdges,start,end-start)
      }
      compiler ("get_edge_weights") (Node :: NeighborView(MDouble)) implements composite ${
        val start = node_apply($self,$1.id)
        val end = if( ($1.id+1) < array_length($self.getNodes) ) node_apply($self,($1.id+1))
          else array_length(edge_weights($self))
        NeighborView[Double](edge_weights($self),start,end-start)
      }
      compiler ("edge_weights") (Nil :: MArray(MDouble)) implements getter(0, "_weights")
      infix ("getNodes") (Nil :: MArray(MInt)) implements getter(0, "_nodes")
      compiler("node_apply")(MInt :: MInt) implements composite ${array_apply($self.getNodes,$1)}
      infix ("getEdges") (Nil :: MArray(MInt)) implements getter(0, "_edges")
      compiler("edge_apply")(MInt :: MInt) implements composite ${array_apply($self.getEdges,$1)}
    }
    addGraphCommonOps(UndirectedGraph) 
  } 
}

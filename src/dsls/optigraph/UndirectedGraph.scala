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
      infix ("numEdges")(Nil :: MInt) implements composite ${array_length($self.getCSREdges)}

      infix ("isDirected") (Nil :: MBoolean) implements composite ${false}

      infix ("degree") (Node :: MInt) implements composite ${ 
        node_apply($self,($1.id+1))  - node_apply($self,$1.id) 
      }
      infix ("outDegree") (Node :: MInt) implements composite ${ $self.degree($1) }
      infix ("inDegree") (Node :: MInt) implements composite ${ $self.degree($1) }

      infix ("neighbors") (Node :: NeighborView(MInt)) implements composite ${
        val start = node_apply($self,$1.id)
        val end = node_apply($self,($1.id+1))
        NeighborView[Int]($self.getCSREdges,start,end-start)
      }
      infix ("outNeighbors") (Node :: NeighborView(MInt)) implements composite ${ $self.neighbors($1) } 
      infix ("inNeighbors") (Node :: NeighborView(MInt)) implements composite ${ $self.neighbors($1) }

      // infix ("commonNeighbors") ((Node,Node) :: MLong) implements composite ${
      //   val neigh1 = $self.neighbors($1)
      //   val neigh2 = $self.neighbors($2)
      //   val max = if($1 > $2) $2.id else $1.id

      //   neigh1.intersectInRange(neigh2,max)
      // }

      infix ("getNeighborsAndWeights") (Node :: Tuple2(NeighborView(MInt),NeighborView(MDouble))) implements composite ${
        val start = node_apply($self,$1.id)
        val end = node_apply($self,($1.id+1))
        pack(NeighborView[Int]($self.getCSREdges,start,end-start),NeighborView[Double](edge_weights($self),start,end-start))
      }

      compiler ("get_edge_weights") (Node :: NeighborView(MDouble)) implements composite ${
        val start = node_apply($self,$1.id)
        val end = node_apply($self,($1.id+1))
        NeighborView[Double](edge_weights($self),start,end-start)
      }

      infix ("sumDownNeighbors") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",Node==>R)),R), TNumeric(R), addTpePars=R) implements composite ${
        //only sum in neighbors a level up
        sumOverNeighborsC($self.outNeighbors(n))(data){e => (level(e.id)==(level(n.id)+1))}
      }

      infix ("sumUpNeighbors") ( CurriedMethodSignature(List(List(("n",Node),("level",NodeData(MInt))),("data",Node==>R)),R), TNumeric(R), addTpePars=R) implements composite ${
        sumOverNeighborsC($self.inNeighbors(n))(data){e => (level(e.id)==(level(n.id)-1))}
      }

      infix ("totalWeight") (Nil :: MDouble) implements composite ${
        array_reduce[Double](edge_weights($self),{(a,b) => a+b},unit(0d))
      }
      infix ("weightedDegree") (Node :: MDouble) implements composite ${
        get_edge_weights($self,$1).reduce({(a,b)=>a+b})
      }
      infix ("numSelfLoops") (Node :: MDouble) implements composite ${
        ///it seems like a perf hit to do it this way.
        var degree = 0d
        var i = 0
        val start = node_apply($self,$1.id)
        val neighbors = $self.neighbors($1)
        while(i < neighbors.length){
          if(neighbors(i) == $1.id){
            degree = array_apply(edge_weights($self),start+i)
            i = neighbors.length
          }
          i += 1
        }
        degree
      }

      infix ("getCSREdgeWeights") (Nil :: MArray(MDouble)) implements getter(0, "_weights")
      compiler ("edge_weights") (Nil :: MArray(MDouble)) implements getter(0, "_weights")
      infix ("getCSRNodes") (Nil :: MArray(MInt)) implements getter(0, "_nodes")
      compiler("node_apply")(MInt :: MInt) implements composite ${array_apply($self.getCSRNodes,$1)}
      infix ("getCSREdges") (Nil :: MArray(MInt)) implements getter(0, "_edges")
      compiler("edge_apply")(MInt :: MInt) implements composite ${array_apply($self.getCSREdges,$1)}
    }

    addGraphCommonOps(UndirectedGraph) 
  } 
}

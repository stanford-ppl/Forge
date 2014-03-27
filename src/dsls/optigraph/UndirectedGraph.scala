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
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeIdView = lookupTpe("NodeIdView")
    val NodeSHash = lookupTpe("NodeSHash")

    //Actual UndirectedGraph declaration
    val UndirectedGraph = tpe("UndirectedGraph") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")
    val Tuple2 = lookupTpe("Tup2")
    val SHashMap = tpe("scala.collection.mutable.HashMap", (K,V))

    data(UndirectedGraph,("_numNodes",MInt),("_heavyNodes",SHashMap(MInt,MInt)),("_edgeHash",MHashMap(Tuple2(MInt,MInt),MInt)),("_externalIDs",MArray(MInt)),("_nodes",MArray(MInt)),("_edges",MArray(MInt))) 
    static(UndirectedGraph)("apply", Nil, (MethodSignature(List(("count",MInt),("shash",SHashMap(MInt,MInt)),("edgeHash",MHashMap(Tuple2(MInt,MInt),MInt)),("exID",MArray(MInt)),("outNodes",MArray(MInt)),("outEdges",MArray(MInt))), UndirectedGraph))) implements allocates(UndirectedGraph,${$count},${$shash},${$edgeHash},${$exID},${$outNodes},${outEdges})

    val UndirectedGraphOps = withTpe(UndirectedGraph)     
    UndirectedGraphOps{
      //UndirectedGraph directed or not?
      infix ("numEdges")(Nil :: MInt) implements single ${array_length(edge_raw_data($self))}

      infix ("isDirected") (Nil :: MBoolean) implements single ${false}
      infix ("hasEdge") ((MInt,MInt) :: MBoolean) implements single ${
        val eHash = get_edge_hash($self)
        fhashmap_contains(eHash,pack($1,$2))
      }
      infix ("hasEdge") ((Node,Node) :: MBoolean) implements single ${
        val eHash = get_edge_hash($self)
        fhashmap_contains(eHash,pack($1.id,$2.id))
      }

      infix ("neighborHash") (Node :: NodeSHash(MInt,MInt), effect = simple) implements composite ${
        val hash = NodeSHash[Int,Int]
        $self.neighbors($1).serialForEach{n => hash.add(n,n)}
        hash
      }
      infix("sumOverEdges")( (Edge==>R) :: R, TNumeric(R), addTpePars=R) implements composite ${
        NodeIdView($self.numNodes).mapreduce[R]( {n => 
          $self.neighbors(n).mapreduce[R]({ nbr =>
              $1(Edge(Node(n),Node(nbr)))
            },(a,b) => a+b, e => true)
        }, (a,b) => a+b, e => true)
      }
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
        val end  = if( ($1.id+1) < array_length(node_raw_data($self)) ) node_apply($self,($1.id+1)) 
          else array_length(edge_raw_data($self))
        end - node_apply($self,$1.id) 
      }
      infix ("inDegree") (Node :: MInt) implements single ${$self.outDegree($1)}
      //get out neighbors
      infix ("outNbrs") (Node :: NodeDataView(MInt)) implements single ${get_nbrs($self,$1)} 
      infix ("inNbrs") (Node :: NodeDataView(MInt)) implements single ${get_nbrs($self,$1)}
      infix ("neighbors") (MInt :: NodeDataView(MInt)) implements single ${get_nbrs($self,Node($1))}
      infix ("neighbors") (Node :: NodeDataView(MInt)) implements single ${get_nbrs($self,$1)}
      compiler ("get_nbrs") (Node :: NodeDataView(MInt)) implements single ${
        val start = node_apply($self,$1.id)
        val end = if( ($1.id+1) < array_length(node_raw_data($self)) ) node_apply($self,($1.id+1))
          else array_length(edge_raw_data($self))
        NodeDataView[Int](edge_raw_data($self),start,end-start)
      }

      compiler ("get_edge_hash") (Nil :: MHashMap(Tuple2(MInt,MInt),MInt)) implements getter(0, "_edgeHash")
      compiler ("node_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_nodes")
      compiler("node_apply")(MInt :: MInt) implements single ${array_apply(node_raw_data($self),$1)}
      compiler ("edge_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_edges")
      compiler("edge_apply")(MInt :: MInt) implements single ${array_apply(edge_raw_data($self),$1)}
    }
    addGraphCommonOps(UndirectedGraph) 
  } 
}

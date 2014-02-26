/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: The main file for all SpecUndirectedGraph operations.  Glues 
togther all structures and declares SpecUndirectedGraph operations visible
to user. Inherits from Graph.scala

Data is stored the same as in a directed graph but we only store
out edges. In an undirected graph in=out edges.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait SpecUndirectedGraphOps{
  this: OptiGraphDSL =>

  def importSpecUndirectedGraphOps() {
    //previously declared types we use
    val Node = lookupTpe("Node")
    val Edge = lookupTpe("Edge")
    val NodeData = lookupTpe("NodeData")
    val NodeDataView = lookupTpe("NodeDataView")
    val NodeIdView = lookupTpe("NodeIdView")
    val NodeSHash = lookupTpe("NodeSHash")

    //Actual SpecUndirectedGraph declaration
    val SpecUndirectedGraph = tpe("SpecUndirectedGraph") 
    val T = tpePar("T")
    val R = tpePar("R")
    val K = tpePar("K")
    val V = tpePar("V")
    val Tuple2 = lookupTpe("Tup2")
    val SHashMap = tpe("scala.collection.mutable.HashMap", (K,V))

    data(SpecUndirectedGraph,("_numNodes",MInt),("_heavyNodes",SHashMap(MInt,MInt)),("_nbrHash",MArray(MHashMap(MInt,MInt))),("_externalIDs",MArray(MInt)),("_nodes",MArray(MInt)),("_edges",MArray(MInt))) 
    static(SpecUndirectedGraph)("apply", Nil, (MethodSignature(List(("count",MInt),("shash",SHashMap(MInt,MInt)),("nbrHash",MArray(MHashMap(MInt,MInt))),("exID",MArray(MInt)),("outNodes",MArray(MInt)),("outEdges",MArray(MInt))), SpecUndirectedGraph))) implements allocates(SpecUndirectedGraph,${$count},${$shash},${$nbrHash},${$exID},${$outNodes},${outEdges})

    val SpecUndirectedGraphOps = withTpe(SpecUndirectedGraph)     
    SpecUndirectedGraphOps{
      //SpecUndirectedGraph directed or not?
      infix ("numEdges")(Nil :: MInt) implements single ${array_length(edge_raw_data2($self))}

      infix ("isDirected") (Nil :: MBoolean) implements single ${false}
      
      infix ("hasEdge") ((MInt,MInt) :: MBoolean) implements single ${
        val eHash = get_nbr_hash($self)
        if($1 < $2)
            fhashmap_contains(eHash($1),$2)
        else
            fhashmap_contains(eHash($2),$1)
      }
      infix ("hasEdge") ((Node,Node) :: MBoolean) implements single ${
        $self.hasEdge($1.id,$2.id)
      }

      infix ("neighborHash") (Node :: NodeSHash(MInt,MInt), effect = simple) implements composite ${
        val hash = NodeSHash[Int,Int]
        $self.neighbors($1).serialForEach{n => hash.add(n,n)}
        hash
      }
      infix ("intersectSets") (Node :: MInt) implements composite ${
        val nbrs = $self.neighbors($1)

        nbrs.mapreduce[Int]({ nbr =>
          //if($1.id > nbr) 0
          //else{
            val nbrsOfNbrs = $self.neighbors(nbr)
            var i = 0
            var j = 0
            var t = 0
            
            while(i < nbrs.length  && j < nbrsOfNbrs.length){
              while(nbrsOfNbrs(j) < nbrs(i)){
                j += 1
              }
              if(nbrs(i)==nbrsOfNbrs(j)){ 
                println("counting: " + $1.id + " nbr: " + nbr + " nbrsOfNbrs: " + nbrsOfNbrs(j))
                t += 1
              }
              i += 1
            }
            t
         // }
        },(a,b) => a+b, e => true)
      }
      infix ("sumTrianglesOverEdges") (Node :: MInt) implements composite ${
        $self.neighbors($1).mapreduce[Int]({ nbr =>
          if($self.neighbors($1).length > $self.neighbors(nbr).length)
            $self.neighbors(nbr).mapreduce[Int]({ nbrOfNbr =>
              if($self.hasEdge($1.id,nbrOfNbr)) 1
              else 0
            },(a,b) => a+b, e => true)
          else 
            $self.neighbors($1).mapreduce[Int]({ nbr2 =>
              if($self.hasEdge(nbr2,nbr)) 1
              else 0
            },(a,b) => a+b, e => true)
        },(a,b) => a+b, e => true)
      }
      infix ("twoLevelHash") (Node :: MInt) implements composite ${
        $self.neighbors($1).mapreduce[Int]({ nbr =>
          $self.neighbors(nbr).mapreduce[Int]({ nbrOfNbr =>
            if($self.hasEdge($1.id,nbrOfNbr)) 1
            else 0
          },(a,b) => a+b, e => true)
        },(a,b) => a+b, e => true)
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
        val start = node_apply2($self,$1.id)
        val end = if( ($1.id+1) < array_length(node_raw_data2($self)) ) node_apply2($self,($1.id+1))
          else array_length(edge_raw_data2($self))
        NodeDataView[Int](edge_raw_data2($self),start,end-start)
      }

      compiler ("get_nbr_hash") (Nil :: MArray(MHashMap(MInt,MInt))) implements getter(0, "_nbrHash")
      compiler ("node_raw_data2") (Nil :: MArray(MInt)) implements getter(0, "_nodes")
      compiler("node_apply2")(MInt :: MInt) implements single ${array_apply(node_raw_data2($self),$1)}
      compiler ("edge_raw_data2") (Nil :: MArray(MInt)) implements getter(0, "_edges")
      compiler("edge_apply2")(MInt :: MInt) implements single ${array_apply(edge_raw_data2($self),$1)}
    }
    addGraphCommonOps(SpecUndirectedGraph) 
  } 
}

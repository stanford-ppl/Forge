/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: Stores data asscoicated with nodes in an array 
buffer indexed by internal node IDs
*///////////////////////////////////////////////////////////////

package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

trait NodeSHashOps {
  this: OptiGraphDSL =>
  def importNodeSHashOps() {
    val T = tpePar("T")
    val K = tpePar("K")
    val V = tpePar("V")
    val SHashMap = tpe("scala.collection.mutable.HashMap", (K,V))

    val NodeData = lookupTpe("NodeData")
    val NodeSHash = tpe("NodeSHash", (K,V)) 
    data(NodeSHash,("_data",SHashMap(K,V)))
    static(NodeSHash)("apply", (K,V), Nil :: NodeSHash(K,V)) implements allocates(NodeSHash,${shashmap_shashmap[K,V]()})

    val NodeSHashOps = withTpe(NodeSHash)
    NodeSHashOps{  
      infix("mapreduce")((K ==> T,(T,T) ==> T, K==>MBoolean) :: T, TNumeric(T), addTpePars=(T)) implements composite ${NodeData(nd_shash_keys($self)).mapreduce($1,$2,$3)}
      infix("hasEdgeWith")(K :: MBoolean) implements composite ${nd_shash($self).contains($1)}
      infix("add")((K,V) :: MUnit, effect = write(0)) implements composite ${nd_shash($self).update($1,$2)}
      infix("print")(Nil :: MUnit, effect = simple) implements composite ${
        val rr = nd_shash_keys($self)
        var i = 0
        while(i < array_length(rr)){
          println("keyArray: " + array_apply(rr,i))
          i+=1
        }
      }

      compiler ("nd_shash") (Nil :: SHashMap(K,V)) implements getter(0, "_data")
      compiler ("nd_shash_keys") (Nil :: MArray(K)) implements composite ${nd_shash($self).keys}
    }
  } 
}
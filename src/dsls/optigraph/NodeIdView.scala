/*//////////////////////////////////////////////////////////////
Author: Christopher R. Aberger

Description: Gets a parallel collection of Node ID's.
By definition this is viewing a collection containing 
0 to NumberOfNodes, which is different from a NodeView as that
views the actual data inside the NodeData collection.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait NodeIdViewOps {
  this: OptiGraphDSL =>

  def importNodeIdViewOps() {
    val NodeData = lookupTpe("NodeData")
    val T = tpePar("T")
    val R = tpePar("R")
    val NodeIdView = tpe("NodeIdView")

    data(NodeIdView, ("_length", MInt))
    static (NodeIdView) ("apply", Nil, MInt :: NodeIdView) implements allocates(NodeIdView, ${$0})

    val NodeIdViewOps = withTpe(NodeIdView)
    NodeIdViewOps {
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("apply") (MInt :: MInt) implements composite ${ $1 }
      
      //Really the only two parallel ops you can have for this structure
      infix ("foreach") ((MInt ==> MUnit) :: MUnit, effect=simple) implements foreach(MInt, 0, ${a => $1(a)})
      infix ("mapreduce") ( (MInt ==> T,(T,T) ==> T, MInt==>MBoolean) :: T, TNumeric(T), addTpePars=(T)) implements mapReduce((MInt,T), 0, ${e => $1(e)}, ${numeric_zero[T]}, ${(a,b) => $2(a,b)}, Some(${c => $3(c)}))
      
      //Debug
      infix ("serialForeach") ((MInt ==> MUnit) :: MUnit, effect = simple) implements single ${
        var i = 0
        while(i < $self.length){
          $1($self(i))
          i += 1
        }
      }
  
      compiler ("NodeIdView_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("NodeIdViews cannot be allocated from a parallel op") }
      compiler ("NodeIdView_illegalupdate") ((MInt, MInt) :: MNothing, effect = simple) implements composite ${ fatal("NodeIdViews cannot be updated") }
      parallelize as ParallelCollection(MInt, lookupOp("NodeIdView_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("NodeIdView_illegalupdate"))
    }
  }
}
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

    data(NodeIdView, ("_data", MArray(MInt)), ("_length", MInt))
    static (NodeIdView) ("apply", Nil, (MArray(MInt),MInt) :: NodeIdView) implements allocates(NodeIdView, ${$0}, ${$1})

    val NodeIdViewOps = withTpe(NodeIdView)
    NodeIdViewOps {
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("apply") (MInt :: MInt) implements composite ${ $1 }
      infix ("foreach") ((MInt ==> MUnit) :: MUnit, effect=simple) implements foreach(MInt, 0, ${a => $1(a)})
      compiler ("NodeIdView_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("NodeIdViews cannot be allocated from a parallel op") }
      compiler ("NodeIdView_illegalupdate") ((MInt, MInt) :: MNothing, effect = simple) implements composite ${ fatal("NodeIdViews cannot be updated") }
      parallelize as ParallelCollection(MInt, lookupOp("NodeIdView_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("NodeIdView_illegalupdate"))
    }
  }
}

package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner,Config}

/*
All this does is allow us to get the Node ID's
Just spits back parallel collection from 0 to NodeArray.length
*/

trait NodeIdViewOps {
  this: OptiGraphDSL =>

  def importNodeIdViewOps() {
    val NodeIdView = tpe("NodeIdView")
    val NodeData = lookupTpe("NodeData")
    val T = tpePar("T")
    val R = tpePar("R")
    // data fields
    data(NodeIdView, ("_data", MArray(MInt)), ("_length", MInt))

    // static methods
    static (NodeIdView) ("apply", Nil, (MArray(MInt),MInt) :: NodeIdView) implements allocates(NodeIdView, ${$0}, ${$1})

    val NodeIdViewOps = withTpe(NodeIdView)
    NodeIdViewOps {
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("apply") (MInt :: MInt) implements composite ${ $1 }

      compiler ("NodeIdView_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("NodeIdViews cannot be allocated from a parallel op") }
      compiler ("NodeIdView_illegalupdate") ((MInt, MInt) :: MNothing, effect = simple) implements composite ${ fatal("NodeIdViews cannot be updated") }

      infix ("foreach") ((MInt ==> MUnit) :: MUnit, effect=simple) implements foreach(MInt, 0, ${a => $1(a)})
      infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(MInt, 0, ${a => println(a)})

      //infix ("map_tuples") ( (MInt ==> R) :: NodeData(R), addTpePars=R) implements map((MInt,R), 0, ${ e => $1(e) }) 

      parallelize as ParallelCollection(MInt, lookupOp("NodeIdView_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("NodeIdView_illegalupdate"))
    }
  }
}
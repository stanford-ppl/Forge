package ppl.dsl.forge
package examples
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner,Config}

/*
All this does is allow us to get the Node ID's
Just spits back parallel collection from 0 to NodeArray.length
*/

trait NodeViewOps {
  this: OptiGraphDSL =>

  def importNodeViewOps() {
    val NodeView = tpe("NodeView")

    // data fields
    data(NodeView, ("_data", MArray(MInt)), ("_length", MInt))

    // static methods
    static (NodeView) ("apply", Nil, (MArray(MInt),MInt) :: NodeView) implements allocates(NodeView, ${$0}, ${$1})

    val NodeViewOps = withTpe(NodeView)
    NodeViewOps {
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("apply") (MInt :: MInt) implements composite ${ $1 }

      compiler ("nodeview_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("NodeViews cannot be allocated from a parallel op") }
      compiler ("nodeview_illegalupdate") ((MInt, MInt) :: MNothing, effect = simple) implements composite ${ fatal("NodeViews cannot be updated") }

      infix ("foreach") ((MInt ==> MUnit) :: MUnit, effect=simple) implements foreach(MInt, 0, ${a => $1(a)})
      infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(MInt, 0, ${a => println(a)})

      parallelize as ParallelCollection(MInt, lookupOp("nodeview_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("nodeview_illegalupdate"))
    }
  }
}
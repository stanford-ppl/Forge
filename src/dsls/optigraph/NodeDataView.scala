package ppl.dsl.forge
package	dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner,Config}
//Differs from a node view because here you actually look at data
//versus just return an index of the data
trait NodeDataViewOps {
  this: OptiGraphDSL =>
  def importNodeDataViewOps() {
    val T = tpePar("T")
    val NodeDataView = tpe("NodeDataView",T)
    val NodeData = lookupTpe("NodeData")

    // data fields
    data(NodeDataView, ("_data", MArray(T)), ("_start", MInt), ("_stride", MInt), ("_length", MInt))

    // static methods
    static (NodeDataView) ("apply", T, (MArray(T), MInt ,MInt, MInt) :: NodeDataView(T)) implements allocates(NodeDataView, ${$0}, ${$1}, ${$2}, ${$3})

    val NodeDataViewOps = withTpe(NodeDataView)
    NodeDataViewOps {
      compiler ("NodeDataView_data") (Nil :: MArray(T)) implements getter(0, "_data")
      infix ("NodeDataView_getRawArray") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("NodeDataView_start") (Nil :: MInt) implements getter(0, "_start")
      compiler ("NodeDataView_stride") (Nil :: MInt) implements getter(0, "_stride")

      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("apply") (MInt :: T) implements composite ${ array_apply(NodeDataView_data($self), NodeDataView_start($self) + $1*NodeDataView_stride($self)) }

      compiler ("NodeDataView_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("NodeDataViews cannot be allocated from a parallel op") }
      compiler ("NodeDataView_illegalupdate") ((MInt, T) :: MNothing, effect = simple) implements composite ${ fatal("NodeDataViews cannot be updated") }

      infix ("foreach") ((T ==> MUnit) :: MUnit, effect = simple) implements foreach(T, 0, ${a => $1(a)})
      infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(T, 0, ${a => println(a)})

      val R = tpePar("R")
      infix ("mapreduce") ( (T ==> R,(R,R) ==> R, T==>MBoolean) :: R, TNumeric(R), addTpePars=(T,R)) implements mapReduce((T,R), 0, ${e => $1(e)}, ${numeric_zero[R]}, ${(a,b) => $2(a,b)}, Some(${c => $3(c)}) )
      //infix ("filter") ((T ==> MBoolean) :: NodeData(T)) implements filter((T,T), 0, ${e => $1(e)}, ${e => e})

      parallelize as ParallelCollection(T, lookupOp("NodeDataView_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("NodeDataView_illegalupdate"))
    }
  }
}

package ppl.dsl.forge
package examples
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait EdgeViewOps {
  this: OptiGraphDSL =>

  def importEdgeViewOps() {
    val EdgeView = tpe("EdgeView")
    val GraphCollection = lookupTpe("GraphCollection")

    // data fields
    data(EdgeView, ("_data", MArray(MInt)), ("_start", MInt), ("_stride", MInt), ("_length", MInt))

    // static methods
    static (EdgeView) ("apply", Nil, (MArray(MInt), MInt ,MInt, MInt) :: EdgeView) implements allocates(EdgeView, ${$0}, ${$1}, ${$2}, ${$3})

    val EdgeViewOps = withTpe(EdgeView)
    EdgeViewOps {
      compiler ("edgeview_data") (Nil :: MArray(MInt)) implements getter(0, "_data")
      infix ("edgeview_getRawArray") (Nil :: MArray(MInt)) implements getter(0, "_data")
      compiler ("edgeview_start") (Nil :: MInt) implements getter(0, "_start")
      compiler ("edgeview_stride") (Nil :: MInt) implements getter(0, "_stride")

      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("apply") (MInt :: MInt) implements composite ${ array_apply(edgeview_data($self), edgeview_start($self) + $1*edgeview_stride($self)) }

      compiler ("edgeview_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("Edgeviews cannot be allocated from a parallel op") }
      compiler ("edgeview_illegalupdate") ((MInt, MInt) :: MNothing, effect = simple) implements composite ${ fatal("Edgeviews cannot be updated") }

      infix ("foreach") ((MInt ==> MUnit) :: MUnit) implements foreach(MInt, 0, ${a => $1(a)})
      infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(MInt, 0, ${a => println(a)})

      parallelize as ParallelCollection(MInt, lookupOp("edgeview_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("edgeview_illegalupdate"))
      
    }

    // allows us to perform operations without converting to a DenseVector first
    //addEdgeCommonOps(EdgeView,MInt)
  }
}
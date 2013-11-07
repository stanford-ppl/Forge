package ppl.dsl.forge
package examples
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait ArrayViewOps {
  this: OptiGraphDSL =>

  def importArrayViewOps() {
    val ArrayView = tpe("ArrayView")

    // data fields
    data(ArrayView, ("_data", MArray(MInt)), ("_start", MInt), ("_stride", MInt), ("_length", MInt))

    // static methods
    static (ArrayView) ("apply", Nil, (MArray(MInt), MInt ,MInt, MInt) :: ArrayView) implements allocates(ArrayView, ${$0}, ${$1}, ${$2}, ${$3})

    val ArrayViewOps = withTpe(ArrayView)
    ArrayViewOps {
      compiler ("arrayview_data") (Nil :: MArray(MInt)) implements getter(0, "_data")
      infix ("arrayview_getRawArray") (Nil :: MArray(MInt)) implements getter(0, "_data")
      compiler ("arrayview_start") (Nil :: MInt) implements getter(0, "_start")
      compiler ("arrayview_stride") (Nil :: MInt) implements getter(0, "_stride")

      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("apply") (MInt :: MInt) implements composite ${ array_apply(arrayview_data($self), arrayview_start($self) + $1*arrayview_stride($self)) }

      compiler ("arrayview_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("Arrayviews cannot be allocated from a parallel op") }
      compiler ("arrayview_illegalupdate") ((MInt, MInt) :: MNothing, effect = simple) implements composite ${ fatal("Arrayviews cannot be updated") }

      infix ("foreach") ((MInt ==> MUnit) :: MUnit) implements foreach(MInt, 0, ${a => $1(a)})
      infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(MInt, 0, ${a => println(a)})

      parallelize as ParallelCollection(MInt, lookupOp("arrayview_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("arrayview_illegalupdate"))
    }
  }
}
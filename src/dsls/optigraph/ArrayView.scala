package ppl.dsl.forge
package	dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait ArrayViewOps {
  this: OptiGraphDSL =>
  def importArrayViewOps() {
    val T = tpePar("T")
    val ArrayView = tpe("ArrayView",T)
    val GraphCollection = lookupTpe("GraphCollection")

    // data fields
    data(ArrayView, ("_data", MArray(T)), ("_start", MInt), ("_stride", MInt), ("_length", MInt))

    // static methods
    static (ArrayView) ("apply", T, (MArray(T), MInt ,MInt, MInt) :: ArrayView(T)) implements allocates(ArrayView, ${$0}, ${$1}, ${$2}, ${$3})

    val ArrayViewOps = withTpe(ArrayView)
    ArrayViewOps {
      compiler ("arrayview_data") (Nil :: MArray(T)) implements getter(0, "_data")
      infix ("arrayview_getRawArray") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("arrayview_start") (Nil :: MInt) implements getter(0, "_start")
      compiler ("arrayview_stride") (Nil :: MInt) implements getter(0, "_stride")

      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("apply") (MInt :: T) implements composite ${ array_apply(arrayview_data($self), arrayview_start($self) + $1*arrayview_stride($self)) }

      compiler ("arrayview_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("Arrayviews cannot be allocated from a parallel op") }
      compiler ("arrayview_illegalupdate") ((MInt, T) :: MNothing, effect = simple) implements composite ${ fatal("Arrayviews cannot be updated") }

      infix ("foreach") ((T ==> MUnit) :: MUnit, effect = simple) implements foreach(T, 0, ${a => $1(a)})
      infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(T, 0, ${a => println(a)})

      val R = tpePar("R")
      infix ("mapreduce") ( (T ==> R,(R,R) ==> R, T==>MBoolean) :: R, TNumeric(R), addTpePars=(T,R)) implements mapReduce((T,R), 0, ${e => $1(e)}, ${numeric_zero[R]}, ${(a,b) => $2(a,b)}, Some(${c => $3(c)}) )
      infix ("filter") ((T ==> MBoolean) :: GraphCollection(T)) implements filter((T,T), 0, ${e => $1(e)}, ${e => e})

      parallelize as ParallelCollection(T, lookupOp("arrayview_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("arrayview_illegalupdate"))
    }
  }
}

package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait SparseRowViewOps {
  this: OptiLADSL =>

  def importSparseRowViewOps() {
    val T = tpePar("T")
    val R = tpePar("R")
    // val DenseVector = lookupTpe("DenseVector")
    // val DenseVectorView = lookupTpe("DenseVectorView")
    // val IndexVector = lookupTpe("IndexVector")
    // val SparseVector = lookupTpe("SparseVector")
    val SparseRowView = lookupTpe("SparseRowView")
    // val SparseMatrix = lookupTpe("SparseMatrix")
    // val Tuple2 = lookupTpe("Tup2")
    // val Tuple6 = lookupTpe("Tup6")

    data(SparseRowView, ("_data", MArray(T)), ("_start", MInt), ("_length", MInt))
    static (SparseRowView) ("apply", T, (MArray(T), MInt, MInt) :: SparseRowView(T)) implements allocates(SparseRowView, ${$0}, ${$1}, ${$2})

    val SparseRowViewOps = withTpe(SparseRowView)
    SparseRowViewOps {
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      infix ("apply") (MInt :: T) implements composite ${ array_apply(SparseRowView_data($self), SparseRowView_start($self) + $1) }
      infix ("reduce") (((T,T) ==> T) :: T, TArith(T)) implements reduce(T, 0, ${implicitly[Arith[T]].empty}, ${ (a,b) => $1(a,b) })
      infix ("mapreduce") ( (T ==> R,(R,R) ==> R, T==>MBoolean) :: R, TArith(R), addTpePars=(T,R)) implements mapReduce((T,R), 0, ${e => $1(e)}, ${implicitly[Arith[R]].empty}, ${(a,b) => $2(a,b)}, Some(${c => $3(c)}) )
      infix ("foreach") ((T ==> MUnit) :: MUnit, effect = simple) implements foreach(T, 0, ${a => $1(a)})
      infix ("serialForEach") ((T ==> MUnit) :: MUnit, effect = simple) implements single ${
        var i = 0
        while(i < $self.length){
          $1($self(i))
          i += 1
        }
      }
      infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(T, 0, ${a => println(a)})
      infix ("getRawArray") (Nil :: MArray(T)) implements composite ${
        val d = array_empty[T]($self.length)
        array_copy(SparseRowView_data($self),SparseRowView_start($self),d,0,$self.length)
        d
      }
      compiler ("SparseRowView_data") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("SparseRowView_start") (Nil :: MInt) implements getter(0, "_start")
      compiler ("SparseRowView_illegalalloc") (MInt :: MNothing, effect = simple) implements composite ${ fatal("SparseRowViews cannot be allocated from a parallel op") }
      compiler ("SparseRowView_illegalupdate") ((MInt, T) :: MNothing, effect = simple) implements composite ${ fatal("SparseRowViews cannot be updated") }
      
      parallelize as ParallelCollection(T, lookupOp("SparseRowView_illegalalloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("SparseRowView_illegalupdate"))
    }
  }
}

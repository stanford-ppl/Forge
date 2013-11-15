package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

/**
 * Defines the Bufferable type class for vectors and matrices.
 * This type class enables a type to be used with untilconvergedBuffered.
 */
trait BufferableOps {
  this: OptiMLDSL =>

  object TBufferable extends TypeClassSignature {
    def name = "Bufferable"
    def prefix = "_buf"
    def wrapper = Some("buftype")
  }

  def importBufferableOps() {
    val T = tpePar("T")

    val Bufferable = tpeClass("Bufferable", TBufferable, T)

    // Bufferable type class interface
    infix (Bufferable) ("mutable", T, T :: T, effect = mutable)
    infix (Bufferable) ("write", T, (T,T) :: MUnit, effect = write(1))
    infix (Bufferable) ("size", T, T :: MInt)

    // OptiLA types
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")

    val DenseVectorBufferable = tpeClassInst("BufferableDenseVector", T, Bufferable(DenseVector(T)))
    infix (DenseVectorBufferable) ("mutable", T, DenseVector(T) :: DenseVector(T), effect = mutable) implements composite ${ DenseVector[T]($0.length, $0.isRow) }
    infix (DenseVectorBufferable) ("write", T, (DenseVector(T),DenseVector(T)) :: MUnit, effect = write(1)) implements composite ${
      $0.indices foreach { i => $1(i) = $0(i) }
    }
    infix (DenseVectorBufferable) ("size", T, DenseVector(T) :: MInt) implements composite ${ $0.length }

    val DenseMatrixBufferable = tpeClassInst("BufferableDenseMatrix", T, Bufferable(DenseMatrix(T)))
    infix (DenseMatrixBufferable) ("mutable", T, DenseMatrix(T) :: DenseMatrix(T), effect = mutable) implements composite ${ DenseMatrix[T]($0.numRows, $0.numCols) }
    infix (DenseMatrixBufferable) ("write", T, (DenseMatrix(T),DenseMatrix(T)) :: MUnit, effect = write(1)) implements composite ${
      // can fuse with flat matrix loops
      (unit(0)::$0.size) foreach { i =>
        // need to access the matrix array directly at index i (instead of using normal accessor which computes an offset)
        densematrix_raw_update($1,i,densematrix_raw_apply($0,i))
      }

      // can fuse with nested matrix loops
      // $0.rowIndices foreach { i =>
      //   $0.colIndices foreach { j =>
      //     $1(i,j) = $0(i,j)
      //   }
      // }
    }
    infix (DenseMatrixBufferable) ("size", T, DenseMatrix(T) :: MInt) implements composite ${ $0.size }

    // tuples of bufferables
    for (arity <- 2 until maxTuples) {
      val Tup = lookupTpe("Tup"+arity)
      val pars = (0 until arity).map(i => tpePar(('A'.toInt+i).toChar.toString) withBound TBufferable).toList
      val TupBuf = tpeClassInst("BufferableTup"+arity, pars, Bufferable(Tup))

      val makeTupBufStr = "pack" + (1 to arity).map(i => "t._"+i+".mutable").mkString("((",",","))")
      infix (TupBuf) ("mutable", pars, ("t",Tup) :: Tup, effect = mutable) implements composite ${ \$makeTupBufStr }

      val writeTupBufStr = (1 to arity).map(i => "t1._"+i+".write(t2._"+i+")").mkString("\n")
      infix (TupBuf) ("write", pars, (("t1",Tup),("t2",Tup)) :: MUnit, effect = write(1)) implements composite ${ \$writeTupBufStr }

      // val sizeTupBufStr = (1 to arity).map(i => "t._"+i+".size").mkString("+") // scalac typer crash
      val sizeTupBufStr = (1 to arity).map(i => "bufferable_size(t._"+i+")").mkString("+")
      infix (TupBuf) ("size", pars, ("t",Tup) :: MInt) implements composite ${ \$sizeTupBufStr }
    }

  }
}

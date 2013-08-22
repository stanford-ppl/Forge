package ppl.dsl.forge
package examples
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

    // OptiLA types
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")

    val DenseVectorBufferable = tpeClassInst("BufferableDenseVector", T, Bufferable(DenseVector(T)))
    infix (DenseVectorBufferable) ("mutable", T, DenseVector(T) :: DenseVector(T), effect = mutable) implements composite ${ DenseVector[T]($0.length, $0.isRow) }
    infix (DenseVectorBufferable) ("write", T, (DenseVector(T),DenseVector(T)) :: MUnit, effect = write(1)) implements composite ${
      $0.indices foreach { i => $1(i) = $0(i) }
    }

    val DenseMatrixBufferable = tpeClassInst("BufferableDenseMatrix", T, Bufferable(DenseMatrix(T)))
    infix (DenseMatrixBufferable) ("mutable", T, DenseMatrix(T) :: DenseMatrix(T), effect = mutable) implements composite ${ DenseMatrix[T]($0.numRows, $0.numCols) }
    infix (DenseMatrixBufferable) ("write", T, (DenseMatrix(T),DenseMatrix(T)) :: MUnit, effect = write(1)) implements composite ${
      $0.rowIndices foreach { i =>
        $0.colIndices foreach { j =>
          $1(i,j) = $0(i,j)
        }
      }
    }

    // tuples of bufferables
    for (arity <- 2 until maxTuples) {
      val Tup = lookupTpe("Tup"+arity)
      val pars = (0 until arity).map(i => tpePar(('A'.toInt+i).toChar.toString) withBound TBufferable).toList
      val TupBuf = tpeClassInst("BufferableTup"+arity, pars, Bufferable(Tup))

      val makeTupBufStr = "make_tuple" + arity + (1 to arity).map(i => "t._"+i+".mutable").mkString("((",",","))")
      infix (TupBuf) ("mutable", pars, ("t",Tup) :: Tup, effect = mutable) implements composite ${ \$makeTupBufStr }

      val writeTupBufStr = (1 to arity).map(i => "t1._"+i+".write(t2._"+i+")").mkString("\n")
      infix (TupBuf) ("write", pars, (("t1",Tup),("t2",Tup)) :: MUnit, effect = write(1)) implements composite ${ \$writeTupBufStr }
    }

  }
}

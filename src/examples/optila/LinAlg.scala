package ppl.dsl.forge
package examples
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait LinAlgOps {
  this: OptiLADSL =>

  def importLinAlgOps() {
    val LinAlg = grp("LinAlg")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")

    infix (LinAlg) ("\\", Nil, (DenseMatrix(MDouble),DenseVector(MDouble)) :: DenseVector(MDouble)) implements single ${ fatal("no non-native \\\\ method exists") }
    label(lookupOp("LinAlg","\\"), "linsolve")
  }
}

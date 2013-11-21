package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait LinAlgOps {
  this: OptiLADSL =>

  def importLinAlgOps() {
    val LinAlg = grp("LinAlg")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val T = tpePar("T")

    /* linear system solve */
    infix (LinAlg) ("\\", Nil, (DenseMatrix(MDouble),DenseVector(MDouble)) :: DenseVector(MDouble)) implements single ${ fatal("no non-native \\\\ method exists") }
    label(lookupOp("LinAlg","\\"), "linsolve")

    /* LU factorization */
    direct (LinAlg) ("lu", Nil, DenseMatrix(MDouble) :: CTuple3(DenseMatrix(MDouble),DenseMatrix(MDouble),DenseMatrix(MInt))) implements composite ${ (fatal("no non-native lu method exists"), fatal(""), fatal("")) }

    /* Cholesky factorization */
    direct (LinAlg) ("chol", Nil, MethodSignature(List(("A",DenseMatrix(MDouble)), ("tri",MString,"\"upper\"")),DenseMatrix(MDouble))) implements composite ${ fatal("no non-native chol method exists") }

    /* determinant */
    compiler (LinAlg) ("densematrix_determinant_22", T withBound TNumeric withBound TArith, ("x",DenseMatrix(T)) :: T) implements single ${
      x(0,0)*x(1,1)-x(0,1)*x(1,0)
    }

    compiler (LinAlg) ("densematrix_determinant_33", T withBound TNumeric withBound TArith, ("x",DenseMatrix(T)) :: T) implements single ${
      x(0,0)*x(1,1)*x(2,2) + x(0,1)*x(1,2)*x(2,0) + x(0,2)*x(1,0)*x(2,1) -
      x(0,2)*x(1,1)*x(2,0) - x(0,1)*x(1,0)*x(2,2) - x(0,0)*x(1,2)*x(2,1)
    }

    compiler (LinAlg) ("densematrix_determinant_44", T withBound TNumeric withBound TArith, ("x",DenseMatrix(T)) :: T) implements single ${
      val two = unit(2).AsInstanceOf[T]

      x(0,1)*x(0,1)*x(2,3)*x(2,3)     - x(2,2)*x(3,3)*x(0,1)*x(0,1)     + two*x(3,3)*x(0,1)*x(0,2)*x(1,2) -
      two*x(0,1)*x(0,2)*x(1,3)*x(2,3) - two*x(0,1)*x(0,3)*x(1,2)*x(2,3) + two*x(2,2)*x(0,1)*x(0,3)*x(1,3) +
      x(0,2)*x(0,2)*x(1,3)*x(1,3)     - x(1,1)*x(3,3)*x(0,2)*x(0,2)     - two*x(0,2)*x(0,3)*x(1,2)*x(1,3) +
      two*x(1,1)*x(0,2)*x(0,3)*x(2,3) + x(0,3)*x(0,3)*x(1,2)*x(1,2)     - x(1,1)*x(2,2)*x(0,3)*x(0,3) -
      x(0,0)*x(3,3)*x(1,2)*x(1,2)     + two*x(0,0)*x(1,2)*x(1,3)*x(2,3) - x(0,0)*x(2,2)*x(1,3)*x(1,3) -
      x(0,0)*x(1,1)*x(2,3)*x(2,3)     + x(0,0)*x(1,1)*x(2,2)*x(3,3)
    }

    direct (LinAlg) ("det", T withBound TNumeric withBound TArith, ("x",DenseMatrix(T)) :: T) implements single ${
      if (x.numRows == 2 && x.numCols == 2) densematrix_determinant_22(x)
      else if (x.numRows == 3 && x.numCols == 3) densematrix_determinant_33(x)
      else if (x.numRows == 4 && x.numCols == 4) densematrix_determinant_44(x)
      else {
        // need to use LU decomposition to compute the determinant in general
        fatal("DenseMatrix determinants for matrices > 4x4 is not implemented yet")
      }
    }
  }
}

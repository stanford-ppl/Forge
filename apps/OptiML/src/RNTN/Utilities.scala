import optiml.compiler._
import optiml.library._
import optiml.shared._

trait Utilities extends OptiMLApplication {

	//Prints error message
	def errorOut(message: Rep[String]): Rep[Int] = {
		errorOn(message)
		0
	}

	def errorOn(message: Rep[String]) {
		println(message)
		exit(-1)
	}

	def randperm (n: Rep[Int], k: Rep[Int]): Rep[IndexVector] = { shuffle((0::n)).slice(0, k) }

	// Copied from Matlab's implementation of the softmax activation function
	def softmax (eta: Rep[DenseMatrix[Double]]): Rep[DenseMatrix[Double]] = {
		val tmp = exp(eta)
		val colSums = tmp.sumCols
		(*, 0::tmp.numCols) {col=> tmp.getCol(col) / colSums}
	}

	def softmaxVect (eta: Rep[DenseVector[Double]]): Rep[DenseVector[Double]] = {
		val tmp = exp(eta)
		tmp / tmp.sum
	}

	def verbosePrint(message: Rep[String], verbose: Rep[Boolean]) {
		if (verbose) println(message)
	}

	def setMatrix(m: Rep[DenseMatrix[Double]], n: Rep[DenseMatrix[Double]]) {
		(0::m.numRows) foreach { row =>
	     		(0::m.numCols) foreach { col =>
	        		m(row, col) = n(row, col)
	      		}
	    	}
	} 

 	def pack3D(m: Rep[DenseVector[DenseMatrix[Double]]]): Rep[DenseMatrix[Double]] = {
		val origRows   = m(0).numRows
 		val origZ	   = m.length
 		val packedRows = origZ*origRows
 		(0::packedRows, *) {row => 
 			val z = floor(row/packedRows)
 			val y = row%origRows
 			m(z).getRow(y)
 		}
 	}

 	def unpack3D(m: Rep[DenseMatrix[Double]], zDim: Rep[Int]): Rep[DenseVector[DenseMatrix[Double]]] = {
 		val newNumRows = m.numRows/zDim
 		(0::zDim) {z =>
 			(0::newNumRows, *) {y =>
 				m(z*newNumRows + y).toDense
 			}
 		}
 	}

 	def get3D(m: Rep[DenseMatrix[Double]], yRows: Rep[Int], row: Rep[Int]): Rep[DenseMatrix[Double]] = {
 		m((yRows*row)::(yRows*(row + 1)), *)
 	}
}

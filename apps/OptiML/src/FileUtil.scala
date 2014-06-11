import optiml.compiler._
import optiml.library._
import optiml.shared._

trait FileUtil extends OptiMLApplication {

  /* The input file is expected to follow the format:
   *
   *  <header>
   *  <num documents> <num tokens>
   *  <tokenlist>
   *  <document word matrix, where each row repesents a document and each column a distinct token>
   *    each line of the doc word matrix begins with class (0 or 1) and ends with -1
   *    the matrix is sparse, so each row has a tuple of (tokenIndex, number of appearances)
  */
  def readTokenMatrix(f: Rep[String]) = {
    val lines = readVector[DenseVector[String]](f, words => words)
    val numDocs = lines(1).apply(0).toInt
    val numTokens = lines(1).apply(1).toInt
    val tokens = lines(2)

    val matrix = lines.drop(3)
    val labels = matrix.map(v => v.first.toInt).t
    val data = (0::numDocs, *) { i =>
      val row = matrix(i).slice(1,matrix(i).length-1)
      val tupleIndices = DenseVector.uniform(0,2,row.length).map(_.toInt)
      val cumulIndices = tupleIndices.map(row(_).toInt).prefixSum

      // TODO: need to use a sparse matrix to avoid this mutation efficiently
      val denseRow = DenseVector[Int](numTokens, true)
      (0::tupleIndices.length) foreach { j =>
        denseRow(cumulIndices(j)) = row(tupleIndices(j)+1).toInt
      }
      denseRow.unsafeImmutable
    }
    TrainingSet(data, labels)
  }
}

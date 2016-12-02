import optiml.compiler._
import optiml.library._
import optiml.shared._

import scala.virtualization.lms.common.Record

object IrisClassifyRunner extends OptiMLApplicationCompiler with IrisClassify
trait IrisClassify extends OptiMLApplication {

  lazy val iris_class = DiscreteFeature("Iris-setosa", "Iris-versicolor")

  def mySchema(v: Rep[DenseVector[String]]) =
    Record (
      sepalLength = v(0).toDouble,
      sepalWidth = v(1).toDouble,
      petalLength = v(2).toDouble,
      petalWidth = v(3).toDouble,
      cls = v(4)
    )

  def main() = {
    val in = readARFF(args(0), mySchema)

    val sep0: Rep[Double] = in(0).sepalLength

    println("My first row is: ")
    println("sepalLength: " + in(0).sepalLength + ", sepalWidth: " + in(0).sepalWidth + ", petalLength: " + in(0).petalLength + ", petalWidth: " + in(0).petalWidth  + ", class: " + in(0).cls)

    val data = DenseMatrix(in map { row => DenseVector(row.sepalLength, row.sepalWidth, row.petalLength, row.petalWidth) })
    val labels = in map { row => iris_class(row.cls) == 1.0 } // Iris-setosa is false, Iris-versicolor is true

    val dataNormalized = normalize(data)
    val dataWithIntercept = DenseVector.ones(dataNormalized.numRows).t.toMat <<| dataNormalized
    val trainingSet = DenseTrainingSet(dataWithIntercept, labels)

    println("Running cross validation..")
    val acc = crossValidate[Double,DenseVector[Double],DenseTrainingSet](trainingSet, s => logreg(s, maxIter = 100, verbose = false), (m,set,i) => sigmoid(m *:* set(i)) > 0.5, accuracy, verbose = true)
    println("accuracy: " + acc)

    val auc = crossValidateAUC[Double,DenseVector[Double],DenseTrainingSet](trainingSet, s => logreg(s, maxIter = 100, verbose = false), (m,set,i) => sigmoid(m *:* set(i)))
    println("auc: " + auc)
  }
}

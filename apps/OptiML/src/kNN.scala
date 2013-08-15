import optiml.compiler._
import optiml.library._
import optiml.shared._

object kNNCompiler extends OptiMLApplicationCompiler with kNN
object kNNInterpreter extends OptiMLApplicationInterpreter with kNN

trait kNN extends OptiMLApplication {

  def print_usage = {
    println("Usage: kNN")
    exit(-1)
  }

  def main() = {
    val group = DenseMatrix((1.0, 1.1),(1.0, 1.0),(0.0, 0.0),(0.0, 0.1))
    val labels = DenseVector('A', 'A', 'B', 'B')
    val trainingSet = TrainingSet(group,labels)

    val k = 2
    val inX = DenseVector(0.9,0.9)

    val kIndices = trainingSet.data.mapRowsToVector(row => dist(row, inX)).sortWithIndex._2.take(k)
    val kLabels = trainingSet.labels.apply(kIndices).groupBy(i => i)
    val maxIndex = kLabels.map(_.length).maxIndex
    val maxLabel = kLabels(maxIndex).apply(0) // return label associated with group
    println("label: " + maxLabel)
  }
}

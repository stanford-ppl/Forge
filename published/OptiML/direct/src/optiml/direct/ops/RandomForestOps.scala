package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait RandomForestOps extends Base {
  this: OptiML => 

  implicit def repToRandomForestRandomForestOpsCls(x: Rep[RandomForest])(implicit __pos: SourceContext) = new RandomForestRandomForestOpsCls(x)(__pos)
  implicit def varToRandomForestRandomForestOpsCls(x: Var[RandomForest])(implicit __pos: SourceContext) = new RandomForestRandomForestOpsCls(readVar(x))(__pos)

  class RandomForestRandomForestOpsCls(val self: Rep[RandomForest])(implicit __pos: SourceContext) {
    def trees(implicit __pos: SourceContext) = randomforest_trees(self)(__pos)
    def predict(testPt: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: Overload1) = randomforest_predict(self,testPt)(__pos)
  }



  def randomforest_trees(self: Rep[RandomForest])(implicit __pos: SourceContext): Rep[DenseVector[DecisionTree]]
  def randomforest_predict(self: Rep[RandomForest],testPt: Rep[DenseVector[Double]])(implicit __pos: SourceContext): Rep[Double]
}
trait RandomForestCompilerOps extends RandomForestOps {
  this: OptiML => 

  def alloc_forest(trees: Rep[DenseVector[DecisionTree]])(implicit __pos: SourceContext): Rep[RandomForest]
}


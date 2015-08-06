package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait DecisionTreeOps extends Base {
  this: OptiML => 

  def dtree(trainingSet: Rep[DenseTrainingSet[Double,Double]],maxDepth: Rep[Int] = unit(-1),maxNumFeatures: Rep[Int] = unit(-1),minSamplesSplit: Rep[Int] = unit(2),minSamplesLeaf: Rep[Int] = unit(1),useSamples: Rep[IndexVector] = unit(null.asInstanceOf[IndexVector]),criterion: TCriterion = MSE)(implicit __pos: SourceContext) = decisiontree_dtree(trainingSet,maxDepth,maxNumFeatures,minSamplesSplit,minSamplesLeaf,useSamples,criterion)(__pos)

  implicit def repToDecisionTreeDecisionTreeOpsCls(x: Rep[DecisionTree])(implicit __pos: SourceContext) = new DecisionTreeDecisionTreeOpsCls(x)(__pos)
  implicit def varToDecisionTreeDecisionTreeOpsCls(x: Var[DecisionTree])(implicit __pos: SourceContext) = new DecisionTreeDecisionTreeOpsCls(readVar(x))(__pos)

  class DecisionTreeDecisionTreeOpsCls(val self: Rep[DecisionTree])(implicit __pos: SourceContext) {
    def numNodes(implicit __pos: SourceContext,__imp1: Overload2) = decisiontree_numnodes(self)(__pos)
    def capacity(implicit __pos: SourceContext) = decisiontree_capacity(self)(__pos)
    def isLeaf(implicit __pos: SourceContext) = decisiontree_isleaf(self)(__pos)
    def leftChildren(implicit __pos: SourceContext) = decisiontree_leftchildren(self)(__pos)
    def rightChildren(implicit __pos: SourceContext) = decisiontree_rightchildren(self)(__pos)
    def feature(implicit __pos: SourceContext) = decisiontree_feature(self)(__pos)
    def threshold(implicit __pos: SourceContext) = decisiontree_threshold(self)(__pos)
    def value(implicit __pos: SourceContext) = decisiontree_value(self)(__pos)
    def impurity(implicit __pos: SourceContext) = decisiontree_impurity(self)(__pos)
    def numNodeSamples(implicit __pos: SourceContext) = decisiontree_numnodesamples(self)(__pos)
    def addNode(parent: Rep[Int],isLeft: Rep[Boolean],isLeaf: Rep[Boolean],feature: Rep[Int],threshold: Rep[Double],impurity: Rep[Double],numNodeSamples: Rep[Int])(implicit __pos: SourceContext) = decisiontree_addnode(self,parent,isLeft,isLeaf,feature,threshold,impurity,numNodeSamples)(__pos)
    def predict(testPt: Rep[DenseVector[Double]])(implicit __pos: SourceContext,__imp1: Overload2) = decisiontree_predict(self,testPt)(__pos)
    def pprint()(implicit __pos: SourceContext,__imp1: Overload4) = decisiontree_pprint(self)(__pos)
  }



  def decisiontree_numnodes(self: Rep[DecisionTree])(implicit __pos: SourceContext): Rep[Int]
  def decisiontree_capacity(self: Rep[DecisionTree])(implicit __pos: SourceContext): Rep[Int]
  def decisiontree_isleaf(self: Rep[DecisionTree])(implicit __pos: SourceContext): Rep[ForgeArray[Boolean]]
  def decisiontree_leftchildren(self: Rep[DecisionTree])(implicit __pos: SourceContext): Rep[ForgeArray[Int]]
  def decisiontree_rightchildren(self: Rep[DecisionTree])(implicit __pos: SourceContext): Rep[ForgeArray[Int]]
  def decisiontree_feature(self: Rep[DecisionTree])(implicit __pos: SourceContext): Rep[ForgeArray[Int]]
  def decisiontree_threshold(self: Rep[DecisionTree])(implicit __pos: SourceContext): Rep[ForgeArray[Double]]
  def decisiontree_value(self: Rep[DecisionTree])(implicit __pos: SourceContext): Rep[ForgeArray[Double]]
  def decisiontree_impurity(self: Rep[DecisionTree])(implicit __pos: SourceContext): Rep[ForgeArray[Double]]
  def decisiontree_numnodesamples(self: Rep[DecisionTree])(implicit __pos: SourceContext): Rep[ForgeArray[Int]]
  def decisiontree_addnode(self: Rep[DecisionTree],parent: Rep[Int],isLeft: Rep[Boolean],isLeaf: Rep[Boolean],feature: Rep[Int],threshold: Rep[Double],impurity: Rep[Double],numNodeSamples: Rep[Int])(implicit __pos: SourceContext): Rep[Int]
  def decisiontree_dtree(trainingSet: Rep[DenseTrainingSet[Double,Double]],maxDepth: Rep[Int] = unit(-1),maxNumFeatures: Rep[Int] = unit(-1),minSamplesSplit: Rep[Int] = unit(2),minSamplesLeaf: Rep[Int] = unit(1),useSamples: Rep[IndexVector] = unit(null.asInstanceOf[IndexVector]),criterion: TCriterion = MSE)(implicit __pos: SourceContext): Rep[DecisionTree]
  def decisiontree_predict(tree: Rep[DecisionTree],testPt: Rep[DenseVector[Double]])(implicit __pos: SourceContext): Rep[Double]
  def decisiontree_pprint(tree: Rep[DecisionTree])(implicit __pos: SourceContext): Rep[Unit]
}
trait DecisionTreeCompilerOps extends DecisionTreeOps {
  this: OptiML => 

  def alloc_tree(initCapacity: Rep[Int])(implicit __pos: SourceContext): Rep[DecisionTree]
  def infix_set_num_nodes(self: Rep[DecisionTree],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def infix_set_capacity(self: Rep[DecisionTree],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def infix_set_is_leaf(self: Rep[DecisionTree],__arg1: Rep[ForgeArray[Boolean]])(implicit __pos: SourceContext): Rep[Unit]
  def infix_set_left_children(self: Rep[DecisionTree],__arg1: Rep[ForgeArray[Int]])(implicit __pos: SourceContext): Rep[Unit]
  def infix_set_right_children(self: Rep[DecisionTree],__arg1: Rep[ForgeArray[Int]])(implicit __pos: SourceContext): Rep[Unit]
  def infix_set_feature(self: Rep[DecisionTree],__arg1: Rep[ForgeArray[Int]])(implicit __pos: SourceContext): Rep[Unit]
  def infix_set_threshold(self: Rep[DecisionTree],__arg1: Rep[ForgeArray[Double]])(implicit __pos: SourceContext): Rep[Unit]
  def infix_set_value(self: Rep[DecisionTree],__arg1: Rep[ForgeArray[Double]])(implicit __pos: SourceContext): Rep[Unit]
  def infix_set_impurity(self: Rep[DecisionTree],__arg1: Rep[ForgeArray[Double]])(implicit __pos: SourceContext): Rep[Unit]
  def infix_set_num_node_samples(self: Rep[DecisionTree],__arg1: Rep[ForgeArray[Int]])(implicit __pos: SourceContext): Rep[Unit]
  def tree_realloc(tree: Rep[DecisionTree],minCapacity: Rep[Int])(implicit __pos: SourceContext): Rep[Unit]
  def init_tree(maxDepth: Rep[Int])(implicit __pos: SourceContext): Rep[DecisionTree]
  def tree_split(tree: Rep[DecisionTree],trainingSet: Rep[DenseTrainingSet[Double,Double]],samples: Rep[IndexVector],maxNumFeatures: Rep[Int],impurity: Rep[Double],constantFeatures: Rep[DenseVector[Boolean]],criterion: TCriterion)(implicit __pos: SourceContext): Rep[Tup7[IndexVector,Int,Double,Int,Double,Double,DenseVector[Boolean]]]
  def compute_impurity_improvement(trainingSet: Rep[DenseTrainingSet[Double,Double]],impurity: Rep[Double],samples: Rep[IndexVector],splitPos: Rep[Int],criterion: TCriterion)(implicit __pos: SourceContext): Rep[Tup3[Double,Double,Double]]
  def compute_impurity(trainingSet: Rep[DenseTrainingSet[Double,Double]],samples: Rep[IndexVector],criterion: TCriterion)(implicit __pos: SourceContext): Rep[Double]
  def impurity_mse(trainingSet: Rep[DenseTrainingSet[Double,Double]],samples: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Double]
  def impurity_gini(trainingSet: Rep[DenseTrainingSet[Double,Double]],samples: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Double]
  def tree_score(trainingSet: Rep[DenseTrainingSet[Double,Double]],samples: Rep[IndexVector],criterion: TCriterion)(implicit __pos: SourceContext): Rep[Double]
  def tree_score_mse(trainingSet: Rep[DenseTrainingSet[Double,Double]],samples: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Double]
  def tree_score_gini(trainingSet: Rep[DenseTrainingSet[Double,Double]],samples: Rep[IndexVector])(implicit __pos: SourceContext): Rep[Double]
}


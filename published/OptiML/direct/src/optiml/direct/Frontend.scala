package optiml.direct

import java.io.{BufferedWriter, FileWriter, PrintWriter}
////import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/*
// the trait that all OptiML applications must extend
trait OptiMLApplication extends OptiML with OptiMLLift {
  var args: Rep[Array[String]]
  var stagingArgs: Array[String]
  def main()
}
*/
trait OptiMLApplication extends OptiML

/**
 * dsl definition
 */
trait VarOps
trait Base extends GenOverloadHack {
  class SourceContext
  object SourceContext {
    implicit def sourceContext: SourceContext = new SourceContext
  }

  type Rep[+T]
  trait Typ[T]
  type Var[T]
  def __newVar[T:Typ](init: Rep[T])(implicit pos: SourceContext): Var[T]
  implicit def readVar[T](x: Var[T]): Rep[T]
  def infix_+=[T](lhs: Var[T], rhs: T)(implicit o: ROverload1, mT: Typ[T], pos: SourceContext): Unit
  def infix_+=[T](lhs: Var[T], rhs: Rep[T])(implicit o: ROverload2, mT: Typ[T], pos: SourceContext): Unit
  def infix_+=[T](lhs: Var[T], rhs: Var[T])(implicit o: ROverload3, mT: Typ[T], pos: SourceContext): Unit

  def __ifThenElse[T:Typ](cond: => Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]): Rep[T]

  implicit def typUnit: Typ[Unit]
  implicit def typBool: Typ[Boolean]
  implicit def typShort: Typ[Short]
  implicit def typInt: Typ[Int]
  implicit def typLong: Typ[Long]
  implicit def typFloat: Typ[Float]
  implicit def typDouble: Typ[Double]
  implicit def typString: Typ[String]
  implicit def unit[T:Typ](x:T): Rep[T]
}
/*
trait OptiMLLift
  extends LiftPrimitive with LiftFString with LiftVar {  this: OptiML =>
}


*/
trait OptiMLIdentifiers extends Base with GenOverloadHack {
  /**
   * singleton identifiers
   */
  object * extends IndexWildcard
  object ABS extends DistanceMetric
  object SQUARE extends DistanceMetric
  object EUC extends DistanceMetric
  object L1 extends NormId
  object L2 extends NormId
  object FRO extends NormId
  object Std extends NormalizeMethod
  object Unity extends NormalizeMethod
  object MSE extends TCriterion
  object Gini extends TCriterion

  /**
   * types with no associated data structure
   */
  abstract class IndexWildcard
  implicit def m_IndexWildcard: Typ[IndexWildcard]
  abstract class DistanceMetric
  implicit def m_DistanceMetric: Typ[DistanceMetric]
  abstract class NormId
  implicit def m_NormId: Typ[NormId]
  abstract class NormalizeMethod
  implicit def m_NormalizeMethod: Typ[NormalizeMethod]
  abstract class TCriterion
  implicit def m_TCriterion: Typ[TCriterion]
}

trait OptiML extends OptiMLIdentifiers
 with DDFGFWeightOps with DiscreteFeatureOps with CSRGraphOps with FeatureHelperOps with HasMinMaxOps with FileStreamOps with PrimitiveOps with DenseMatrixOps with MiscOps with FStringOps with BasicMathOps with CSRNgbrOps with IOOps with ValidateOps with FactorGraphOps with ShapeOps with DenseVectorViewOps with RandOps with RangeOps with Tup7Ops with ArithOps with DDFGFEdgeOps with DDFGFVariableOps with CHashMapOps with GrayscaleImageOps with DenseMatrixViewOps with CastOps with ContinuousFeatureOps with SByteBufferOps with Tup8Ops with UTriangleOps with RandomForestOps with ControlOps with SparseVectorViewOps with DecisionTreeOps with OrderingOps with Tup6Ops with SHashMapOps with HashStreamOps with ReplicatedOps with SparseTrainingSetOps with SparseMatrixOps with SparseVectorOps with ComputeStreamOps with Tup9Ops with IndexVectorOps with SparseMatrixBuildableOps with MLioOps with Tup4Ops with DenseTrainingSetOps with LAioOps with TrainingSetLikeOps with DDFGFFactorOps with MathOps with BufferableOps with Tup5Ops with StringableOps with Tup3Ops with Tup2Ops with LinAlgOps with ComplexOps with DenseVectorOps with BinaryFeatureOps with ClassifierOps with ForgeArrayOps with ForgeArrayBufferOps with ForgeHashMapOps with VarOps with LambdaOps with RecordOps with InputOutputOps with ProfilingOps with ReppableOps with TestsOps with AssertsOps with BLASOps with LAPACKOps with RewriteOps with DistributedOps with SumOps {
  this: OptiMLApplication => 

  /**
   * abstract types
   */
  type Tup2[A,B]
  implicit def m_Tup2[A:Typ,B:Typ]: Typ[Tup2[A,B]]
  type Tup3[A,B,C]
  implicit def m_Tup3[A:Typ,B:Typ,C:Typ]: Typ[Tup3[A,B,C]]
  type Tup4[A,B,C,D]
  implicit def m_Tup4[A:Typ,B:Typ,C:Typ,D:Typ]: Typ[Tup4[A,B,C,D]]
  type Tup5[A,B,C,D,E]
  implicit def m_Tup5[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ]: Typ[Tup5[A,B,C,D,E]]
  type Tup6[A,B,C,D,E,F]
  implicit def m_Tup6[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ]: Typ[Tup6[A,B,C,D,E,F]]
  type Tup7[A,B,C,D,E,F,G]
  implicit def m_Tup7[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ]: Typ[Tup7[A,B,C,D,E,F,G]]
  type Tup8[A,B,C,D,E,F,G,H]
  implicit def m_Tup8[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ]: Typ[Tup8[A,B,C,D,E,F,G,H]]
  type Tup9[A,B,C,D,E,F,G,H,I]
  implicit def m_Tup9[A:Typ,B:Typ,C:Typ,D:Typ,E:Typ,F:Typ,G:Typ,H:Typ,I:Typ]: Typ[Tup9[A,B,C,D,E,F,G,H,I]]
  trait DenseVector[T]
  implicit def m_DenseVector[T:Typ]: Typ[DenseVector[T]]
  type DenseVectorView[T]
  implicit def m_DenseVectorView[T:Typ]: Typ[DenseVectorView[T]]
  type DenseMatrix[T]
  implicit def m_DenseMatrix[T:Typ]: Typ[DenseMatrix[T]]
  type DenseMatrixView[T]
  implicit def m_DenseMatrixView[T:Typ]: Typ[DenseMatrixView[T]]
  type IndexVector
  implicit def m_IndexVector: Typ[IndexVector]
  type SparseVector[T]
  implicit def m_SparseVector[T:Typ]: Typ[SparseVector[T]]
  type SparseVectorView[T]
  implicit def m_SparseVectorView[T:Typ]: Typ[SparseVectorView[T]]
  type SparseMatrix[T]
  implicit def m_SparseMatrix[T:Typ]: Typ[SparseMatrix[T]]
  type SparseMatrixBuildable[T]
  implicit def m_SparseMatrixBuildable[T:Typ]: Typ[SparseMatrixBuildable[T]]
  type Range
  implicit def m_Range: Typ[Range]
  type Complex
  implicit def m_Complex: Typ[Complex]
  type UTriangle
  implicit def m_UTriangle: Typ[UTriangle]
  type HashStream[V]
  implicit def m_HashStream[V:Typ]: Typ[HashStream[V]]
  type FileStream
  implicit def m_FileStream: Typ[FileStream]
  type ComputeStream[T]
  implicit def m_ComputeStream[T:Typ]: Typ[ComputeStream[T]]
  type DenseTrainingSet[D,L]
  implicit def m_DenseTrainingSet[D:Typ,L:Typ]: Typ[DenseTrainingSet[D,L]]
  type SparseTrainingSet[D,L]
  implicit def m_SparseTrainingSet[D:Typ,L:Typ]: Typ[SparseTrainingSet[D,L]]
  type ContinuousFeature
  implicit def m_ContinuousFeature: Typ[ContinuousFeature]
  type DiscreteFeature
  implicit def m_DiscreteFeature: Typ[DiscreteFeature]
  type BinaryFeature
  implicit def m_BinaryFeature: Typ[BinaryFeature]
  type CSRGraph
  implicit def m_CSRGraph: Typ[CSRGraph]
  type CSRNgbr
  implicit def m_CSRNgbr: Typ[CSRNgbr]
  type FactorGraph
  implicit def m_FactorGraph: Typ[FactorGraph]
  type DDFGFWeight
  implicit def m_DDFGFWeight: Typ[DDFGFWeight]
  type DDFGFVariable
  implicit def m_DDFGFVariable: Typ[DDFGFVariable]
  type DDFGFFactor
  implicit def m_DDFGFFactor: Typ[DDFGFFactor]
  type DDFGFEdge
  implicit def m_DDFGFEdge: Typ[DDFGFEdge]
  type Replicated[T]
  implicit def m_Replicated[T:Typ]: Typ[Replicated[T]]
  type GrayscaleImage
  implicit def m_GrayscaleImage: Typ[GrayscaleImage]
  type DecisionTree
  implicit def m_DecisionTree: Typ[DecisionTree]
  type RandomForest
  implicit def m_RandomForest: Typ[RandomForest]

  /**
   * hacks for Scala-Virtualized
   */
  def __ifThenElse[T](cond: => Boolean, thenp: => T, elsep: => T): T /* = cond match {
    case true => thenp
    case false => elsep
  }*/

}


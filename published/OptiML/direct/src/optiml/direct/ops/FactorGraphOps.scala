package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait FactorGraphOps extends Base {
  this: OptiML => 

  object FactorGraph {
    def apply(v2f: Rep[CSRGraph],f2v: Rep[CSRGraph],weightValue: Rep[DenseVector[Double]],weightIsFixed: Rep[DenseVector[Boolean]],variableValue: Rep[DenseVector[Boolean]],variableIsEvidence: Rep[DenseVector[Boolean]],factorWeightIdx: Rep[DenseVector[Int]],factorFunction: Rep[DenseVector[Int]],edgeIsPositiveF2V: Rep[DenseVector[Boolean]],nonEvidenceVariables: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload12) = factorgraph_object_apply(v2f,f2v,weightValue,weightIsFixed,variableValue,variableIsEvidence,factorWeightIdx,factorFunction,edgeIsPositiveF2V,nonEvidenceVariables)(__pos)
  }

  implicit def repToFactorGraphFactorGraphOpsCls(x: Rep[FactorGraph])(implicit __pos: SourceContext) = new FactorGraphFactorGraphOpsCls(x)(__pos)
  implicit def varToFactorGraphFactorGraphOpsCls(x: Var[FactorGraph])(implicit __pos: SourceContext) = new FactorGraphFactorGraphOpsCls(readVar(x))(__pos)

  class FactorGraphFactorGraphOpsCls(val self: Rep[FactorGraph])(implicit __pos: SourceContext) {
    def v2f(implicit __pos: SourceContext) = factorgraph_v2f(self)(__pos)
    def f2v(implicit __pos: SourceContext) = factorgraph_f2v(self)(__pos)
    def numVariables(implicit __pos: SourceContext) = factorgraph_numvariables(self)(__pos)
    def numFactors(implicit __pos: SourceContext) = factorgraph_numfactors(self)(__pos)
    def numEdges(implicit __pos: SourceContext,__imp1: Overload2) = factorgraph_numedges(self)(__pos)
    def numWeights(implicit __pos: SourceContext) = factorgraph_numweights(self)(__pos)
    def weightValue(implicit __pos: SourceContext) = factorgraph_weightvalue(self)(__pos)
    def weightIsFixed(implicit __pos: SourceContext) = factorgraph_weightisfixed(self)(__pos)
    def variableValue(implicit __pos: SourceContext) = factorgraph_variablevalue(self)(__pos)
    def variableIsEvidence(implicit __pos: SourceContext) = factorgraph_variableisevidence(self)(__pos)
    def factorWeightIdx(implicit __pos: SourceContext) = factorgraph_factorweightidx(self)(__pos)
    def factorFunction(implicit __pos: SourceContext,__imp1: Overload1) = factorgraph_factorfunction(self)(__pos)
    def edgeIsPositiveF2V(implicit __pos: SourceContext) = factorgraph_edgeispositivef2v(self)(__pos)
    def nonEvidenceVariables(implicit __pos: SourceContext) = factorgraph_nonevidencevariables(self)(__pos)
    def mutable(implicit __pos: SourceContext,__imp1: Overload4) = factorgraph_mutable(self)(__pos)
    def mutableWeights(implicit __pos: SourceContext) = factorgraph_mutableweights(self)(__pos)
    def mutableVariables(implicit __pos: SourceContext) = factorgraph_mutablevariables(self)(__pos)
    def deepcopy(implicit __pos: SourceContext,__imp1: Overload2) = factorgraph_deepcopy(self)(__pos)
  }



  def factorgraph_object_apply(v2f: Rep[CSRGraph],f2v: Rep[CSRGraph],weightValue: Rep[DenseVector[Double]],weightIsFixed: Rep[DenseVector[Boolean]],variableValue: Rep[DenseVector[Boolean]],variableIsEvidence: Rep[DenseVector[Boolean]],factorWeightIdx: Rep[DenseVector[Int]],factorFunction: Rep[DenseVector[Int]],edgeIsPositiveF2V: Rep[DenseVector[Boolean]],nonEvidenceVariables: Rep[DenseVector[Int]])(implicit __pos: SourceContext): Rep[FactorGraph]
  def factorgraph_v2f(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[CSRGraph]
  def factorgraph_f2v(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[CSRGraph]
  def factorgraph_numvariables(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[Int]
  def factorgraph_numfactors(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[Int]
  def factorgraph_numedges(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[Int]
  def factorgraph_numweights(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[Int]
  def factorgraph_weightvalue(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[DenseVector[Double]]
  def factorgraph_weightisfixed(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[DenseVector[Boolean]]
  def factorgraph_variablevalue(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[DenseVector[Boolean]]
  def factorgraph_variableisevidence(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[DenseVector[Boolean]]
  def factorgraph_factorweightidx(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[DenseVector[Int]]
  def factorgraph_factorfunction(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[DenseVector[Int]]
  def factorgraph_edgeispositivef2v(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[DenseVector[Boolean]]
  def factorgraph_nonevidencevariables(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[DenseVector[Int]]
  def factorgraph_mutable(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[FactorGraph]
  def factorgraph_mutableweights(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[FactorGraph]
  def factorgraph_mutablevariables(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[FactorGraph]
  def factorgraph_deepcopy(self: Rep[FactorGraph])(implicit __pos: SourceContext): Rep[FactorGraph]
}

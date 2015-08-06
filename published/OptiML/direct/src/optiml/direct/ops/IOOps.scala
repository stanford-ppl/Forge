package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait IOOps extends Base {
  this: OptiML => 

  def readFactorGraph(factorsPath: Rep[String],variablesPath: Rep[String],weightsPath: Rep[String],edgesPath: Rep[String])(implicit __pos: SourceContext) = io_readfactorgraph(factorsPath,variablesPath,weightsPath,edgesPath)(__pos)


  def io_readfactorgraph(factorsPath: Rep[String],variablesPath: Rep[String],weightsPath: Rep[String],edgesPath: Rep[String])(implicit __pos: SourceContext): Rep[FactorGraph]
}
trait IOCompilerOps extends IOOps {
  this: OptiML => 

  def ddfg_read_weights(path: Rep[String])(implicit __pos: SourceContext): Rep[DenseVector[DDFGFWeight]]
  def ddfg_read_variables(path: Rep[String])(implicit __pos: SourceContext): Rep[DenseVector[DDFGFVariable]]
  def ddfg_read_edges(path: Rep[String])(implicit __pos: SourceContext): Rep[DenseVector[DDFGFEdge]]
  def ddfg_read_factors(path: Rep[String])(implicit __pos: SourceContext): Rep[DenseVector[DDFGFFactor]]
  def ddfg_util_cumsum(__arg0: Rep[DenseVector[Int]])(implicit __pos: SourceContext): Rep[DenseVector[Int]]
}


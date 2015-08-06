package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait CSRGraphOps extends Base {
  this: OptiML => 

  object CSRGraph {
    def apply(numNodes: Rep[Int],numEdges: Rep[Int],nodes: Rep[ForgeArray[Int]],edges: Rep[ForgeArray[Int]])(implicit __pos: SourceContext,__imp1: Overload3) = csrgraph_object_apply(numNodes,numEdges,nodes,edges)(__pos,overload3)
    def apply(nodes: Rep[DenseVector[Int]],edges: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload4) = csrgraph_object_apply(nodes,edges)(__pos,overload4)
  }

  implicit def repToCSRGraphCSRGraphOpsCls(x: Rep[CSRGraph])(implicit __pos: SourceContext) = new CSRGraphCSRGraphOpsCls(x)(__pos)
  implicit def varToCSRGraphCSRGraphOpsCls(x: Var[CSRGraph])(implicit __pos: SourceContext) = new CSRGraphCSRGraphOpsCls(readVar(x))(__pos)

  class CSRGraphCSRGraphOpsCls(val self: Rep[CSRGraph])(implicit __pos: SourceContext) {
    def numNodes(implicit __pos: SourceContext,__imp1: Overload1) = csrgraph_numnodes(self)(__pos)
    def numEdges(implicit __pos: SourceContext,__imp1: Overload1) = csrgraph_numedges(self)(__pos)
    def nodes(implicit __pos: SourceContext) = csrgraph_nodes(self)(__pos)
    def edges(implicit __pos: SourceContext) = csrgraph_edges(self)(__pos)
    def ngbrNodes(__arg1: Rep[Int])(implicit __pos: SourceContext) = csrgraph_ngbrnodes(self,__arg1)(__pos)
    def ngbrEdges(__arg1: Rep[Int])(implicit __pos: SourceContext) = csrgraph_ngbredges(self,__arg1)(__pos)
    def ngbrs(__arg1: Rep[Int])(implicit __pos: SourceContext) = csrgraph_ngbrs(self,__arg1)(__pos)
    def deepcopy(implicit __pos: SourceContext,__imp1: Overload1) = csrgraph_deepcopy(self)(__pos)
  }



  def csrgraph_object_apply(numNodes: Rep[Int],numEdges: Rep[Int],nodes: Rep[ForgeArray[Int]],edges: Rep[ForgeArray[Int]])(implicit __pos: SourceContext,__imp1: Overload3): Rep[CSRGraph]
  def csrgraph_object_apply(nodes: Rep[DenseVector[Int]],edges: Rep[DenseVector[Int]])(implicit __pos: SourceContext,__imp1: Overload4): Rep[CSRGraph]
  def csrgraph_numnodes(self: Rep[CSRGraph])(implicit __pos: SourceContext): Rep[Int]
  def csrgraph_numedges(self: Rep[CSRGraph])(implicit __pos: SourceContext): Rep[Int]
  def csrgraph_nodes(self: Rep[CSRGraph])(implicit __pos: SourceContext): Rep[DenseVectorView[Int]]
  def csrgraph_edges(self: Rep[CSRGraph])(implicit __pos: SourceContext): Rep[DenseVectorView[Int]]
  def csrgraph_ngbrnodes(self: Rep[CSRGraph],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVectorView[Int]]
  def csrgraph_ngbredges(self: Rep[CSRGraph],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[IndexVector]
  def csrgraph_ngbrs(self: Rep[CSRGraph],__arg1: Rep[Int])(implicit __pos: SourceContext): Rep[DenseVector[CSRNgbr]]
  def csrgraph_deepcopy(self: Rep[CSRGraph])(implicit __pos: SourceContext): Rep[CSRGraph]
}
trait CSRGraphCompilerOps extends CSRGraphOps {
  this: OptiML => 

  def csrgraph_get_numnodes(self: Rep[CSRGraph])(implicit __pos: SourceContext): Rep[Int]
  def csrgraph_get_numedges(self: Rep[CSRGraph])(implicit __pos: SourceContext): Rep[Int]
  def csrgraph_get_nodes(self: Rep[CSRGraph])(implicit __pos: SourceContext): Rep[ForgeArray[Int]]
  def csrgraph_get_edges(self: Rep[CSRGraph])(implicit __pos: SourceContext): Rep[ForgeArray[Int]]
}


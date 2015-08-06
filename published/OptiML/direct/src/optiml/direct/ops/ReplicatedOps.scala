package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait ReplicatedOpsBase extends Base {
  this: OptiML => 

  implicit def readLocal[T:Typ](r: Rep[Replicated[T]])(implicit __pos: SourceContext): Rep[T] = { r.local }

}

trait ReplicatedOps extends ReplicatedOpsBase {
  this: OptiML => 

  object Replicated {
    def apply[T:Typ](__arg0: Rep[ForgeArray[T]])(implicit __pos: SourceContext,__imp1: Overload21) = replicated_object_apply[T](__arg0)(implicitly[Typ[T]],__pos)
  }

  implicit def repToReplicatedReplicatedOpsCls[T:Typ](x: Rep[Replicated[T]])(implicit __pos: SourceContext) = new ReplicatedReplicatedOpsCls(x)(implicitly[Typ[T]],__pos)
  implicit def varToReplicatedReplicatedOpsCls[T:Typ](x: Var[Replicated[T]])(implicit __pos: SourceContext) = new ReplicatedReplicatedOpsCls(readVar(x))(implicitly[Typ[T]],__pos)

  class ReplicatedReplicatedOpsCls[T:Typ](val self: Rep[Replicated[T]])(implicit __pos: SourceContext) {
    def local(implicit __pos: SourceContext) = replicated_local[T](self)(implicitly[Typ[T]],__pos)
  }



  def replicated_object_apply[T:Typ](__arg0: Rep[ForgeArray[T]])(implicit __pos: SourceContext): Rep[Replicated[T]]
  def replicated_local[T:Typ](self: Rep[Replicated[T]])(implicit __pos: SourceContext): Rep[T]
}
trait ReplicatedCompilerOps extends ReplicatedOps {
  this: OptiML => 

  def get_copies[T:Typ](self: Rep[Replicated[T]])(implicit __pos: SourceContext): Rep[ForgeArray[T]]
}


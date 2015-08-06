package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait CHashMapOps extends Base {
  this: OptiML => 

  def CHashMap[K:Typ,V:Typ]()(implicit __pos: SourceContext) = chashmap_chashmap[K,V]()(implicitly[Typ[K]],implicitly[Typ[V]],__pos)

  implicit def repToCHashMapjavautilconcurrentConcurrentHashMapOpsCls[K:Typ,V:Typ](x: Rep[java.util.concurrent.ConcurrentHashMap[K,V]])(implicit __pos: SourceContext) = new CHashMapjavautilconcurrentConcurrentHashMapOpsCls(x)(implicitly[Typ[K]],implicitly[Typ[V]],__pos)
  implicit def varToCHashMapjavautilconcurrentConcurrentHashMapOpsCls[K:Typ,V:Typ](x: Var[java.util.concurrent.ConcurrentHashMap[K,V]])(implicit __pos: SourceContext) = new CHashMapjavautilconcurrentConcurrentHashMapOpsCls(readVar(x))(implicitly[Typ[K]],implicitly[Typ[V]],__pos)

  class CHashMapjavautilconcurrentConcurrentHashMapOpsCls[K:Typ,V:Typ](val self: Rep[java.util.concurrent.ConcurrentHashMap[K,V]])(implicit __pos: SourceContext) {
    def apply(__arg1: Rep[K])(implicit __pos: SourceContext,__imp1: Overload12) = chashmap_apply[K,V](self,__arg1)(implicitly[Typ[K]],implicitly[Typ[V]],__pos)
    def update(__arg1: Rep[K],__arg2: Rep[V])(implicit __pos: SourceContext,__imp1: Overload5) = chashmap_update[K,V](self,__arg1,__arg2)(implicitly[Typ[K]],implicitly[Typ[V]],__pos)
    def contains(__arg1: Rep[K])(implicit __pos: SourceContext,__imp1: Overload5) = chashmap_contains[K,V](self,__arg1)(implicitly[Typ[K]],implicitly[Typ[V]],__pos)
    def keys(implicit __pos: SourceContext,__imp1: Overload1) = chashmap_keys[K,V](self)(implicitly[Typ[K]],implicitly[Typ[V]],__pos)
    def values(implicit __pos: SourceContext,__imp1: Overload1) = chashmap_values[K,V](self)(implicitly[Typ[K]],implicitly[Typ[V]],__pos)
  }



  def chashmap_chashmap[K:Typ,V:Typ]()(implicit __pos: SourceContext): Rep[java.util.concurrent.ConcurrentHashMap[K,V]]
  def chashmap_apply[K:Typ,V:Typ](__arg0: Rep[java.util.concurrent.ConcurrentHashMap[K,V]],__arg1: Rep[K])(implicit __pos: SourceContext): Rep[V]
  def chashmap_update[K:Typ,V:Typ](__arg0: Rep[java.util.concurrent.ConcurrentHashMap[K,V]],__arg1: Rep[K],__arg2: Rep[V])(implicit __pos: SourceContext): Rep[Unit]
  def chashmap_contains[K:Typ,V:Typ](__arg0: Rep[java.util.concurrent.ConcurrentHashMap[K,V]],__arg1: Rep[K])(implicit __pos: SourceContext): Rep[Boolean]
  def chashmap_keys[K:Typ,V:Typ](__arg0: Rep[java.util.concurrent.ConcurrentHashMap[K,V]])(implicit __pos: SourceContext): Rep[ForgeArray[K]]
  def chashmap_values[K:Typ,V:Typ](__arg0: Rep[java.util.concurrent.ConcurrentHashMap[K,V]])(implicit __pos: SourceContext): Rep[ForgeArray[V]]
}
trait CHashMapCompilerOps extends CHashMapOps {
  this: OptiML => 

  def chashmap_from_arrays[K:Typ,V:Typ](__arg0: Rep[ForgeArray[K]],__arg1: Rep[ForgeArray[V]])(implicit __pos: SourceContext): Rep[java.util.concurrent.ConcurrentHashMap[K,V]]
  def chashmap_keys_array[K:Typ,V:Typ](__arg0: Rep[java.util.concurrent.ConcurrentHashMap[K,V]])(implicit __pos: SourceContext): Rep[scala.Array[K]]
  def chashmap_values_array[K:Typ,V:Typ](__arg0: Rep[java.util.concurrent.ConcurrentHashMap[K,V]])(implicit __pos: SourceContext): Rep[scala.Array[V]]
}


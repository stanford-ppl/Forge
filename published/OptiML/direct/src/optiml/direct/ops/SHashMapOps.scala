package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait SHashMapOps extends Base {
  this: OptiML => 

  def SHashMap[K:Typ,V:Typ]()(implicit __pos: SourceContext) = shashmap_shashmap[K,V]()(implicitly[Typ[K]],implicitly[Typ[V]],__pos)

  implicit def repToSHashMapscalacollectionmutableHashMapOpsCls[K:Typ,V:Typ](x: Rep[scala.collection.mutable.HashMap[K,V]])(implicit __pos: SourceContext) = new SHashMapscalacollectionmutableHashMapOpsCls(x)(implicitly[Typ[K]],implicitly[Typ[V]],__pos)
  implicit def varToSHashMapscalacollectionmutableHashMapOpsCls[K:Typ,V:Typ](x: Var[scala.collection.mutable.HashMap[K,V]])(implicit __pos: SourceContext) = new SHashMapscalacollectionmutableHashMapOpsCls(readVar(x))(implicitly[Typ[K]],implicitly[Typ[V]],__pos)

  class SHashMapscalacollectionmutableHashMapOpsCls[K:Typ,V:Typ](val self: Rep[scala.collection.mutable.HashMap[K,V]])(implicit __pos: SourceContext) {
    def apply(__arg1: Rep[K])(implicit __pos: SourceContext,__imp1: Overload22) = shashmap_apply[K,V](self,__arg1)(implicitly[Typ[K]],implicitly[Typ[V]],__pos)
    def update(__arg1: Rep[K],__arg2: Rep[V])(implicit __pos: SourceContext,__imp1: Overload6) = shashmap_update[K,V](self,__arg1,__arg2)(implicitly[Typ[K]],implicitly[Typ[V]],__pos)
    def contains(__arg1: Rep[K])(implicit __pos: SourceContext,__imp1: Overload7) = shashmap_contains[K,V](self,__arg1)(implicitly[Typ[K]],implicitly[Typ[V]],__pos)
    def keys(implicit __pos: SourceContext,__imp1: Overload2) = shashmap_keys[K,V](self)(implicitly[Typ[K]],implicitly[Typ[V]],__pos)
    def values(implicit __pos: SourceContext,__imp1: Overload2) = shashmap_values[K,V](self)(implicitly[Typ[K]],implicitly[Typ[V]],__pos)
  }



  def shashmap_shashmap[K:Typ,V:Typ]()(implicit __pos: SourceContext): Rep[scala.collection.mutable.HashMap[K,V]]
  def shashmap_apply[K:Typ,V:Typ](__arg0: Rep[scala.collection.mutable.HashMap[K,V]],__arg1: Rep[K])(implicit __pos: SourceContext): Rep[V]
  def shashmap_update[K:Typ,V:Typ](__arg0: Rep[scala.collection.mutable.HashMap[K,V]],__arg1: Rep[K],__arg2: Rep[V])(implicit __pos: SourceContext): Rep[Unit]
  def shashmap_contains[K:Typ,V:Typ](__arg0: Rep[scala.collection.mutable.HashMap[K,V]],__arg1: Rep[K])(implicit __pos: SourceContext): Rep[Boolean]
  def shashmap_keys[K:Typ,V:Typ](__arg0: Rep[scala.collection.mutable.HashMap[K,V]])(implicit __pos: SourceContext): Rep[ForgeArray[K]]
  def shashmap_values[K:Typ,V:Typ](__arg0: Rep[scala.collection.mutable.HashMap[K,V]])(implicit __pos: SourceContext): Rep[ForgeArray[V]]
}
trait SHashMapCompilerOps extends SHashMapOps {
  this: OptiML => 

  def shashmap_from_arrays[K:Typ,V:Typ](__arg0: Rep[ForgeArray[K]],__arg1: Rep[ForgeArray[V]])(implicit __pos: SourceContext): Rep[scala.collection.mutable.HashMap[K,V]]
  def shashmap_keys_array[K:Typ,V:Typ](__arg0: Rep[scala.collection.mutable.HashMap[K,V]])(implicit __pos: SourceContext): Rep[scala.Array[K]]
  def shashmap_values_array[K:Typ,V:Typ](__arg0: Rep[scala.collection.mutable.HashMap[K,V]])(implicit __pos: SourceContext): Rep[scala.Array[V]]
}


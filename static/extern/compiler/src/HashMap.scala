package LOWERCASE_DSL_NAME.compiler

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Config

// For compiler (Delite) implementation
trait ForgeHashMapOpsExp extends DeliteMapOpsExp {
  this: ForgeArrayOpsExp with DeliteOpsExp with PrimitiveOpsExp with OrderingOpsExp with EqualExp =>

  type ForgeHashMap[K,V] = DeliteMap[K,V]
  implicit def forgeMapManifest[K:Manifest,V:Manifest] = manifest[DeliteMap[K,V]]

  def fhashmap_from_shashmap[K:Manifest,V:Manifest](m: Rep[scala.collection.mutable.HashMap[K,V]])(implicit ctx: SourceContext): Rep[ForgeHashMap[K,V]]
    = throw new UnsupportedOperationException("ScalaHashMap to ForgeHashMap not implemented in Delite yet")
  def fhashmap_size[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]])(implicit ctx: SourceContext): Rep[Int]
    = dmap_size(m)
  def fhashmap_get[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[V]
    = dmap_get(m,key)
  def fhashmap_contains[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[Boolean]
    = dmap_contains(m,key)
  def fhashmap_keys[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]])(implicit ctx: SourceContext): Rep[ForgeArray[K]]
    = dmap_keys(m)
  def fhashmap_values[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]])(implicit ctx: SourceContext): Rep[ForgeArray[V]]
    = dmap_values(m)
  //def fhashmap_toArray[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]])(implicit ctx: SourceContext): Rep[ForgeArray[(K,V)]]
}

trait ScalaGenForgeHashMapOps extends ScalaGenDeliteMapOps
trait CudaGenForgeHashMapOps
trait OpenCLGenForgeHashMapOps
trait CGenForgeHashMapOps

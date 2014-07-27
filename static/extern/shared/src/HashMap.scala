package LOWERCASE_DSL_NAME.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

// Front-end
trait ForgeHashMapOps extends Base {
  this: ForgeArrayOps =>

  type ForgeHashMap[K,V]
  implicit def forgeMapManifest[K:Manifest,V:Manifest]: Manifest[ForgeHashMap[K,V]]

  implicit class ForgeHashMapOps[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]]) {
    def apply(key: Rep[K])(implicit ctx: SourceContext) = fhashmap_get(m,key)
    def contains(key: Rep[K])(implicit ctx: SourceContext) = fhashmap_contains(m,key)
    def keys()(implicit ctx: SourceContext) = fhashmap_keys(m)
  }

  def fhashmap_get[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[V]
  def fhashmap_contains[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[Boolean]
  def fhashmap_keys[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]])(implicit ctx: SourceContext): Rep[ForgeArray[K]]
}

trait ForgeHashMapCompilerOps extends ForgeHashMapOps {
  this: ForgeArrayCompilerOps =>

  def fhashmap_from_shashmap[K:Manifest,V:Manifest](m: Rep[scala.collection.mutable.HashMap[K,V]])(implicit ctx: SourceContext): Rep[ForgeHashMap[K,V]]
  def fhashmap_from_arrays[K:Manifest,V:Manifest](keys: Rep[ForgeArray[K]], values: Rep[ForgeArray[V]])(implicit ctx: SourceContext): Rep[ForgeHashMap[K,V]]
  def fhashmap_size[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]])(implicit ctx: SourceContext): Rep[Int]
  def fhashmap_values[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]])(implicit ctx: SourceContext): Rep[ForgeArray[V]]
  //def fhashmap_toArray[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]])(implicit ctx: SourceContext): Rep[ForgeArray[(K,V)]]
}

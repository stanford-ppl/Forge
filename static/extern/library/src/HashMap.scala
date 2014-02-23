package LOWERCASE_DSL_NAME.library

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

trait ForgeHashMapWrapper extends HUMAN_DSL_NAMEBase {
  this: ForgeArrayWrapper =>

  type ForgeHashMap[K,V] = scala.collection.mutable.HashMap[K,V]
  implicit def forgeMapManifest[K:Manifest,V:Manifest] = manifest[ForgeHashMap[K,V]]

  def fhashmap_from_shashmap[K:Manifest,V:Manifest](m: Rep[scala.collection.mutable.HashMap[K,V]])(implicit ctx: SourceContext): Rep[ForgeHashMap[K,V]]
    = m
  def fhashmap_from_arrays[K:Manifest,V:Manifest](keys: Rep[ForgeArray[K]], values: Rep[ForgeArray[V]])(implicit ctx: SourceContext): Rep[ForgeHashMap[K,V]]
    = scala.collection.mutable.HashMap(keys.zip(values): _*)
  def fhashmap_size[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]])(implicit ctx: SourceContext): Rep[Int]
    = m.size
  def fhashmap_get[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[V]
    = m(key)
  def fhashmap_contains[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[Boolean]
    = m.contains(key)
  def fhashmap_keys[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]])(implicit ctx: SourceContext): Rep[ForgeArray[K]]
    = m.keys.toArray
  def fhashmap_values[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]])(implicit ctx: SourceContext): Rep[ForgeArray[V]]
    = m.values.toArray
  //def fhashmap_toArray[K:Manifest,V:Manifest](m: Rep[ForgeHashMap[K,V]])(implicit ctx: SourceContext): Rep[ForgeArray[(K,V)]]
}



package optiml.direct

import scala.annotation.unchecked.uncheckedVariance
//import scala.reflect.{Manifest,SourceContext}

// Front-end
trait ForgeHashMapOps extends Base {
  this: ForgeArrayOps =>

  type ForgeHashMap[K,V]
  implicit def forgeMapManifest[K:Typ,V:Typ]: Typ[ForgeHashMap[K,V]]

  implicit class ForgeHashMapOps[K:Typ,V:Typ](m: Rep[ForgeHashMap[K,V]]) {
    def apply(key: Rep[K])(implicit ctx: SourceContext) = fhashmap_get(m,key)
    def contains(key: Rep[K])(implicit ctx: SourceContext) = fhashmap_contains(m,key)
    def keys()(implicit ctx: SourceContext) = fhashmap_keys(m)
  }

  def fhashmap_get[K:Typ,V:Typ](m: Rep[ForgeHashMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[V]
  def fhashmap_contains[K:Typ,V:Typ](m: Rep[ForgeHashMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[Boolean]
  def fhashmap_keys[K:Typ,V:Typ](m: Rep[ForgeHashMap[K,V]])(implicit ctx: SourceContext): Rep[ForgeArray[K]]
}

trait ForgeHashMapCompilerOps extends ForgeHashMapOps {
  this: ForgeArrayCompilerOps =>

  def fhashmap_from_shashmap[K:Typ,V:Typ](m: Rep[scala.collection.mutable.HashMap[K,V]])(implicit ctx: SourceContext): Rep[ForgeHashMap[K,V]]
  def fhashmap_from_arrays[K:Typ,V:Typ](keys: Rep[ForgeArray[K]], values: Rep[ForgeArray[V]])(implicit ctx: SourceContext): Rep[ForgeHashMap[K,V]]
  def fhashmap_size[K:Typ,V:Typ](m: Rep[ForgeHashMap[K,V]])(implicit ctx: SourceContext): Rep[Int]
  def fhashmap_values[K:Typ,V:Typ](m: Rep[ForgeHashMap[K,V]])(implicit ctx: SourceContext): Rep[ForgeArray[V]]
  //def fhashmap_toArray[K:Typ,V:Typ](m: Rep[ForgeHashMap[K,V]])(implicit ctx: SourceContext): Rep[ForgeArray[(K,V)]]
}

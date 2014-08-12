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

  def fhashmap_from_shashmap[K:Manifest,V:Manifest](m: Rep[scala.collection.mutable.HashMap[K,V]])(implicit ctx: SourceContext): Rep[ForgeHashMap[K,V]] = {
    val keys = farray_from_sarray(reflectPure(ExtSHashMapKeys(m)))
    val values = farray_from_sarray(reflectPure(ExtSHashMapValues(m)))
    val id: Exp[K] => Exp[K] = k => k
    dmap_fromCollection[K,K,V](keys, id, values)
  }
  def fhashmap_from_arrays[K:Manifest,V:Manifest](keys: Rep[ForgeArray[K]], values: Rep[ForgeArray[V]])(implicit ctx: SourceContext): Rep[ForgeHashMap[K,V]] = {
    // if we already have an index, we could skip rebuilding it as well 
    val id: Exp[K] => Exp[K] = k => k
    val index = reflectPure(DeliteMapBuildIndex(keys,id)) 
    reflectPure(DeliteMapNewImm(keys, values, index, values.length))    
    // dmap_fromCollection[K,K,V](keys, id, values)
  }
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

  // avoid mixing in SHashMapOpsExp, which requires the mixed-in DSL to have called importHashMap() in Scala.scala (a dependency we don't want to force)
  case class ExtSHashMapKeys[K:Manifest,V:Manifest](m: Exp[scala.collection.mutable.HashMap[K,V]]) extends Def[Array[K]] {
    val mK = manifest[K]
    val mV = manifest[V]
  }
  case class ExtSHashMapValues[K:Manifest,V:Manifest](m: Exp[scala.collection.mutable.HashMap[K,V]]) extends Def[Array[V]] {
    val mK = manifest[K]
    val mV = manifest[V]
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@ExtSHashMapKeys(x) => reflectPure(ExtSHashMapKeys(f(x))(e.mK,e.mV))
    case e@ExtSHashMapValues(x) => reflectPure(ExtSHashMapValues(f(x))(e.mK,e.mV))

    case Reflect(e@ExtSHashMapKeys(x), u, es) => reflectMirrored(Reflect(ExtSHashMapKeys(f(x))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@ExtSHashMapValues(x), u, es) => reflectMirrored(Reflect(ExtSHashMapValues(f(x))(e.mK,e.mV), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
}

trait ScalaGenForgeHashMapOps extends ScalaGenDeliteMapOps {
  val IR: ForgeHashMapOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ExtSHashMapKeys(x) => emitValDef(sym, "" + quote(x) + ".keys.toArray")
    case ExtSHashMapValues(x) => emitValDef(sym, "" + quote(x) + ".values.toArray")
    case _ => super.emitNode(sym,rhs)
  }
}
trait CudaGenForgeHashMapOps
trait OpenCLGenForgeHashMapOps
trait CGenForgeHashMapOps extends CGenDeliteMapOps

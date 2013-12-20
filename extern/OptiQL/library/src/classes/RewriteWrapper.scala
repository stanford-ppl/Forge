package optiql.library.classes

import optiql.shared.ops._
import optiql.library._
import optiql.library.classes._
import scala.reflect.{Manifest,SourceContext}
import scala.collection.mutable.HashMap

trait RewriteWrapper extends RewriteOps {
  this: OptiQLBase with OptiQLClasses =>

  def groupByHackImpl[K:Manifest,V:Manifest](self: Rep[Table[V]], keySelector: Rep[V] => Rep[K])(implicit pos: SourceContext): Rep[Table[Tup2[K,Table[V]]]] = {
    val map = self.data.take(self.size).groupBy(keySelector)

    val pairs = new scala.collection.mutable.ArrayBuffer[Tup2[K,Table[V]]]
    for ((key,v) <- map) {
      val values = v.toArray
      pairs += new Tup2(key, new Table(v.length, v))
    }

    new Table(pairs.length, pairs.toArray)
  }

  def sortHackImpl[A:Manifest,K:Manifest:Ordering](self: Rep[Table[A]], keySelector: Rep[A] => Rep[K], ascending: Boolean)(implicit pos: SourceContext): Rep[Table[A]] = {
    val data = if (ascending) 
      self.data.take(self.size).sortWith((a,b) => implicitly[Ordering[K]].lt(keySelector(a), keySelector(b)))
    else 
      self.data.take(self.size).sortWith((a,b) => implicitly[Ordering[K]].lt(keySelector(b), keySelector(a)))
    new Table(self.size, data)
  }

}

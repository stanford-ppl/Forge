package optiql.library.classes

import optiql.shared.ops._
import optiql.library._
import optiql.library.classes._
import scala.reflect.{Manifest,SourceContext}
import scala.collection.mutable.HashMap

trait RewriteWrapper extends RewriteOps {
  this: OptiQLBase with OptiQLClasses =>

  def upgradeInt[R:Manifest](value: Rep[Int]): Rep[R] = value.toDouble.asInstanceOf[Rep[R]]

  def groupByHackImpl[K:Manifest,V:Manifest](self: Rep[Table[V]], keySelector: Rep[V] => Rep[K])(implicit pos: SourceContext): Rep[Table[Tup2[K,Table[V]]]] = {
    val map = self.data.take(self.size).groupBy(keySelector)

    val pairs = new scala.collection.mutable.ArrayBuffer[Tup2[K,Table[V]]]
    for ((key,v) <- map) {
      val values = v.toArray
      pairs += new Tup2(key, new Table(v.length, v))
    }

    new Table(pairs.length, pairs.toArray)
  }

  def zeroType[T:Manifest]: Rep[T] = null.asInstanceOf[Rep[T]]

}

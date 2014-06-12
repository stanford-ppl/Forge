package optiql.library.classes

import optiql.shared.ops._
import optiql.library._
import optiql.library.classes._
import scala.reflect.{Manifest,SourceContext}
import scala.collection.mutable.HashMap

trait RewriteWrapper {
  this: OptiQLBase with OptiQLClasses =>

  def groupByHackImpl[K:Manifest,V:Manifest](self: Rep[Table[V]], keySelector: Rep[V] => Rep[K])(implicit pos: SourceContext): Rep[Table[Tup2[K,Table[V]]]] = {
    val arr = self.data.take(self.size)
    val keys = arr.map(keySelector).distinct //DeliteMap is order-preserving on keys, be consistent for sanity
    val map = arr.groupBy(keySelector)

    val pairs = for (key <- keys) yield {
      val v = map(key)
      new Tup2(key, new Table(v.length, v))
    }

    new Table(pairs.length, pairs)
  }

  def sortHackImpl[A:Manifest](self: Rep[Table[A]], comparator: (Rep[A],Rep[A]) => Rep[Int])(implicit pos: SourceContext): Rep[Table[A]] = {
    val arr = self.data.take(self.size)
    val ord = new Ordering[A] {
      def compare(x: A, y: A): Int = comparator(x,y)
    }
    val sorted = arr.sorted(ord)
    new Table(self.size, sorted)
  }

  def compareHackImpl[A:Manifest:Ordering](lhs: Rep[A], rhs: Rep[A]): Rep[Int] = {
    implicitly[Ordering[A]].compare(lhs, rhs)
  }

  ///////

  def table_printastable[A:Manifest](self: Rep[Table[A]],maxRows: Rep[Int] = unit(100))(implicit __pos: SourceContext) = {
    TablePrinter.printAsTable(self, maxRows)
  }
  
  def table_writeasjson[A:Manifest](self: Rep[Table[A]],path: Rep[String])(implicit __pos: SourceContext) = {
    TablePrinter.writeAsJSON(self, path)
  }

}

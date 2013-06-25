package optiql.shared.ops

import scala.virtualization.lms.common.StructOps
import optiql.shared._
import optiql.shared.ops._
import scala.reflect.{Manifest,SourceContext}

trait RewriteOps 
trait RewriteCompilerOps extends RewriteOps {
  this: OptiQL =>

  def upgradeInt[R:Manifest](value: Rep[Int]): Rep[R]
  def groupByHackImpl[K:Manifest,V:Manifest](self: Rep[Table[V]], keySelector: Rep[V] => Rep[K])(implicit pos: SourceContext): Rep[Table[Tup2[K,Table[V]]]]
  def zeroType[T:Manifest]: Rep[T]
}

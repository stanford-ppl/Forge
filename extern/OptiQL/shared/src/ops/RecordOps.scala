package optiql.shared.ops

import scala.virtualization.lms.common.StructOps
import optiql.shared._
import optiql.shared.ops._
import scala.reflect.{Manifest,SourceContext}

trait OptiQLRecordOps extends StructOps with ForgeArrayCompilerOps {
  //hack to expose arrays
  def farray_apply[T:Manifest](a: Rep[ForgeArray[T]], i: Rep[Int])(implicit ctx: SourceContext) = array_apply(a,i)
  def farray_length[T:Manifest](a: Rep[ForgeArray[T]])(implicit ctx: SourceContext) = array_length(a)
}

trait OptiQLRecordCompilerOps extends OptiQLRecordOps {
  this: OptiQL =>

  def upgradeInt[R:Manifest](value: Rep[Int]): Rep[R]
  def groupByHackImpl[K:Manifest,V:Manifest](self: Rep[Table[V]], keySelector: Rep[V] => Rep[K])(implicit pos: SourceContext): Rep[Table[Tup2[K,Table[V]]]]
  def zeroType[T:Manifest]: Rep[T]

}

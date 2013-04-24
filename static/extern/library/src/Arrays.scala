package LOWERCASE_DSL_NAME.library.extern

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import LOWERCASE_DSL_NAME.library.HUMAN_DSL_NAMEBase

trait ForgeArrayWrapper extends HUMAN_DSL_NAMEBase {
  type ForgeArray[T] = scala.Array[T]
  
  def array_empty[T:Manifest](__arg0: Rep[Int])(implicit __imp0: SourceContext): Rep[ForgeArray[T]] 
    = new ForgeArray[T](__arg0)
  def array_copy[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[ForgeArray[T]],__arg3: Rep[Int],__arg4: Rep[Int])(implicit __imp0: SourceContext): Rep[Unit]
    = System.arraycopy(__arg0,__arg1,__arg2,__arg3,__arg4)
  def array_update[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int],__arg2: Rep[T])(implicit __imp0: SourceContext): Rep[Unit]
    = __arg0(__arg1) = __arg2
  def array_apply[T:Manifest](__arg0: Rep[ForgeArray[T]],__arg1: Rep[Int])(implicit __imp0: SourceContext): Rep[T]
    = __arg0(__arg1)
  def array_length[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[Int]          
    = __arg0.length
  def array_asimmutable[T:Manifest](__arg0: Rep[ForgeArray[T]])(implicit __imp0: SourceContext): Rep[ForgeArray[T]]
    = __arg0
}





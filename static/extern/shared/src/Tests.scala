package LOWERCASE_DSL_NAME.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

import java.io._
import scala.collection.mutable.ArrayBuffer

trait TestsOps
trait TestsCompilerOps extends TestsOps

trait ForgeTestModule extends Base {
  //var args: Rep[Array[String]]
  def main(): Unit

  def collector: Rep[ArrayBuffer[Boolean]]

  def collect(s: Rep[Boolean]): Unit

  def mkReport(): Rep[Unit]
}

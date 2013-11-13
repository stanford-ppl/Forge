package LOWERCASE_DSL_NAME.compiler

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteFileReaderOpsExp, ScalaGenDeliteFileReaderOps}
import ppl.delite.framework.datastructures._

// For compiler (Delite) implementation
trait InputOutputOpsExp extends DeliteFileReaderOpsExp {
  this: ForgeArrayOpsExp with ForgeArrayBufferOpsExp =>

  def forge_filereader_readlines[A:Manifest](path: Rep[String], f: Rep[String] => Rep[A]): Rep[ForgeArray[A]] = {
    DeliteNewFileReader.readLines[A](path)(f)
  }

  def forge_filereader_readlines_flattened[A:Manifest](path: Rep[String], f: Rep[String] => Rep[ForgeArray[A]]): Rep[ForgeArray[A]] = {
    DeliteNewFileReader.readLinesFlattened[A](path)(f)
  }

  def forge_filereader_readlines_unstructured[A:Manifest](path: Rep[String], append: (Rep[String], Rep[ForgeArrayBuffer[A]]) => Rep[Unit]): Rep[ForgeArray[A]] = {
    DeliteFileReader.readLinesUnstructured[A](path)(append)
  }
}

trait ScalaGenInputOutputOps extends ScalaGenDeliteFileReaderOps
trait CudaGenInputOutputOps
trait OpenCLGenInputOutputOps
trait CGenInputOutputOps

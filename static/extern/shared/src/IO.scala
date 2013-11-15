package LOWERCASE_DSL_NAME.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

// Front-end
trait InputOutputOps extends ForgeArrayOps with ForgeArrayBufferOps {
  object ForgeFileReader {
    def readLines[A:Manifest](path: Rep[String])(f: Rep[String] => Rep[A]): Rep[ForgeArray[A]] = forge_filereader_readlines(path, f)
    def readLinesFlattened[A:Manifest](path: Rep[String])(f: Rep[String] => Rep[ForgeArray[A]]): Rep[ForgeArray[A]] = forge_filereader_readlines_flattened(path, f)
    def readLinesUnstructured[A:Manifest](path: Rep[String])(f: (Rep[String], Rep[ForgeArrayBuffer[A]]) => Rep[Unit]): Rep[ForgeArray[A]] = forge_filereader_readlines_unstructured(path, f)
  }

  def forge_filereader_readlines[A:Manifest](path: Rep[String], f: Rep[String] => Rep[A]): Rep[ForgeArray[A]]
  def forge_filereader_readlines_flattened[A:Manifest](path: Rep[String], f: Rep[String] => Rep[ForgeArray[A]]): Rep[ForgeArray[A]]
  def forge_filereader_readlines_unstructured[A:Manifest](path: Rep[String], f: (Rep[String], Rep[ForgeArrayBuffer[A]]) => Rep[Unit]): Rep[ForgeArray[A]]
}

trait InputOutputCompilerOps extends InputOutputOps with ForgeArrayCompilerOps with ForgeArrayBufferCompilerOps

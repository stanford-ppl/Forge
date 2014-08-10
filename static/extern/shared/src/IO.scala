package LOWERCASE_DSL_NAME.shared

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

// Front-end
trait InputOutputOps extends Base {
	this: ForgeArrayOps with ForgeArrayBufferOps =>

  object ForgeFileReader {
    def readLines[A:Manifest](path: Rep[String])(f: Rep[String] => Rep[A])(implicit ctx: SourceContext): Rep[ForgeArray[A]] = forge_filereader_readlines(path, f)
    def readLinesFlattened[A:Manifest](path: Rep[String])(f: Rep[String] => Rep[ForgeArray[A]])(implicit ctx: SourceContext): Rep[ForgeArray[A]] = forge_filereader_readlines_flattened(path, f)
    def readLinesUnstructured[A:Manifest](path: Rep[String])(f: (Rep[String], Rep[ForgeArrayBuffer[A]]) => Rep[Unit])(implicit ctx: SourceContext): Rep[ForgeArray[A]] = forge_filereader_readlines_unstructured(path, f)
  }

  def forge_filereader_readlines[A:Manifest](path: Rep[String], f: Rep[String] => Rep[A])(implicit ctx: SourceContext): Rep[ForgeArray[A]]
  def forge_filereader_readlines_flattened[A:Manifest](path: Rep[String], f: Rep[String] => Rep[ForgeArray[A]])(implicit ctx: SourceContext): Rep[ForgeArray[A]]
  def forge_filereader_readlines_unstructured[A:Manifest](path: Rep[String], f: (Rep[String], Rep[ForgeArrayBuffer[A]]) => Rep[Unit])(implicit ctx: SourceContext): Rep[ForgeArray[A]]
}

trait InputOutputCompilerOps extends InputOutputOps {
	this: ForgeArrayCompilerOps with ForgeArrayBufferCompilerOps =>
}

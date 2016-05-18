package LOWERCASE_DSL_NAME.compiler

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._

// For compiler (Delite) implementation
trait InputOutputOpsExp extends DeliteFileReaderOpsExp with DeliteFileWriterOpsExp {
  this: ForgeArrayOpsExp with ForgeArrayBufferOpsExp =>

  type ForgeFileInputStream = DeliteFileInputStream
  implicit def forgeInputStreamManifest = manifest[ForgeFileInputStream]

  def forge_filereader_readlines[A:Manifest](path: Rep[String], f: Rep[String] => Rep[A])(implicit ctx: SourceContext): Rep[ForgeArray[A]] = {
    DeliteFileReader.readLines[A](path)(f)
  }

  def forge_filereader_readlines_flattened[A:Manifest](path: Rep[String], f: Rep[String] => Rep[ForgeArray[A]])(implicit ctx: SourceContext): Rep[ForgeArray[A]] = {
    DeliteFileReader.readLinesFlattened[A](path)(f)
  }

  def forge_filereader_readlines_chunk[A:Manifest](stream: Rep[ForgeFileInputStream], offset: Rep[Long], numBytes: Rep[Long], f: (Rep[String], Rep[String]) => Rep[A])(implicit ctx: SourceContext): Rep[ForgeArray[A]] = {
    DeliteFileReader.readLinesChunk[A](stream)(offset, numBytes)(f)
  }

  def forge_fileinputstream_new(path: Rep[String])(implicit ctx: SourceContext): Rep[ForgeFileInputStream] = {
    dfis_new_effectful(Seq(path))
  }

  def forge_fileinputstream_readline(stream: Rep[ForgeFileInputStream])(implicit ctx: SourceContext): Rep[String] = {
    dfis_readLine_effectful(stream)
  }

  def forge_fileinputstream_size(stream: Rep[ForgeFileInputStream])(implicit ctx: SourceContext): Rep[Long] = {
    dfis_size(stream)
  }

  def forge_fileinputstream_close(stream: Rep[ForgeFileInputStream])(implicit ctx: SourceContext): Rep[Unit] = {
    dfis_close(stream)
  }

  def forge_filewriter_writelines(path: Rep[String], numLines: Rep[Int], append: Rep[Boolean], f: Rep[Int] => Rep[String])(implicit ctx: SourceContext): Rep[Unit] = {
    DeliteFileWriter.writeLines(path, numLines, append)(f)
  }


  type ForgeFileOutputStream = DeliteFileOutputStream
  implicit def forgeOutputStreamManifest = manifest[ForgeFileOutputStream]

  def forge_fileoutputstream_new(path: Rep[String], _append: Rep[Boolean])(implicit ctx: SourceContext): Rep[ForgeFileOutputStream] = {
    dfos_new(path, append = _append) // single-threaded / single-file
  }

  def forge_fileoutputstream_writeline(stream: Rep[ForgeFileOutputStream], line: Rep[String])(implicit ctx: SourceContext): Rep[Unit] = {
    dfos_writeLine(stream, line)
  }

  def forge_fileoutputstream_close(stream: Rep[ForgeFileOutputStream])(implicit ctx: SourceContext): Rep[Unit] = {
    dfos_close(stream)
  }
}

trait ScalaGenInputOutputOps extends ScalaGenDeliteFileReaderOps with ScalaGenDeliteFileWriterOps {
  val IR: InputOutputOpsExp
}
trait CudaGenInputOutputOps
trait OpenCLGenInputOutputOps
trait CGenInputOutputOps extends CGenDeliteFileReaderOps with CGenDeliteFileWriterOps {
  val IR: InputOutputOpsExp
}

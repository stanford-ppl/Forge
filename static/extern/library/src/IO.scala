package LOWERCASE_DSL_NAME.library

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

import java.io.{InputStreamReader, BufferedReader, OutputStreamWriter, BufferedWriter, PrintWriter}

import org.apache.hadoop.conf._
import org.apache.hadoop.fs._

trait InputOutputWrapper extends HUMAN_DSL_NAMEBase {
  this: ForgeArrayWrapper with ForgeArrayBufferWrapper =>

  def forge_filereader_readlines[A:Manifest](path: Rep[String], f: Rep[String] => Rep[A])(implicit ctx: SourceContext): Rep[ForgeArray[A]] = {
    forge_filereader_readlines_unstructured(path, (line, buf) => array_buffer_append(buf, f(line)))
  }

  def forge_filereader_readlines_flattened[A:Manifest](path: Rep[String], f: Rep[String] => Rep[ForgeArray[A]])(implicit ctx: SourceContext): Rep[ForgeArray[A]] = {
    forge_filereader_readlines_unstructured(path, (line, buf) => {
      val a = f(line)
      for (i <- 0 until array_length(a)) {
        array_buffer_append(buf, a(i))
      }
    })
  }

  def forge_filereader_readlines_unstructured[A:Manifest](path: Rep[String], append: (Rep[String], Rep[ForgeArrayBuffer[A]]) => Rep[Unit])(implicit ctx: SourceContext): Rep[ForgeArray[A]] = {
    val input = forge_fileinputstream_new(path)
    val out = new ForgeArrayBuffer[A](0)
    var line = forge_fileinputstream_readline(input)
    while (line != null) {
      append(line, out)
      line = forge_fileinputstream_readline(input)
    }
    forge_fileinputstream_close(input)
    out.toArray
  }

  def forge_filewriter_writelines(path: Rep[String], numLines: Rep[Int], f: Rep[Int] => Rep[String])(implicit ctx: SourceContext): Rep[Unit] = {
    val output = forge_fileoutputstream_new(path)
    for (i <- 0 until numLines) {
      forge_fileoutputstream_writeline(output, f(i))
    }
    forge_fileoutputstream_close(output)
  }


  // Note: ForgeFileInputStream interpreter does not exactly replicate the semantics of DeliteFileInputStream,
  // since we do not concatenate multiple physical files together. Therefore, we cannot read a directory in
  // interpreter mode. This ought to be fixed, but would cause significant code duplication unless we come up with
  // a better refactoring.

  class ForgeFileInputStream(val reader: Rep[BufferedReader])
  implicit def forgeInputStreamManifest = manifest[ForgeFileInputStream]

  def forge_fileinputstream_new(path: Rep[String])(implicit ctx: SourceContext): Rep[ForgeFileInputStream] = {
    val hPath = new Path(path)
    val conf = new Configuration()
    val stream = hPath.getFileSystem(conf).open(hPath)
    new ForgeFileInputStream(new BufferedReader(new InputStreamReader(stream)))
  }

  def forge_fileinputstream_readline(stream: Rep[ForgeFileInputStream])(implicit ctx: SourceContext): Rep[String] = {
    stream.reader.readLine()
  }

  def forge_fileinputstream_close(stream: Rep[ForgeFileInputStream])(implicit ctx: SourceContext): Rep[Unit] = {
    stream.reader.close()
  }


  class ForgeFileOutputStream(val writer: Rep[PrintWriter])
  implicit def forgeOutputStreamManifest = manifest[ForgeFileOutputStream]

  def forge_fileoutputstream_new(path: Rep[String])(implicit ctx: SourceContext): Rep[ForgeFileOutputStream] = {
    val hPath = new Path(path)
    val conf = new Configuration()
    val stream = hPath.getFileSystem(conf).create(hPath)
    new ForgeFileOutputStream(new PrintWriter(new BufferedWriter(new OutputStreamWriter(stream))))
  }

  def forge_fileoutputstream_writeline(stream: Rep[ForgeFileOutputStream], line: Rep[String])(implicit ctx: SourceContext): Rep[Unit] = {
    stream.writer.println(line)
  }

  def forge_fileoutputstream_close(stream: Rep[ForgeFileOutputStream])(implicit ctx: SourceContext): Rep[Unit] = {
    stream.writer.close()
  }
}

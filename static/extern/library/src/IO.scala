package LOWERCASE_DSL_NAME.library

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

import java.io.{OutputStreamWriter, BufferedWriter, PrintWriter}
import generated.scala.io.DeliteFileInputStream

import org.apache.hadoop.conf._
import org.apache.hadoop.fs._
import org.apache.hadoop.util.LineReader
import org.apache.hadoop.io.Text

trait InputOutputWrapper extends HUMAN_DSL_NAMEBase {
  this: ForgeArrayWrapper with ForgeArrayBufferWrapper =>

  type ForgeFileInputStream = DeliteFileInputStream
  implicit def forgeInputStreamManifest = manifest[ForgeFileInputStream]

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
    var line = input.readLine()
    while (line != null) {
      append(line, out)
      line = input.readLine()
    }
    input.close()
    out.toArray
  }

  def forge_filereader_readlines_chunk[A:Manifest](stream: Rep[ForgeFileInputStream], offset: Rep[Long], numBytes: Rep[Long], f: (Rep[String], Rep[String]) => Rep[A])(implicit ctx: SourceContext): Rep[ForgeArray[A]] = {
    val out = new ForgeArrayBuffer[A](0)
    val input = stream.withOffset(offset)
    input.openAtNewLine(0, numBytes)
    while (!input.isEmpty) {
      val location = input.getFileLocation
      val line = input.readLine()
      array_buffer_append(out, f(line, location))
    }
    input.close()
    out.toArray
  }

  def forge_fileinputstream_new(path: Rep[String])(implicit ctx: SourceContext): Rep[ForgeFileInputStream] = {
    val out = DeliteFileInputStream(Seq(path))
    out.open()
    out
  }

  def forge_fileinputstream_readline(stream: Rep[ForgeFileInputStream])(implicit ctx: SourceContext): Rep[String] = {
    stream.readLine()
  }

  def forge_fileinputstream_size(stream: Rep[ForgeFileInputStream])(implicit ctx: SourceContext): Rep[Long] = {
    stream.size
  }

  def forge_fileinputstream_close(stream: Rep[ForgeFileInputStream])(implicit ctx: SourceContext): Rep[Unit] = {
    stream.close()
  }

  def forge_filewriter_writelines(path: Rep[String], numLines: Rep[Int], append: Rep[Boolean], f: Rep[Int] => Rep[String])(implicit ctx: SourceContext): Rep[Unit] = {
    val output = forge_fileoutputstream_new(path, append)
    for (i <- 0 until numLines) {
      forge_fileoutputstream_writeline(output, f(i))
    }
    forge_fileoutputstream_close(output)
  }

  class ForgeFileOutputStream(val writer: Rep[PrintWriter])
  implicit def forgeOutputStreamManifest = manifest[ForgeFileOutputStream]

  def forge_fileoutputstream_new(path: Rep[String], append: Rep[Boolean])(implicit ctx: SourceContext): Rep[ForgeFileOutputStream] = {
    val hPath = new Path(path)
    val conf = new Configuration()
    // Append doesn't work with LocalFileSystem, so we have to use RawLocalFileSystem.
    conf.set("fs.file.impl", classOf[org.apache.hadoop.fs.RawLocalFileSystem].getName)

    // This is a workaround for RawLocalFileSystem not being instantiated when we want:
    val _fs = hPath.getFileSystem(conf)
    val fs =
      if (append && _fs.isInstanceOf[LocalFileSystem]) {
        val rlfs = new RawLocalFileSystem()
        rlfs.setConf(conf)
        rlfs
      }
      else _fs

    val stream = if (fs.exists(hPath) && append) fs.append(hPath) else fs.create(hPath)
    new ForgeFileOutputStream(new PrintWriter(new BufferedWriter(new OutputStreamWriter(stream))))
  }

  def forge_fileoutputstream_writeline(stream: Rep[ForgeFileOutputStream], line: Rep[String])(implicit ctx: SourceContext): Rep[Unit] = {
    stream.writer.println(line)
  }

  def forge_fileoutputstream_close(stream: Rep[ForgeFileOutputStream])(implicit ctx: SourceContext): Rep[Unit] = {
    stream.writer.close()
  }
}

package LOWERCASE_DSL_NAME.library

import scala.annotation.unchecked.uncheckedVariance
import reflect.Manifest;
import org.scala_lang.virtualized.SourceContext
import scala.virtualization.lms.common._

import java.io.{OutputStreamWriter, BufferedWriter, PrintWriter}

import org.apache.hadoop.conf._
import org.apache.hadoop.fs._
import org.apache.hadoop.util.LineReader
import org.apache.hadoop.io.Text

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

  def forge_filereader_readlines_chunk[A:Manifest](path: Rep[String], offset: Rep[Long], numBytes: Rep[Long], f: Rep[String] => Rep[A])(implicit ctx: SourceContext): Rep[ForgeArray[A]] = {
    val out = new ForgeArrayBuffer[A](0)
    val input = forge_fileinputstream_new_withoffset(path, offset)

    // skip ahead to next line - any previous reader should have read past this offset to a full line.
    var line = forge_fileinputstream_readline(input)
    var curPos = input.pos
    if (offset == 0) array_buffer_append(out, f(line))

    // <= because the next chunk will skip the first (partial or full) line above
    while (line != null && (curPos - offset) <= numBytes) {
      line = forge_fileinputstream_readline(input)
      if (line != null) array_buffer_append(out, f(line))
      curPos = input.pos
    }
    forge_fileinputstream_close(input)
    out.toArray
  }

  def forge_filewriter_writelines(path: Rep[String], numLines: Rep[Int], append: Rep[Boolean], f: Rep[Int] => Rep[String])(implicit ctx: SourceContext): Rep[Unit] = {
    val output = forge_fileoutputstream_new(path, append)
    for (i <- 0 until numLines) {
      forge_fileoutputstream_writeline(output, f(i))
    }
    forge_fileoutputstream_close(output)
  }


  // Note: ForgeFileInputStream interpreter does not exactly replicate the semantics of DeliteFileInputStream,
  // since we do not concatenate multiple physical files together. Therefore, we cannot read a directory in
  // interpreter mode. This ought to be fixed, but would cause even more code duplication than is already
  // here unless we come up with a better refactoring.

  class ForgeFileInputStream(val stream: Rep[FSDataInputStream], val reader: Rep[LineReader], var pos: Rep[Long], val size: Rep[Long])
  implicit def forgeInputStreamManifest = manifest[ForgeFileInputStream]

  private var lineText = new Text()

  private def forge_fileinputstream_new_withoffset(path: Rep[String], offset: Rep[Long] = 0L)(implicit ctx: SourceContext): Rep[ForgeFileInputStream] = {
    val hPath = new Path(path)
    val conf = new Configuration()
    val fs = hPath.getFileSystem(conf)
    val stream = fs.open(hPath)
    val reader = new LineReader(stream)
    var pos = 0L
    if (offset > 0) {
      stream.skip(offset)
      pos += offset
    }
    val size = fs.getFileStatus(hPath).getLen()
    new ForgeFileInputStream(stream, new LineReader(stream), pos, size)
  }

  def forge_fileinputstream_new(path: Rep[String])(implicit ctx: SourceContext): Rep[ForgeFileInputStream] = {
    forge_fileinputstream_new_withoffset(path)
  }

  def forge_fileinputstream_readline(stream: Rep[ForgeFileInputStream])(implicit ctx: SourceContext): Rep[String] = {
    val length = stream.reader.readLine(lineText)
    stream.pos += length
    if (length > 0) lineText.toString else null
  }

  def forge_fileinputstream_size(stream: Rep[ForgeFileInputStream])(implicit ctx: SourceContext): Rep[Long] = {
    stream.size
  }

  def forge_fileinputstream_close(stream: Rep[ForgeFileInputStream])(implicit ctx: SourceContext): Rep[Unit] = {
    stream.reader.close()
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

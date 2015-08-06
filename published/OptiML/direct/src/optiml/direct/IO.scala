package optiml.direct

import scala.annotation.unchecked.uncheckedVariance
//import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.util.OverloadHack

// Front-end
trait InputOutputOps extends Base {
	this: ForgeArrayOps with ForgeArrayBufferOps =>

  object ForgeFileReader {
    def readLines[A:Manifest](path: Rep[String])(f: Rep[String] => Rep[A])(implicit ctx: SourceContext): Rep[ForgeArray[A]] = forge_filereader_readlines(path, f)
    def readLinesFlattened[A:Manifest](path: Rep[String])(f: Rep[String] => Rep[ForgeArray[A]])(implicit ctx: SourceContext): Rep[ForgeArray[A]] = forge_filereader_readlines_flattened(path, f)

    // This version allows reading from a stream in parallel starting at a particular offset and proceeding for a fixed number of bytes (used with streaming).
    def readLinesChunk[A:Manifest](path: Rep[String])(offset: Rep[Long], numBytes: Rep[Long])(f: Rep[String] => Rep[A])(implicit pos: SourceContext): Rep[ForgeArray[A]] = forge_filereader_readlines_chunk(path, offset, numBytes, f)
  }

  def forge_filereader_readlines[A:Manifest](path: Rep[String], f: Rep[String] => Rep[A])(implicit ctx: SourceContext): Rep[ForgeArray[A]]
  def forge_filereader_readlines_flattened[A:Manifest](path: Rep[String], f: Rep[String] => Rep[ForgeArray[A]])(implicit ctx: SourceContext): Rep[ForgeArray[A]]
  def forge_filereader_readlines_chunk[A:Manifest](path: Rep[String], offset: Rep[Long], numBytes: Rep[Long], f: Rep[String] => Rep[A])(implicit ctx: SourceContext): Rep[ForgeArray[A]]

  object ForgeFileWriter {
    def writeLines(path: Rep[String], numLines: Rep[Int], append: Rep[Boolean] = unit(false))(f: Rep[Int] => Rep[String])(implicit ctx: SourceContext) = forge_filewriter_writelines(path, numLines, append, f)
  }

  def forge_filewriter_writelines(path: Rep[String], numLines: Rep[Int], append: Rep[Boolean], f: Rep[Int] => Rep[String])(implicit ctx: SourceContext): Rep[Unit]
}

trait InputOutputCompilerOps extends InputOutputOps with OverloadHack {
	this: ForgeArrayCompilerOps with ForgeArrayBufferCompilerOps =>

  // When using the Input/Output streams directly (instead of the reader/writer above), computation is sequential and explicitly effectful.
  // However, the path may still refer to data stores outside of the local filesystem (e.g. S3 or HDFS).

  type ForgeFileInputStream
  implicit def forgeInputStreamManifest: Manifest[ForgeFileInputStream]

  object ForgeFileInputStream {
    def apply(path: Rep[String])(implicit ctx: SourceContext): Rep[ForgeFileInputStream] = forge_fileinputstream_new(path)
  }

  def infix_readLine(stream: Rep[ForgeFileInputStream])(implicit ctx: SourceContext, o: Overloaded1) = forge_fileinputstream_readline(stream)
  def infix_size(stream: Rep[ForgeFileInputStream])(implicit ctx: SourceContext, o: Overloaded1) = forge_fileinputstream_size(stream)
  def infix_close(stream: Rep[ForgeFileInputStream])(implicit ctx: SourceContext, o: Overloaded1) = forge_fileinputstream_close(stream)

  def forge_fileinputstream_new(path: Rep[String])(implicit ctx: SourceContext): Rep[ForgeFileInputStream]
  def forge_fileinputstream_readline(stream: Rep[ForgeFileInputStream])(implicit ctx: SourceContext): Rep[String]
  def forge_fileinputstream_size(stream: Rep[ForgeFileInputStream])(implicit ctx: SourceContext): Rep[Long]
  def forge_fileinputstream_close(stream: Rep[ForgeFileInputStream])(implicit ctx: SourceContext): Rep[Unit]


  type ForgeFileOutputStream
  implicit def forgeOutputStreamManifest: Manifest[ForgeFileOutputStream]

  object ForgeFileOutputStream {
    def apply(path: Rep[String], append: Rep[Boolean] = unit(false))(implicit ctx: SourceContext): Rep[ForgeFileOutputStream] = forge_fileoutputstream_new(path, append)
  }

  def infix_writeLine(stream: Rep[ForgeFileOutputStream], line: Rep[String])(implicit ctx: SourceContext, o: Overloaded1) = forge_fileoutputstream_writeline(stream, line)
  def infix_close(stream: Rep[ForgeFileOutputStream])(implicit ctx: SourceContext, o: Overloaded2) = forge_fileoutputstream_close(stream)

  def forge_fileoutputstream_new(path: Rep[String], append: Rep[Boolean])(implicit ctx: SourceContext): Rep[ForgeFileOutputStream]
  def forge_fileoutputstream_writeline(stream: Rep[ForgeFileOutputStream], line: Rep[String])(implicit ctx: SourceContext): Rep[Unit]
  def forge_fileoutputstream_close(stream: Rep[ForgeFileOutputStream])(implicit ctx: SourceContext): Rep[Unit]
}

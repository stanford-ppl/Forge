package LOWERCASE_DSL_NAME.library

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

trait InputOutputWrapper extends HUMAN_DSL_NAMEBase {
  this: ForgeArrayWrapper with ForgeArrayBufferWrapper =>

  def forge_filereader_readlines[A:Manifest](path: Rep[String], f: Rep[String] => Rep[A]): Rep[ForgeArray[A]] = {
    forge_filereader_readlines_unstructured(path, (line, buf) => array_buffer_append(buf, f(line)))
  }

  def forge_filereader_readlines_flattened[A:Manifest](path: Rep[String], f: Rep[String] => Rep[ForgeArray[A]]): Rep[ForgeArray[A]] = {
    forge_filereader_readlines_unstructured(path, (line, buf) => {
      val a = f(line)
      for (i <- 0 until array_length(a)) {
        array_buffer_append(buf, a(i))
      }
    })
  }

  def forge_filereader_readlines_unstructured[A:Manifest](path: Rep[String], append: (Rep[String], Rep[ForgeArrayBuffer[A]]) => Rep[Unit]): Rep[ForgeArray[A]] = {
    val file = new java.io.File(path)
    val input = new java.io.BufferedReader(new java.io.FileReader(file))
    val out = new ForgeArrayBuffer[A](0)
    var line = input.readLine()
    while (line != null) {
      append(line, out)
      line = input.readLine()
    }
    input.close()
    out.toArray
  }
}





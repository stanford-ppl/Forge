package ppl.dsl.forge
package templates

import java.io.PrintWriter

object Utilities {
  def emitWithIndent(s: String, stream: PrintWriter, indent: Int = 0) = stream.println(" "*indent + s)
  def emitWithIndentInline(s: String, stream: PrintWriter, indent: Int = 0) = stream.print(" "*indent + s)
  
  def emitComment(s: String, stream: PrintWriter, indent: Int = 0) = emitWithIndent("// " + s, stream, indent)
  def emitBlockComment(s: String, stream: PrintWriter, indent: Int = 0) = {
    emitWithIndent("/**", stream, indent)
    emitWithIndentInline(" * ", stream, indent)
    stream.println(s)
    emitWithIndent(" */", stream, indent)
  } 
}
package ppl.dsl.forge
package templates
package doc

import java.io.{File,PrintWriter,FileWriter}
import scala.virtualization.lms.common._

import core._
import shared._
import Utilities._

trait ForgeGenReStructuredText extends ForgeCodeGenDocBase {
  // Text formatting
  def emph(x: String) = "*" + x + "*"
  def bold(x: String) = "**" + x + "**"
  def lit(x: String) = "``" + x + "``"
  def sub(x: String) = "\\ :sub:`" + x + "`\\ "
  def sup(x: String) = "\\ :sup:`" + x + "` \\ "

  def hrule() = "\n*********\n"
  // Headings
  def part(x: String) = "#" * x.length + "\n" + x + "\n" + "#"*x.length
  def chap(x: String) = "*" * x.length + "\n" + x + "\n" + "*"*x.length
  def sect(x: String) = x + "\n" + "="*x.length
  def subsect(x: String) =  x + "\n" + "-"*x.length
  def subsubsect(x: String) = x + "\n" + "^"*x.length

  //val keywordColor =
  /*val keywords = "val|var|def|implicit|for|do|while|if|else|override|private|protected|trait|class|abstract|case|match|with|extends|import|package|try|catch|finally|".r
  val TypePattern = "([A-Z]\\w*)".r
  val ArgPattern = "(\\w+)\\s*:"

  def highlight(x: String) = {
    var res = x
    keywords.findAllIn(res).foreach{m => }
  }*/

  override def code(x: String) = ".. parsed-literal::\n\n  " + x + "\n" //":code:`" + x + "`"
  def code(x: List[String]) = ".. parsed-literal::\n  " + x.map(line => "  " + line).mkString("\n") + "\n"
  //"::\n\n" + x.map(line => "  " + line).mkString("\n") + "\n"

  private val specialChars = List("|", "/", "*", "_", "[", "]")

  override def escapeSpecial(x: String) = {
    var out = x
    specialChars.foreach{c => out = out.replace(c, "\\"+c) }
    out
  }

  def link(x: String, path: String) = {
    //if (inCodeBlock) "`:doc:`"+x+" </"+path+">`:code:`"
    ":doc:`"+x+" </"+path+">`"
  }
  def docref(path:String) = ":ref:`"+path+"`"
  def ref(path: String) = path + "_"
  def label(path: String) = ".. _" + path + ":"
}

package ppl.dsl.forge
package format

import java.io.{PrintWriter}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import core._

trait PrinterOps extends Base {
  this: Forge =>
  
  /**
   * Output formatters
   */  
  
  /**
   * Using a named Printer for printLines instead of a static method in case we eventually want
   * to add state to make the formatting more flexible, i.e. addLines and/or removeLines methods.
   * This would allow users to compose streams over time instead of in a single method call, but
   * is it really necessary?
   */
  abstract class ForgePrinter
  
  object ForgePrinter {
    def apply(name: String = "default") = forge_printer(name)
  }

  def infix_printLines(x: Rep[ForgePrinter], lines: Rep[String]*) = forge_printer_printlines(x,lines.toList)
  def infix_name(x: Rep[ForgePrinter])(implicit o: Overloaded5) = forge_printer_name(x)
      
  def forge_printer(name: String): Rep[ForgePrinter]  
  def forge_printer_name(x: Rep[ForgePrinter]): String
  def forge_printer_printlines(x: Rep[ForgePrinter], lines: List[Rep[String]]): Rep[String]
}

trait PrinterOpsExp extends PrinterOps {
  this: ForgeExp =>

  case class AForgePrinter(name: String) extends Def[ForgePrinter]
  
  def forge_printer(name: String) = AForgePrinter(name)
  
  def forge_printer_name(x: Rep[ForgePrinter]) = x match {
    case Def(AForgePrinter(name)) => name
  }
  
  case class PrintLines(x: Rep[ForgePrinter], lines: List[Rep[String]]) extends Def[String]
  def forge_printer_printlines(x: Rep[ForgePrinter], lines: List[Rep[String]]) = PrintLines(x,lines)
}
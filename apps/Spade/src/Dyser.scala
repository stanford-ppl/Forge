import spade.compiler._
import spade.library._
import spade.shared._
import scala.collection.mutable.ListBuffer

object DyserCompiler extends SpadeApplicationCompiler with Dyser
object DyserInterpreter extends SpadeApplicationInterpreter with Dyser

trait Dyser extends SpadeApplication {

  override def stageArgNames = List("rows", "cols")
  lazy val rows = stageArgOrElse[Int](0, 4)  // <-- Breaks when staged arguments are passed with delitec. Why?
  lazy val cols = stageArgOrElse[Int](1, 4)  // <-- Breaks when staged arguments are passed with delitec. Why?

  def dyser(rows: Int, cols: Int)  {
    val switches = ListBuffer[ListBuffer[Rep[Switch]]]()
    for (i <- 0 to rows) {
      val switchRow = ListBuffer[Rep[Switch]]()
      for (j <- 0 to cols) {
        switchRow.append(Switch())
        if (j > 0) switchRow(j-1) <-> switchRow(j)
        if (i > 0) switches(i-1)(j) <-> switchRow(j)
      }
      switches.append(switchRow)
      if (i > 0) {
        for (k <- 0 until cols) {
          val a = ALU()
          a <-> switches(i-1)(k)
          a <-> switches(i-1)(k+1)
          a <-> switches(i)(k)
          a <-> switches(i)(k+1)
        }
      }
    }
  }

  def main() = {
    val r = rows
    val c = cols
    dyser(r, c)
  }
}

import spade.compiler._
import spade.library._
import spade.shared._
import scala.collection.mutable.ListBuffer

object DyserCompiler extends SpadeApplicationCompiler with Dyser
object DyserInterpreter extends SpadeApplicationInterpreter with Dyser

trait Dyser extends SpadeApplication {
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
    if (stagingArgs.length < 2) {
      println("Usage: DyserCompiler <rows> <cols>")
      sys.exit(-1)
    }
    val rows = stagingArgs(0).toInt
    val cols = stagingArgs(1).toInt
    dyser(rows, cols)
  }
}

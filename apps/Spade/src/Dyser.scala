import spade.compiler._
import spade.library._
import spade.shared._
import scala.collection.mutable.ListBuffer

object DyserCompiler extends SpadeApplicationCompiler with Dyser
object DyserInterpreter extends SpadeApplicationInterpreter with Dyser

trait Dyser extends SpadeApplication {
  def main() = {
    val a1 = ALU()
    val a2 = ALU()
    a1 -> a2
  }
}

import dadl.compiler._
import dadl.library._
import dadl.shared._
import scala.collection.mutable.ListBuffer

object DADLTestCompiler extends DADLApplicationCompiler with DADLTest
object DADLTestInterpreter extends DADLApplicationInterpreter with DADLTest

trait DADLTest extends DADLApplication {
	def main() = {
    val aluList = Listbuffer[ListBuffer[ALU]]()
    for (i <- 0 until 16) {
      aluList.append(ListBuffer[ALU]())

      for (j <- 0 until 16) {
        val a = ALU(32)
        aluList(i).append(a)
        if (i > 0) aluList(i-1)(j) -> a
        if (j > 0) aluList(j-1)(i) -> a
      }
    }
	}
}

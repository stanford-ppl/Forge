import dadl.compiler._
import dadl.library._
import dadl.shared._

object DADLTestCompiler extends DADLApplicationCompiler with DADLTest
object DADLTestInterpreter extends DADLApplicationInterpreter with DADLTest

trait DADLTest extends DADLApplication {
	def main() = {
		val bm = BRAM[Long]("bm", 128)

    def Mux(sel: Rep[Boolean], x: Rep[Int], y: Rep[Int]): Rep[Int] = if (sel) x else y

    val in0 = unit(0)
    val in1 = unit(1)
    val sel = unit(true)

    val out = instance(Mux)(sel, in0, in1)
	}
}

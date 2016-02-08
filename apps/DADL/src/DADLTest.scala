import dadl.compiler._
import dadl.library._
import dadl.shared._

object DADLTestCompiler extends DADLApplicationCompiler with DADLTest
object DADLTestInterpreter extends DADLApplicationInterpreter with DADLTest

trait DADLTest extends DADLApplication {
	def main() = {
		val bm = BRAM[Long]("bm", 128)
	}
}

import dadl.compiler._
import dadl.library._
import dadl.shared._

object DADLTestCompiler extends DADLApplicationCompiler with DADLTest
object DADLTestInterpreter extends DADLApplicationInterpreter with DADLTest

trait DADLTest extends DADLApplication {
	def main() = {
    /*
		val bm = BRAM[Long]("bm", 1, 128)

    def Mux(sel: Wire[Boolean], x: Wire[Int], y: Wire[Int]): Wire[Int] = x
    def Fanout(in: Wire[Int]): Wire[Tup2[Int,Int]] = pack((in, in))

    val in0 = unit(0)
    val in1 = unit(1)
    val sel = unit(true)

    // Inline a module
    val inline = Mux(sel, in0, in1)
    println(inline)

    // Create "instances" of modules
    val out = instance(Mux _)(sel, in0, in1)
    val (a,b) = instance(Fanout _)(in1)
    println(out)
    println(a)
    println(b)
    */
   val v1 = ALU(32)
   val v2 = ALU(32)
   v1 -> v2
	}
}

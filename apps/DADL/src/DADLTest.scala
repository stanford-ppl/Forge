import dadl.compiler._
import dadl.library._
import dadl.shared._

object DADLTestCompiler extends DADLApplicationCompiler with DADLTest
object DADLTestInterpreter extends DADLApplicationInterpreter with DADLTest

trait DADLTest extends DADLApplication {
	def main() = {
		val bm = BRAM[Long]("bm", 128)

    def Mux[T](sel: Wire[Boolean], x: Wire[T], y: Wire[T]): Wire[T] = x
    def Fanout(in: Wire[Int]): Wire[Tup2[Int,Int]] = pack((in, in))

    val in0 = unit(0)
    val in1 = unit(1)
    val sel = unit(true)

    // Inline a module
    val inline = Mux(sel, in0, in1)
    println(inline)

    // Create "instances" of modules
    val out = instance(Mux[Int] _)(sel, in0, in1)
    val (a,b) = instance(Fanout _)(in1)
    println(out)
    println(a)
    println(b)

    // Unforunately can't do type ALU[T] = Record[T]{val in1: T; val in2: T; val out: T}

    def ALU[T:Manifest](bits: Int) = {
      val i1 = input[T]
      val i2 = input[T]
      val o  = instance(Mux[T] _)(sel, i1, i2) // Some function of i1 and i2

      new IO { val in1 = i1; val in2 = i2; val out = o; val out2 = .. }
    }


    // Should take care not to assign things to the output of a module, or have unconnected inputs

	}
}

import spade.compiler._
import spade.library._
import spade.shared._
import scala.collection.mutable.ListBuffer

object SpadeTestCompiler extends SpadeApplicationCompiler with SpadeTest
object SpadeTestInterpreter extends SpadeApplicationInterpreter with SpadeTest

trait SpadeTest extends SpadeApplication {
  def main() = {
//    val bm = BRAM[Long]("bm", 128)
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

    val alu1 = feedback[Int]

    def ALU[T:Manifest](in1: Wire[T], in2: Wire[T]) = {
      instance(Mux[T] _)(sel, in1, in2) // Some function of i1 and i2
    }

    alu1 = instance(ALU[Int] _)(alu1, in1)

    //println(alu1)

    // Should take care not to assign things to the output of a module, or have unconnected inputs
  }
}

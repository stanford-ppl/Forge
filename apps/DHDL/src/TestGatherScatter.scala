import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object TestGatherScatterCompiler extends DHDLApplicationCompiler with TestGatherScatter
object TestGatherScatterInterpreter extends DHDLApplicationInterpreter with TestGatherScatter
trait TestGatherScatter extends DHDLApplication {

  def main() {
    type Elem = Flt //FixPt[Signed, B16, B16]
    val N = 10

    val v1    = OffChipMem[Elem](N)
    val v2    = OffChipMem[Elem](N)

    val vec1 = Array.fill(N)(random[Elem](N))
    setMem(v1, vec1)
    println("v1:" + getMem(v1).mkString(","))

    val sAddr = Array(3,2,5,1,4)

    Accel {
      val addr = BRAM[Index](5)
      val value = BRAM[Elem](5)
      Sequential {
        setBram(addr, sAddr)
        println("addrs:" + getBram(addr).mkString(","))
        value := v1(addr)
        println("values:" + getBram(value).mkString(","))
        println(value)
        v2(addr) := value
      }
      Pipe {val a = 3} //TODO: break contension model if remove this?
    }

    val gold = Array.fill(N)(0.as[Elem])
    sAddr.map { a =>
      gold(a) = vec1(a)
      print("")
    }
    println("v2: " + getMem(v2).mkString(",") + "\n (should be " + gold.mkString(",") + ")")
  }
}

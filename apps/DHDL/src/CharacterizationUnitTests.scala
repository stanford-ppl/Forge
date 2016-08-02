import dhdl.compiler._
import dhdl.library._
import dhdl.shared._


object CharLoadTest extends DHDLApplicationCompiler with CharLoad
trait CharLoad extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  def CharLoad(srcHost: Rep[Array[T]]) = {
    val innerPar = param("innerPar", 1); domainOf(innerPar) = (1, 1, 1)
    val tileSize = param("tileSize", 96); domainOf(tileSize) = (96, 96, 96)
    val len = srcHost.length; bound(len) = 9216

    val N = ArgIn[SInt]
    val out = ArgOut[SInt]
    setArg(N, len)

    val srcFPGA = OffChipMem[SInt](N)
    setMem(srcFPGA, srcHost)

    Accel {
      val f1 = FIFO[SInt](tileSize)
      Sequential(N by tileSize) { i =>
        f1 := srcFPGA(i::i+tileSize)
        Pipe { out := f1.pop }
      }
      ()
    }

    getArg(out)
  }

  def main() {
    val len = args(unit(0)).to[T]

    val src = Array.tabulate[T](len) { i => i }
    val result = CharLoad(src)

    val gold = src(0)
    println("expected: " + gold)
    println("result:   " + result)
    assert(result == gold)
  }
}


object CharBRAMTest extends DHDLApplicationCompiler with CharBRAM
trait CharBRAM extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  def CharBRAM(srcHost: Rep[Array[T]]) = {
    val tileDim0 = param(96);
    val tileDim1 = param(96);

    val innerPar = param("innerPar", 1); domainOf(innerPar) = (1, 1, 1)

    val out = ArgOut[SInt]

    Accel {
      Sequential(tileDim0 by 1, tileDim1 by 1) { (i,j) =>
        val tile = BRAM[T](tileDim0, tileDim1)
        tile(i,j) = i+j
        Pipe { out := tile(i,j) }
      }
    }

    getArg(out)
  }

  def main() {
    val len = args(unit(0)).to[T]

    val src = Array.tabulate[T](len) { i => i }
    val result = CharBRAM(src)

    val gold = src(0)
    println("expected: " + gold)
    println("result:   " + result)
    assert(result == gold)
  }
}

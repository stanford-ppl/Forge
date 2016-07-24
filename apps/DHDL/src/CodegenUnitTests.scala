import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object SimpleSequentialTest extends DHDLApplicationCompiler with SimpleSequential
trait SimpleSequential extends DHDLApplication {
  type Array[T] = ForgeArray[T]

  def simpleseq(xin: Rep[SInt], yin: Rep[SInt]) = {
    val innerPar = param("innerPar", 1); domainOf(innerPar) = (1, 1, 1)
    val tileSize = param("tileSize", 96); domainOf(tileSize) = (96, 96, 96)

    val x = ArgIn[SInt]
    val y = ArgIn[SInt]
    val out = ArgOut[SInt]
    setArg(x, xin)
    setArg(y, yin)

    Accel {
      val b1 = BRAM[SInt](tileSize)
//      Sequential (tileSize by tileSize) { i =>
      Sequential {
        Pipe.foreach(tileSize par innerPar) { ii =>
          b1(ii) = x.value * ii
        }
        Pipe { out := b1(y) }
      }
      ()
    }
    getArg(out)
  }

  def main() {
    val x = args(unit(0)).to[SInt]
    val y = args(unit(1)).to[SInt]

    val result = simpleseq(x, y)

    val b1 = Array.tabulate[SInt](96) { i => x * i }
    val gold = b1(y)
    println("expected: " + gold)
    println("result:   " + result)
    assert(result == gold)
  }
}

object ArgInOutTest extends DHDLApplicationCompiler with ArgInOut
trait ArgInOut extends DHDLApplication {

  def main() {
    val N = 8
  	val x = ArgIn[SInt]
  	val y = ArgOut[SInt]
    setArg(x, N)
    Accel {
      Pipe { y := x + 4 }
    }
    val result = getArg(y)
    println("result = " + result)
  }
}

object DeviceMemcpyTest extends DHDLApplicationCompiler with DeviceMemcpy
trait DeviceMemcpy extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]

  def memcpyViaFPGA(srcHost: Rep[Array[T]]) = {
    val N = srcHost.length
    val fpgamem = OffChipMem[SInt](N)
    setMem(fpgamem, srcHost)

  	val y = ArgOut[SInt]
    Accel { Pipe { y := 10 } }

    getMem(fpgamem)
  }

  def main() {
    val arraySize = args(unit(0)).to[SInt]

    val src = Array.tabulate[SInt](arraySize) { i => i }
    val dst = memcpyViaFPGA(src)

    println("src");
    (0 until arraySize) foreach { i => print(src(i) + " ") }
    println("dst");
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

//    println("dst"); println(dst.mkString(" "))
  }

}

object SimpleTileLoadStoreTest extends DHDLApplicationCompiler with SimpleTileLoadStore
trait SimpleTileLoadStore extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  def simpleLoadStore(a1Host: Rep[Array[T]], a2Host: Rep[Array[T]]) = {
    val loadPar = param("loadPar", 1); domainOf(loadPar) = (1, 1, 1)
    val storePar = param("storePar", 1); domainOf(storePar) = (1, 1, 1)
    val tileSize = param("tileSize", 96); domainOf(tileSize) = (96, 96, 96)
    val N = a1Host.length; bound(N) = 9216

    val a1FPGA = OffChipMem[SInt](N)
//    val a2FPGA = OffChipMem[SInt](N)
    setMem(a1FPGA, a1Host)
//    setMem(a2FPGA, a2Host)
    Accel {
      val b1 = BRAM[SInt](tileSize)
//      Sequential {
        b1 := a1FPGA(0::tileSize)
//        a2FPGA(0::tileSize) := b1
//      }
      ()
    }
//    getMem(a2FPGA)
  }

  def main() {
    val arraySize = args(unit(0)).to[SInt]

    val a1 = Array.tabulate[SInt](arraySize) { i => i }
    val a2 = Array.fill[SInt](arraySize) { 0 }
    simpleLoadStore(a1, a2)
//    val result = simpleLoadStore(a1, a2)
//    (0 until result.length) foreach { i => println(s"""result($i) = ${result(i)}""") }
//    val gold = a1
//    assert(result == gold)
  }
}


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
    val c = args(unit(1)).to[SInt]

    val src = Array.tabulate[SInt](arraySize) { i => i*c }
    val dst = memcpyViaFPGA(src)

    println("src")
    (0 until arraySize) foreach { i => print(src(i) + " ") }
    println("dst")
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

//    println("dst"); println(dst.mkString(" "))
  }

}

object SimpleTileLoadStoreTest extends DHDLApplicationCompiler with SimpleTileLoadStore
trait SimpleTileLoadStore extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  def simpleLoadStore(srcHost: Rep[Array[T]], value: Rep[SInt]) = {
    val loadPar = param("loadPar", 1); domainOf(loadPar) = (1, 1, 1)
    val storePar = param("storePar", 1); domainOf(storePar) = (1, 1, 1)
    val tileSize = param("tileSize", 96); domainOf(tileSize) = (96, 96, 96)
    val N = srcHost.length; bound(N) = 9216

    val srcFPGA = OffChipMem[SInt](N)
    val dstFPGA = OffChipMem[SInt](N)
    setMem(srcFPGA, srcHost)

  	val x = ArgIn[SInt]
    setArg(x, value)
    Accel {
      val b1 = BRAM[SInt](tileSize)
      Sequential {
        b1 := srcFPGA(0::tileSize)

        val b2 = BRAM[SInt](tileSize)
        Pipe (tileSize by 1) { i =>
          b2(i) = b1(i) * x
        }

        dstFPGA(0::tileSize) := b2
      }
      ()
    }
    getMem(dstFPGA)
  }

  def main() {
    val arraySize = args(unit(0)).to[SInt]
    val value = args(unit(1)).to[SInt]

    val src = Array.tabulate[SInt](arraySize) { i => i }
    val dst = simpleLoadStore(src, value)

    val gold = src.map { _ * value }

    println("dst:")
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

    (0 until arraySize) foreach { i => assert(dst(i) == gold(i)) }
  }
}


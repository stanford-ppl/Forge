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

  	val size = ArgIn[SInt]
  	val x = ArgIn[SInt]
    setArg(x, value)
    setArg(size, N)
    Accel {
      val b1 = BRAM[SInt](tileSize)
      Sequential(size by tileSize) { i =>
        b1 := srcFPGA(i::i+tileSize)

        val b2 = BRAM[SInt](tileSize)
        Pipe (tileSize by 1) { ii =>
          b2(ii) = b1(ii) * x
        }

        dstFPGA(i::i+tileSize) := b2
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

object FifoLoadTest extends DHDLApplicationCompiler with FifoLoad
trait FifoLoad extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  def fifoLoad(srcHost: Rep[Array[T]]) = {
    val tileSize = param("tileSize", 96); domainOf(tileSize) = (96, 96, 96)
    val N = srcHost.length; bound(N) = 9216

    val srcFPGA = OffChipMem[SInt](N)
    val dstFPGA = OffChipMem[SInt](N)
    setMem(srcFPGA, srcHost)

    Accel {
      val f1 = FIFO[SInt](tileSize)
      Sequential {
        f1 := srcFPGA(0::tileSize)
        val b1 = BRAM[SInt](tileSize)
        Pipe(tileSize by 1) { i =>
          b1(i) = f1.pop
        }
        dstFPGA(0::tileSize) := b1
      }
      ()
    }
    getMem(dstFPGA)
  }

  def main() {
    val arraySize = args(unit(0)).to[SInt]

    val src = Array.tabulate[SInt](arraySize) { i => i }
    val dst = fifoLoad(src)

    val gold = src.map { i => i }

    println("dst:")
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

    (0 until arraySize) foreach { i => assert(dst(i) == gold(i)) }
  }
}

object ParFifoLoadTest extends DHDLApplicationCompiler with ParFifoLoad
trait ParFifoLoad extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  def parFifoLoad(src1: Rep[Array[T]], src2: Rep[Array[T]]) = {
    val tileSize = param("tileSize", 96); domainOf(tileSize) = (96, 96, 96)
    val N = src1.length; bound(N) = 9216

    val src1FPGA = OffChipMem[T](N)
    val src2FPGA = OffChipMem[T](N)
    val in = ArgIn[T]
    val out = ArgOut[T]
    setArg(in, N)
    setMem(src1FPGA, src1)
    setMem(src2FPGA, src2)

    Accel {
      val f1 = FIFO[T](tileSize)
      val f2 = FIFO[T](tileSize)
      Pipe (in by tileSize) { i =>
        Parallel {
          f1 := src1FPGA(i::i+tileSize)
          f2 := src2FPGA(i::i+tileSize)
        }
        val accum = Reduce(tileSize by 1)(0.as[T]) { i =>
          f1.pop() * f2.pop()
        }{_+_}
        Pipe { out := accum }
      }
      ()
    }
    getArg(out)
  }

  def main() {
    val arraySize = args(unit(0)).to[SInt]

    val src1 = Array.tabulate[SInt](arraySize) { i => i }
    val src2 = Array.tabulate[SInt](arraySize) { i => i*2 }
    val out = parFifoLoad(src1, src2)

//    val gold = ((arraySize - 96 until arraySize)) map { i => src1(i) * src2(i) }.reduce{_+_}
//    println(s"out = " + out + ",gold = " + gold)
    println(s"out = " + out)

//    assert(out == gold)
  }
}

object FifoLoadStoreTest extends DHDLApplicationCompiler with FifoLoadStore
trait FifoLoadStore extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  def fifoLoadStore(srcHost: Rep[Array[T]]) = {
    val tileSize = param("tileSize", 96); domainOf(tileSize) = (96, 96, 96)
    val N = srcHost.length; bound(N) = 9216

    val srcFPGA = OffChipMem[SInt](N)
    val dstFPGA = OffChipMem[SInt](N)
    setMem(srcFPGA, srcHost)

    Accel {
      val f1 = FIFO[SInt](tileSize)
      Parallel {
        Sequential {
          f1 := srcFPGA(0::tileSize)
          dstFPGA(0::tileSize) := f1
        }
        Pipe(tileSize by 1) { i => }
      }
      ()
    }
    getMem(dstFPGA)
  }

  def main() {
    val arraySize = args(unit(0)).to[SInt]

    val src = Array.tabulate[SInt](arraySize) { i => i }
    val dst = fifoLoadStore(src)

    val gold = src.map { i => i }

    println("dst:")
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

    (0 until arraySize) foreach { i => assert(dst(i) == gold(i)) }
  }
}

object SimpleReduceTest extends DHDLApplicationCompiler with SimpleReduce
trait SimpleReduce extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val constTileSize = 96
  def simplemap(xin: Rep[SInt]) = {
    val innerPar = param("innerPar", 1); domainOf(innerPar) = (1, 1, 1)
    val tileSize = param("tileSize", constTileSize); domainOf(constTileSize) = (constTileSize, constTileSize, constTileSize)

    val x = ArgIn[SInt]
    val out = ArgOut[SInt]
    setArg(x, xin)

    Accel {
      Sequential {
        val accum = Reduce (constTileSize par innerPar)(0.as[T]) { ii =>
          x.value * ii
        } {_+_}
        Pipe { out := accum }
      }
      ()
    }

    getArg(out)
  }

  def main() {
    val x = args(unit(0)).to[SInt]

    val result = simplemap(x)

    val b1 = Array.tabulate[SInt](constTileSize) { i => x * i }
    val gold = b1.reduce {_+_}
    println("expected: " + gold)
    println("result:   " + result)
    assert(result == gold)
  }
}

object NiterTest extends DHDLApplicationCompiler with Niter
trait Niter extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val constTileSize = 96

  def nIterTest(len: Rep[SInt]) = {
    val innerPar = param("innerPar", 1); domainOf(innerPar) = (1, 1, 1)
    val tileSize = param("tileSize", constTileSize); domainOf(constTileSize) = (constTileSize, constTileSize, constTileSize)
    bound(len) = 9216

    val N = ArgIn[SInt]
    val out = ArgOut[SInt]
    setArg(N, len)

    Accel {
      Sequential {
        Sequential (N by tileSize) { i =>
          val accum = Reduce (tileSize par innerPar)(0.as[T]) { ii =>
            i * ii
          } {_+_}
          Pipe { out := accum }
        }
      }
      ()
    }

    getArg(out)
  }

  def main() {
    val len = args(unit(0)).to[SInt]

    val result = nIterTest(len)

    val b1 = Array.tabulate[SInt](len) { i => i }

    val gold = b1.reduce {_+_}
    println("expected: " + gold)
    println("result:   " + result)
    assert(result == gold)
  }
}

object SimpleFoldTest extends DHDLApplicationCompiler with SimpleFold
trait SimpleFold extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val constTileSize = 96

  def nIterTest(src: Rep[Array[T]]) = {
    val innerPar = param("innerPar", 1); domainOf(innerPar) = (1, 1, 1)
    val tileSize = param("tileSize", constTileSize); domainOf(constTileSize) = (constTileSize, constTileSize, constTileSize)
    val len = src.length; bound(len) = 9216

    val N = ArgIn[T]
    val out = ArgOut[T]
    setArg(N, len)

    val v1 = OffChipMem[T](N)
    setMem(v1, src)

    Accel {
      Sequential {
        val accum = Reg[T]
        Fold (N by tileSize)(accum, 0.as[T]) { i =>
          val b1 = BRAM[T](tileSize)
          b1 := v1(i::i+tileSize)
          Reduce (tileSize par innerPar)(0.as[T]) { ii =>
            b1(ii)
          } {_+_}
        } {_+_}
        Pipe { out := accum }
      }
      ()
    }

    getArg(out)
  }

  def main() {
    val len = args(unit(0)).to[T]

    val src = Array.tabulate[T](len) { i => i }
    val result = nIterTest(src)

    val gold = src.reduce {_+_}
    println("expected: " + gold)
    println("result:   " + result)
    assert(result == gold)
  }
}

object Memcpy2DTest extends DHDLApplicationCompiler with Memcpy2D
trait Memcpy2D extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]

  def memcpy_2d(src: Rep[ForgeArray[T]], rows: Rep[SInt], cols: Rep[SInt]) = {
    val tileDim1 = param(2);
    val tileDim2 = param(96);  domainOf(tileDim2) = (96, 96, 96)

    val rowsIn = ArgIn[SInt]
    val colsIn = ArgIn[SInt]

    val srcFPGA = OffChipMem[T](rows, cols)
    val dstFPGA = OffChipMem[T](rows, cols)

    // Transfer data and start accelerator
    setArg(rowsIn, rows)
    setArg(colsIn, cols)
    setMem(srcFPGA, src)

    Accel {
      Sequential(rowsIn by tileDim1, colsIn by tileDim2) { (i,j) =>
        val tile = BRAM[T](tileDim1, tileDim2)
        tile := srcFPGA(i::i+tileDim1, j::j+tileDim2)
        dstFPGA (i::i+tileDim1, j::j+tileDim2) := tile
      }
    }
    getMem(dstFPGA)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() = {
    val rows = args(0).to[SInt]
    val cols = args(1).to[SInt]
    val src = Array.tabulate(rows*cols) { i => i }

    val dst = memcpy_2d(src, rows, cols)

    printArr(src, "src:")
    printArr(dst, "dst:")
    (0 until rows*cols) foreach { i => assert(dst(i) == src(i)) }
  }
}

object BlockReduce1DTest extends DHDLApplicationCompiler with BlockReduce1D
trait BlockReduce1D extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]

  val tileSize = 96

  def blockreduce_1d(src: Rep[ForgeArray[T]], size: Rep[SInt]) = {

    val sizeIn = ArgIn[SInt]; setArg(sizeIn, size)
    val colsIn = ArgIn[SInt]

    val srcFPGA = OffChipMem[T](size)
    val dstFPGA = OffChipMem[T](tileSize)

    setMem(srcFPGA, src)

    Accel {
      val accum = BRAM[T](tileSize)
      Fold (sizeIn by tileSize)(accum, 0.as[T]) { i  =>
        val tile = BRAM[T](tileSize)
        tile := srcFPGA(i::i+tileSize)
        tile
      }{_+_}
      dstFPGA (0::tileSize) := accum
    }
    getMem(dstFPGA)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() = {
    val size = args(0).to[SInt]
    val src = Array.tabulate(size) { i => i }

    val dst = blockreduce_1d(src, size)

//    val gold = Array.tabulate(tileSize) { i =>
////      {for(ii <- i until size by tileSize)  yield src(ii) }.reduce{_+_}
//      (i until size by tileSize).map { ii => src(ii) }.reduce{_+_}
//    }

    printArr(src, "src:")
    printArr(dst, "dst:")
//    (0 until tileSize) foreach { i => assert(dst(i) == gold(i)) }
  }
}

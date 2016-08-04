import dhdl.compiler._
import dhdl.library._
import dhdl.shared._


object CharLoadTest extends DHDLApplicationCompiler with CharLoad
trait CharLoad extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  def CharLoad(srcHost: Rep[Array[T]], iters: Rep[SInt]) = {
    val sinnerPar = param("innerPar", 4); 
    val souterPar = param("outerPar", 2);
    val innerPar = 4; 
    val outerPar = 2;
    val tileSize0 = param("tileSize0", 2); 
    val tileSize1 = param("tileSize1", 24480); 
    val dim0 = 2; 
    val dim1 = 24480; 

    val N = ArgIn[SInt]
    val out = List.tabulate(outerPar){i => ArgOut[SInt] }

    setArg(N, iters)

    val srcFPGA = OffChipMem[SInt](tileSize0, tileSize1)
    setMem(srcFPGA, srcHost)

    Accel {
      Sequential (N by 1) { ii =>
        val dummy = List.tabulate(outerPar){i => 
          BRAM[SInt](tileSize0, tileSize1)
        }
        dummy.foreach {dum =>
          isDummy(dum) = true
        }
        Parallel {
          dummy.zipWithIndex.foreach{ case (dum, i) =>
            Pipe {dum := srcFPGA(i*dim0::(i+1)*dim0, i*dim1::(i+1)*dim1, sinnerPar)}
          }
        }
        Parallel {
          dummy.zip(out).foreach { case (dum, o) =>
            Pipe {o := dum(0,0)}
          }
        }
      }
      ()
    }
    out.map { m =>
      getArg(m)
    }
  }

  def main() {
    val iters = args(unit(0)).to[T]

    val src = Array.tabulate[T](12288) { i => i }
    val result = CharLoad(src, iters)

    val gold = src(0)
    println("expected: <Last loaded number>")
    println("result:   " + result)
  }
}

object CharStoreTest extends DHDLApplicationCompiler with CharStore
trait CharStore extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  def CharStore(iters: Rep[T], numin: Rep[T]) = {
    val sinnerPar = param("innerPar", 4); 
    val souterPar = param("outerPar", 2);
    val innerPar = 4; 
    val outerPar = 2;
    val tileSize0 = param("tileSize0", 2); 
    val tileSize1 = param("tileSize1", 24480); 
    val dim0 = 2; 
    val dim1 = 24480; 

    val N = ArgIn[SInt]
    val num = ArgIn[SInt]
    setArg(N, iters)
    setArg(num, numin)
    val dstFPGA = List.tabulate(outerPar){i => OffChipMem[SInt](tileSize0, tileSize1) }

    Accel {
      Sequential (N by 1) { ii =>
        val dummy = List.tabulate(outerPar){i => 
          BRAM[SInt](tileSize0, tileSize1)
        }
        dummy.foreach {dum =>
          isDummy(dum) = true
        }
        Parallel {
          dummy.foreach{ case dum =>
            Pipe {dum(0,0) = num.value}
          }
        }
        Parallel {
          dummy.zip(dstFPGA).zipWithIndex.foreach{ case ((dum, dst), i) =>
            dst (i*dim0::(i+1)*dim0, i*dim1::(i+1)*dim1, sinnerPar) := dum
          }
        }
      }
      ()
    }
    dstFPGA.map { m =>
      getMem(m)
    }

  }

  def main() {
    val len = args(unit(0)).to[T]
    val num = args(unit(1)).to[T]

    val result = CharStore(len, num)

    println("expected: sequential stuff")
    println("result:   " + result)
  }
}


object CharBRAMTest extends DHDLApplicationCompiler with CharBRAM
trait CharBRAM extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  def CharBRAM(numin: Rep[T]) = {
    val tileDim0 = param(96);
    val tileDim1 = param(96);
    val spar0 = param(1);
    val spar1 = param(1);
    val par0 = 1;
    val par1 = 1;

    val num = ArgIn[SInt]
    setArg(num, numin)

    val out = List.tabulate(1*1){i => ArgOut[SInt] }

    Accel {
      val tile = BRAM[T](tileDim0, tileDim1)
      Pipe (tileDim0 by 1 par spar0, tileDim1 by 1 par spar1) { (i,j) =>
        tile(i,j) = num
      }
      val fifo = FIFO[T](987643) // Grep for this in maxj and replace with argout connections
    }

    out.map{ i =>
      getArg(i)
    }
  }

  def main() {
    val numin = args(unit(0)).to[T]

    val result = CharBRAM(numin)

    println("expected: As many nums as par0*par1")
    println("result:   " + result)
  }
}

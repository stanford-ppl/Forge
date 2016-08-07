import dhdl.compiler._
import dhdl.library._
import dhdl.shared._


object CharLoadTest extends DHDLApplicationCompiler with CharLoad
trait CharLoad extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val dim0 = 2; 
  val dim1 = 24480; 
  val outerPar = 4;

  def CharLoad(srcHost: Rep[Array[T]], iters: Rep[SInt]) = {
    val sinnerPar = param("innerPar", 2); 
    val tileSize0 = param("tileSize0", dim0); 
    val tileSize1 = param("tileSize1", dim1); 

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

    val src = Array.tabulate[T](dim0*dim1*outerPar) { i => i }
    val result = CharLoad(src, iters)

    val gold = src(0)
    println("expected 0's followed by dim0*dim1 : ")
    result.foreach{println(_)}
  }
}

object CharStoreTest extends DHDLApplicationCompiler with CharStore
trait CharStore extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val dim0 = 8; 
  val dim1 = 1536; 
  def CharStore(iters: Rep[T], numin: Rep[T]) = {
    val sinnerPar = param("innerPar", 1); 
    val outerPar = 1;
    val tileSize0 = param("tileSize0", dim0); 
    val tileSize1 = param("tileSize1", dim1); 

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
            Pipe {dum(0,0) = num.value} // Template hack broadcasts this write to all banks
          }
        }
        Parallel {
          dummy.zip(dstFPGA).zipWithIndex.foreach{ case ((dum, dst), i) =>
            Pipe {dst (i*dim0::(i+1)*dim0, 0::dim1, sinnerPar) := dum}
          }
        }
      }
      ()
    }
    dstFPGA.map { m =>
      getMem(m)
    }

  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() {
    val len = args(unit(0)).to[T]
    val num = args(unit(1)).to[T]

    val result = CharStore(len, num)

    println("expected: sequential stuff")
    result.foreach{printArr(_, "dst: ")}
  }
}


object CharBramTest extends DHDLApplicationCompiler with CharBram
trait CharBram extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val par0 = 2;
  val par1 = 4;
  def CharBram(numin: Rep[T]) = {
    val tileDim0 = param(96);
    val tileDim1 = param(192);
    val spar0 = param(par0);
    val spar1 = param(par1);

    val num = ArgIn[SInt]
    setArg(num, numin)

    val out = List.tabulate(par0){i => List.tabulate(par1) {j => ArgOut[SInt] }}

    Accel {
      val tile = BRAM[T](tileDim0, tileDim1)
      Pipe (tileDim0 by 1 par spar0) { i =>
        Pipe (tileDim1 by 1 par spar1) { j =>
          tile(i,j) = num
        }
      }
      Parallel {
        out.zipWithIndex.foreach{ case (row, i) =>
          row.zipWithIndex.foreach{ case (o, j) => 
            Pipe { 
              val rd = tile(i, j) 
              if (i > 0 || j > 0) {instanceIndexOf(rd) = 0}
              Pipe {o := rd}
            }
          }
        }
      }
    }

    out.map { row =>
      row.map { m =>
        getArg(m)
      }
    }

  }

  def main() {
    val numin = args(unit(0)).to[T]

    val result = CharBram(numin)

    println("expected: As many arg1's as par0*par1 (" + par0 + "*" + par1 + "=" + par0*par1 + ") :")
    result.map{row =>
      row.foreach{println(_)}
    }
  }
}

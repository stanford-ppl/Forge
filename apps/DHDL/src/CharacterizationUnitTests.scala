import dhdl.compiler._
import dhdl.library._
import dhdl.shared._


object CharLoadTest extends DHDLApplicationCompiler with CharLoad
trait CharLoad extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val innerPar = 16;
  val outerPar = 4;
  val dim0 = 960;
  val dim1 = 960;

  def CharLoad(srcHost: Rep[Array[T]], iters: Rep[SInt]) = {
    val sinnerPar = param("innerPar", innerPar); 
    val tileSize0 = param("tileSize0", dim0); 
    val tileSize1 = param("tileSize1", dim1); 

    val N = ArgIn[SInt]
    val out = List.tabulate(outerPar){i => List.tabulate(innerPar) {j => ArgOut[SInt] }}

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
          out.zip(dummy).zipWithIndex.foreach { case ((row, dum), i) =>
            row.zipWithIndex.foreach { case (o, j) =>
              Pipe {
                val rd = dum(0, j) 
                if (j > 0) {instanceIndexOf(rd) = 0}
                Pipe {o := rd}
              }
            }
          }
        }
      }
      ()
    }
    out.map { row =>
      row.map { m =>
        getArg(m)
      }
    }
  }

  def main() {
    val iters = args(unit(0)).to[T]

    val src = Array.tabulate[T](dim0*dim1*outerPar) { i => i }
    val result = CharLoad(src, iters)

    val gold = src(0)
    println("expected " + outerPar*innerPar + " things in increments of something like " + dim0 + "*" + dim1 + " : ")
    result.map{row => row.foreach{println(_)}}
  }
}

object CharStoreTest extends DHDLApplicationCompiler with CharStore
trait CharStore extends DHDLApplication {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val innerPar = 16;
  val outerPar = 4; 
  val dim0 = 960; 
  val dim1 = 960;
  def CharStore(iters: Rep[T], numin: Rep[T]) = {
    val sinnerPar = param("innerPar", innerPar); 
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
  val innerPar = 16;
  val outerPar = 4;
  val dim0 = 960;
  val dim1 = 960;
  def CharBram(numin: Rep[T]) = {
    val tileDim0 = param(dim0);
    val tileDim1 = param(dim1);
    val spar0 = param(outerPar);
    val spar1 = param(innerPar);

    val num = ArgIn[SInt]
    setArg(num, numin)

    val out = List.tabulate(outerPar){i => List.tabulate(innerPar) {j => ArgOut[SInt] }}

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
              val rd = tile(i,j) 
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

    println("expected: As many arg1's as innerPar*outerPar (" + innerPar + "*" + outerPar + "=" + innerPar*outerPar + ") :")
    result.map{row =>
      row.foreach{println(_)}
    }
  }
}

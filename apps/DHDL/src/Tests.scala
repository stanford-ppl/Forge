import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object TestCompiler extends DHDLApplicationCompiler with Test
object TestInterpreter extends DHDLApplicationInterpreter with Test
trait Test extends DHDLApplication {

  def main() {
    type Q16 = FixPt[Signed, B16, B16]
    type A = SInt

    val N = 10
    val T = 5
    val v1 = OffChipMem[A](N)
    val v2 = OffChipMem[A](N)
    //val out = OffChipMem[A](N, N)

    val vec1 = Array.fill(N)(random[A](10))
    setMem(v1, vec1)

    Accel {
      Pipe(N by T) { i =>
        val b1 = BRAM[A](T)
        b1 := v1(i::i+T)
        v2(i::i+T) := b1
      }
    }

  }
}

// Previously reported that hwblock pointer was stale after unrolling
object Test2Compiler extends DHDLApplicationCompiler with Test2
trait Test2 extends DHDLApplication {
  def main() {
    val T = param(4)
    val P = param(2)

    val x = ArgIn[SInt]
    Accel {
      Reduce(T par P)(0){ii => x.value * ii }{_+_}
      ()
    }
  }
}

// Reported bug where zero wasn't being propagated for registers
object Test3Compiler extends DHDLApplicationCompiler with Test3
trait Test3 extends DHDLApplication {
  def main() {
    val xin = 5
    val T = param(4)
    val P = param(2)
    val x = ArgIn[SInt]
    val out = ArgOut[SInt]
    setArg(x, xin)

    Accel {
      Sequential(T by T){ i =>
        val b1 = BRAM[SInt](T)
        Pipe(T par P){ ii =>
          b1(ii) = x.value * ii
        }
        out := Reduce(T par P)(0){ii => b1(ii) }{_+_}
      }
      ()
    }
    getArg(out)
  }
}

object FilterTestCompiler extends DHDLApplicationCompiler with FilterTest
trait FilterTest extends DHDLApplication {
  def main() {
    val N = args(0).to[SInt]
    val B = param(10)

    val data = OffChipMem[SInt](N)
    val size = ArgIn[SInt]
    val sum = ArgOut[SInt]

    val vec = Array.tabulate(N){i => i}

    println(vec.mkString(","))

    setArg(size, N)
    setMem(data, vec)

    Accel {
      Pipe.fold(size by B par unit(1))(sum){i =>
        val tile = BRAM[SInt](B)
        val fifo = FIFO[SInt](B)
        tile := data(i::i+B)

        Pipe(B by 1 par unit(2)){j =>
          val x = tile(j)
          fifo.push(x, x % 2 == 0)
        }

        val innerSum = Reg[SInt]
        Pipe.reduce(fifo.count by 1 par unit(2))(innerSum){j =>
          fifo.pop()
        }{_+_}
      }{_+_}
    }
    val result = getArg(sum)
    println("result = " + result)
  }
}


// TODO: Eventually this should be banked properly (mem should be duplicated but banked?)
object UnrollTest1Compiler extends DHDLApplicationCompiler with UnrollTest1
trait UnrollTest1 extends DHDLApplication {
  def main() {
    val N = 16
    val out = ArgOut[SInt]

    Accel {
      out := Reduce(N by 1 par unit(2))(0){i =>
        val mem = BRAM[SInt](32)
        mem(i)
      }{_+_}
    }
  }
}

object BankingTest0Compiler extends DHDLApplicationCompiler with BankingTest0
trait BankingTest0 extends DHDLApplicationCompiler {
  def main() {
    Accel {
      val mem = BRAM[SInt](8, 8)
      val out = Reg[SInt]
      Fold(8 by 1 par unit(2))(out, 0){i =>
        Reduce(8 by 1 par unit(8))(0){j =>
          mem(i, j)
        }{_+_}
      }{_+_}
    }
  }
}

object BankingTest1Compiler extends DHDLApplicationCompiler with BankingTest1
trait BankingTest1 extends DHDLApplicationCompiler {
  def main() {
    Accel {
      val mem = BRAM[SInt](8, 8)
      val out = Reg[SInt]
      Fold(8 by 1 par unit(2))(out, 0){i =>
        Reduce(8 by 1 par unit(8))(0){j =>
          // Contrived example:
          // Total parallelization relative to memory is 16, but par of j is only 8
          mem(j)
        }{_+_}
      }{_+_}
    }
  }
}

object BankingTest2Compiler extends DHDLApplicationCompiler with BankingTest2
trait BankingTest2 extends DHDLApplicationCompiler {
  def main() {
    Accel {
      val mem = BRAM[SInt](8, 8)
      val out = Reg[SInt]
      Fold(8 by 1 par unit(2))(out, 0){i =>
        Reduce(8 by 1 par unit(8))(0){j =>
          mem(0, j)
        }{_+_}
      }{_+_}
    }
  }
}

object BankingTest3Compiler extends DHDLApplicationCompiler with BankingTest3
trait BankingTest3 extends DHDLApplicationCompiler {
  def main() {
    Accel {
      val mem = BRAM[SInt](8, 8)
      val out = Reg[SInt]
      Fold(8 by 1 par unit(2))(out, 0){i =>
        Reduce(8 by 1 par unit(8))(0){j =>
          mem(j, j)
        }{_+_}
      }{_+_}
    }
  }
}

object BankingTest4Compiler extends DHDLApplicationCompiler with BankingTest4
trait BankingTest4 extends DHDLApplicationCompiler {
  def main() {
    Accel {
      val mem = BRAM[SInt](8, 8)
      val out = Reg[SInt]
      Fold(8 by 1 par unit(2))(out, 0){i =>
        Reduce(8 by 1 par unit(8))(0){j =>
          mem(i, i)
        }{_+_}
      }{_+_}
    }
  }
}

object BankingTest5Compiler extends DHDLApplicationCompiler with BankingTest5
trait BankingTest5 extends DHDLApplicationCompiler {
  def main() {
    Accel {
      val mem = BRAM[SInt](8, 8)
      val out = Reg[SInt]
      Fold(8 by 1 par unit(2))(out, 0){i =>
        Reduce(8 by 1 par unit(8))(0){j =>
          mem(j, i)
        }{_+_}
      }{_+_}
    }
  }
}

// Previously having issue with primitive operations being code motioned out of Accel
// despite the fact that all
object LiftTestCompiler extends DHDLApplicationCompiler with LiftTest
trait LiftTest extends DHDLApplication {
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
      Sequential (tileSize by tileSize) { i =>
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

object LiftTest2Compiler extends DHDLApplicationCompiler with LiftTest2
trait LiftTest2 extends DHDLApplication {
  def main() {
    val tileSize = param(32)
    val out = ArgOut[SInt]
    Accel {
      out := 16.as[SInt] * tileSize
    }
    println(getArg(out))
  }
}


object LiftTest3Compiler extends DHDLApplicationCompiler with LiftTest3
trait LiftTest3 extends DHDLApplication {
  def main() {
    val T = param(32)
    val P = param(16)
    val out = ArgOut[SInt]
    Accel {
      out := 32.as[SInt] * T * P
    }
    println(getArg(out))
  }
}

object ParLoadCompiler extends DHDLApplicationCompiler with ParLoadTest
trait ParLoadTest extends DHDLApplication {
  def main() {
    val P1 = param(4)
    val P2 = param(2)
    val P3 = param(2)
    val mem = OffChipMem[SInt](32,32)
    val out = ArgOut[SInt]
    Accel {
      val bram = BRAM[SInt](32,32)
      bram := mem(0::32, 0::32, P1)

      Fold(32 par P2)(out, 0.as[SInt]){i =>
        Reduce(32 par P3)(0.as[SInt]){j =>
          bram(i,j)
        }{_+_}
      }{_+_}
      ()
    }
    println(getArg(out))
  }
}

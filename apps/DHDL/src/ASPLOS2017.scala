import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object MatMultAsplos extends DHDLApplicationCompiler with MatMultAsplos
object MatMultAsplosInterpreter extends DHDLApplicationInterpreter with MatMultAsplos
trait MatMultAsplos extends DHDLApplication {
  type T = Flt //FixPt[Signed,B16,B16]
  type Array[T] = ForgeArray[T]

  def matmultAsplos(A: Rep[Array[T]], B: Rep[Array[T]], M: Rep[SInt], N: Rep[SInt], P: Rep[SInt]) = {
    bound(M) = 1536
    bound(N) = 1536
    bound(P) = 1536

    val a = OffChipMem[T](M, P)
    val b = OffChipMem[T](P, N)
    val c = OffChipMem[T](M, N)
    val m = ArgIn[SInt]
    val n = ArgIn[SInt]
    val p = ArgIn[SInt]

    val bm        = param(4);   domainOf(bm) = (1,1536,1)
    val bn        = param(4);   domainOf(bn) = (96,1536,96)
    val bp        = param(4);   domainOf(bp) = (96,1536,96)
    val outerPar  = param(1);   domainOf(outerPar)  = (1,6,1)
    val middlePar = param(2);   domainOf(middlePar) = (1,96,1)
    val innerPar  = param(2);   domainOf(innerPar)  = (1,96,1)
    val upMidPar  = param(1);   domainOf(upMidPar)  = (1,1,1)
    val stPar     = param(1);   domainOf(stPar)     = (1,1,1)

    setArg(m, M)
    setArg(n, N)
    setArg(p, P)
    setMem(a, A)
    setMem(b, B)

    Accel {
      Pipe(m by bm, (n by bn) par outerPar){(i,j) =>
        Pipe((p by bp) par upMidPar){k =>
          val tileA = BRAM[T](bm, bp)
          val tileB = BRAM[T](bp, bn)
          val tileC = BRAM[T](bm, bn)
          Parallel {
            tileA := a(i::i+bm, k::k+bp, param(1))
            tileB := b(k::k+bp, j::j+bn, param(1))
          }
          Sequential(bm by 1, (bn by 1) par middlePar){ (ii,jj) =>    // MetaPipe?
            val prod = Reduce((bp by 1) par innerPar)(0.as[T]){ kk => tileA(ii, kk) * tileB(kk, jj) }{_+_}
            val prev = mux(k == 0, 0.as[T], tileC(ii,jj))
            tileC(ii,jj) = prev + prod.value
          }
          c(i::i+bm, j::j+bn, stPar) := tileC
        }
      }
    }
    getMem(c)
  }

  def main() = {
    val M = args(0).to[SInt]
    val N = args(1).to[SInt]
    val P = args(2).to[SInt]

    val a = Array.fill(M){ Array.fill(P){random[T](100)} }
    val b = Array.fill(P){ Array.fill(N){random[T](100)} }

    val result = matmultAsplos(a.flatten, b.flatten, M, N, P)

    val gold = Array.tabulate(M){i =>
      val aRow = a(i)
      Array.tabulate(N){j =>
        val bCol = b.map{row => row(j)}
        aRow.zip(bCol){_*_}.reduce{_+_}
      }
    }.flatten

    println("expected: " + gold.mkString(", "))
    println("result:   " + result.mkString(", "))
    assert(gold == result)
  }
}


object BlackScholesAsplos extends DHDLApplicationCompiler with BlackScholesAsplos
object BlackScholesAsplosInterpreter extends DHDLApplicationInterpreter with BlackScholesAsplos
trait BlackScholesAsplos extends DHDLApplication {

  lazy val tileSize = param(96)
  lazy val outerPar = param(1)
  lazy val innerPar = param(2)
  lazy val numOptions = ArgIn[SInt]

  final val inv_sqrt_2xPI = 0.39894228040143270286f

  def CNDF(x: Rep[Flt]) = {
    val ax = abs(x)

    val xNPrimeofX = exp((ax ** 2) * -0.05f) * inv_sqrt_2xPI
    val xK2 = 1.as[Flt] / ((ax * 0.2316419f) + 1.0f)

    val xK2_2 = xK2 ** 2
    val xK2_3 = xK2_2 * xK2
    val xK2_4 = xK2_3 * xK2
    val xK2_5 = xK2_4 * xK2

    val xLocal_10 = xK2 * 0.319381530f
    val xLocal_20 = xK2_2 * -0.356563782f
    val xLocal_30 = xK2_3 * 1.781477937f
    val xLocal_31 = xK2_4 * -1.821255978f
    val xLocal_32 = xK2_5 * 1.330274429f

    val xLocal_21 = xLocal_20 + xLocal_30
    val xLocal_22 = xLocal_21 + xLocal_31
    val xLocal_23 = xLocal_22 + xLocal_32
    val xLocal_1 = xLocal_23 + xLocal_10

    val xLocal0 = xLocal_1 * xNPrimeofX
    val xLocal  = -xLocal0 + 1.0f

    mux(x < 0.0f, xLocal0, xLocal)
  }

  def BlkSchlsEqEuroNoDiv(sptprice: Rep[Flt], strike: Rep[Flt], rate: Rep[Flt],
                          volatility: Rep[Flt], time: Rep[Flt], otype: Rep[UInt]) = {

    val xLogTerm = log( sptprice / strike )
    val xPowerTerm = (volatility ** 2) * 0.5f
    val xNum = (rate + xPowerTerm) * time + xLogTerm
    val xDen = volatility * sqrt(time)

    val xDiv = xNum / (xDen ** 2)
    val nofXd1 = CNDF(xDiv)
    val nofXd2 = CNDF(xDiv - xDen)

    val futureValueX = strike * exp(-rate * time)

    val negNofXd1 = -nofXd1 + 1.0f
    val negNofXd2 = -nofXd2 + 1.0f

    val optionPrice1 = (sptprice * nofXd1) - (futureValueX * nofXd2)
    val optionPrice2 = (futureValueX * negNofXd2) - (sptprice * negNofXd1)
    mux(otype == 0, optionPrice2, optionPrice1)
  }

  def blackscholesAsplos(
    otype:      Rep[OffChipMem[UInt]],
    sptprice:   Rep[OffChipMem[Flt]],
    strike:     Rep[OffChipMem[Flt]],
    rate:       Rep[OffChipMem[Flt]],
    volatility: Rep[OffChipMem[Flt]],
    otime:      Rep[OffChipMem[Flt]],
    optprice:   Rep[OffChipMem[Flt]]
  ): Rep[Unit] = {

    Pipe((numOptions by tileSize) par outerPar) { i =>
      val otypeRAM      = BRAM[UInt](tileSize)
      val sptpriceRAM   = BRAM[Flt](tileSize)
      val strikeRAM     = BRAM[Flt](tileSize)
      val rateRAM       = BRAM[Flt](tileSize)
      val volatilityRAM = BRAM[Flt](tileSize)
      val otimeRAM      = BRAM[Flt](tileSize)

      Parallel {
        otypeRAM := otype(i::i+tileSize, innerPar)
        sptpriceRAM := sptprice(i::i+tileSize, innerPar)
        strikeRAM := strike(i::i+tileSize, innerPar)
        rateRAM := rate(i::i+tileSize, innerPar)
        volatilityRAM := volatility(i::i+tileSize, innerPar)
        otimeRAM := otime(i::i+tileSize, innerPar)
      }

      val optpriceRAM = BRAM[Flt](tileSize)
      Pipe((tileSize by 1) par innerPar){ j =>
        val price = BlkSchlsEqEuroNoDiv(sptpriceRAM(j), strikeRAM(j), rateRAM(j), volatilityRAM(j), otimeRAM(j), otypeRAM(j))
        optpriceRAM(j) = price
      }
      optprice(i::i+tileSize, innerPar) := optpriceRAM
    }
  }

  def printArr(a: Rep[ForgeArray[Flt]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }
  def main() {
    val N = args(0).to[SInt]

    bound(N) = 9995328
    domainOf(tileSize) = (96,19200,96)
    domainOf(outerPar) = (1,1,1)
    domainOf(innerPar) = (1,96,1)

    val types  = OffChipMem[UInt](N)
    val prices = OffChipMem[Flt](N)
    val strike = OffChipMem[Flt](N)
    val rate   = OffChipMem[Flt](N)
    val vol    = OffChipMem[Flt](N)
    val time   = OffChipMem[Flt](N)
    val optprice = OffChipMem[Flt](N)

    val sotype      = Array.fill(N)(random[UInt](2))
    val ssptprice   = Array.fill(N)(random[Flt])
    val sstrike     = Array.fill(N)(random[Flt])
    val srate       = Array.fill(N)(random[Flt])
    val svolatility = Array.fill(N)(random[Flt])
    val sotime      = Array.fill(N)(random[Flt])

    setArg(numOptions, N)
    setMem(types, sotype)
    setMem(prices, ssptprice)
    setMem(strike, sstrike)
    setMem(rate, srate)
    setMem(vol, svolatility)
    setMem(time, sotime)

    Accel{ blackscholesAsplos(types, prices, strike, rate, vol, time, optprice) }

    val out = getMem(optprice)

    printArr(sstrike, "sstrike:")
    printArr(out, "result:")
  }
}

import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object BlackScholesCompiler extends DHDLApplicationCompiler with BlackScholes
object BlackScholesInterpreter extends DHDLApplicationInterpreter with BlackScholes
trait BlackScholes extends DHDLApplication {
  override def stageArgNames = List("tileSize")

  lazy val tileSize = stageArgOrElse[Int](0, 4)
  lazy val numOptions = ArgIn[Fix]("numOptions")

  final val inv_sqrt_2xPI = 0.39894228040143270286f

  def CNDF(x: Rep[Flt]) = {
    val ax = abs(x)

    val xNPrimeofX = exp((ax ** 2) * -0.05f) * inv_sqrt_2xPI
    val xK2 = 1.0f / ((ax * 0.2316419f) + 1.0f)

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
    val xLocal  = 1.0f - xLocal0
    val subFromOne = 1.0f - xLocal

    mux(x < 0.0f, subFromOne, xLocal)
  }

  def BlkSchlsEqEuroNoDiv(sptprice: Rep[Flt], strike: Rep[Flt], rate: Rep[Flt],
                          volatility: Rep[Flt], time: Rep[Flt], otype: Rep[Fix]) = {

    val xLogTerm = log( sptprice / strike )
    val xPowerTerm = 0.5f * (volatility ** 2)
    val xNum = (rate + xPowerTerm) * time + xLogTerm
    val xDen = volatility * sqrt(time)

    val xDiv = xNum / (xDen ** 2)
    val nofXd1 = CNDF(xDiv)
    val nofXd2 = CNDF(xDiv - xDen)

    val futureValueX = strike * exp(-rate * time)

    val negNofXd1 = 1.0f - nofXd1
    val negNofXd2 = 1.0f - nofXd2

    val optionPrice1 = (sptprice * nofXd1) - (futureValueX * nofXd2)
    val optionPrice2 = (futureValueX * negNofXd2) - (sptprice * negNofXd1)
    mux(otype == 0, optionPrice2, optionPrice1)
  }

  def blackscholes(
    otype:      Rep[OffChipMem[Fix]],
    sptprice:   Rep[OffChipMem[Flt]],
    strike:     Rep[OffChipMem[Flt]],
    rate:       Rep[OffChipMem[Flt]],
    volatility: Rep[OffChipMem[Flt]],
    otime:      Rep[OffChipMem[Flt]],
    optprice:   Rep[OffChipMem[Flt]]
  ): Rep[Unit] = {
    //TODO: Need to duplicate these for parallelization but if put in the function, have no visibility
    val otypeRAM      = BRAM[Fix](tileSize)
    val sptpriceRAM   = BRAM[Flt](tileSize)
    val strikeRAM     = BRAM[Flt](tileSize)
    val rateRAM       = BRAM[Flt](tileSize)
    val volatilityRAM = BRAM[Flt](tileSize)
    val otimeRAM      = BRAM[Flt](tileSize)

    MetaPipe(numOptions by tileSize) { i =>
      Parallel {
        otype.ld(otypeRAM, i, tileSize)
        sptprice.ld(sptpriceRAM, i, tileSize)
        strike.ld(strikeRAM, i, tileSize)
        rate.ld(rateRAM, i, tileSize)
        volatility.ld(volatilityRAM, i, tileSize)
        otime.ld(otimeRAM, i, tileSize)
      }

      val optpriceRAM = BRAM[Flt](tileSize)
      Pipe(tileSize by 1) { j =>
        val price = BlkSchlsEqEuroNoDiv(sptpriceRAM(j), strikeRAM(j), rateRAM(j), volatilityRAM(j), otimeRAM(j), otypeRAM(j))
        optpriceRAM(j) = price
      }
      optprice.st(optpriceRAM, i, tileSize)
    }
  }

  def main() {
    val N = 32

    val types  = OffChipMem[Fix]("otype", N)
    val prices = OffChipMem[Flt]("sptprice", N)
    val strike = OffChipMem[Flt]("strke", N)
    val rate   = OffChipMem[Flt]("rate", N)
    val vol    = OffChipMem[Flt]("volatility", N)
    val time   = OffChipMem[Flt]("otime", N)
    val optprice = OffChipMem[Flt]("optprice", N)

    val sotype      = Array.fill(N)(randomFix(2))
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

    Accel{ blackscholes(types, prices, strike, rate, vol, time, optprice) }

    val out = getMem(optprice)

    println(out.mkString(","))
  }
}

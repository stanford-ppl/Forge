import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object BlackScholesCompiler extends DHDLApplicationCompiler with BlackScholes
object BlackScholesInterpreter extends DHDLApplicationInterpreter with BlackScholes
trait BlackScholes extends DHDLApplication {
  override def stageArgNames = List("tileSize")

  lazy val tileSize = param(7104)
  lazy val outerPar = param(1)
  lazy val innerPar = param(16)
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

  def blackscholes(
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

    Accel{ blackscholes(types, prices, strike, rate, vol, time, optprice) }

    val out = getMem(optprice)

    println(out.mkString(","))
  }
}

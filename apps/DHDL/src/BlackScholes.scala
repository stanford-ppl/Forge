import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object BlackScholesCompiler extends DHDLApplicationCompiler with BlackScholes 
object BlackScholesInterpreter extends DHDLApplicationInterpreter with BlackScholes

trait BlackScholes extends DHDLApplication {
	def printUsage = {
    println("Usage: blackscholes")
    exit(-1)
	}

  val inv_sqrt_2xPI = 0.39894228040143270286f

  def CNDF(InputX: Rep[Float]) = {
    val lt_0 = (InputX < 0.0f)
    val sign = mux(lt_0, true, false)
    val xInput = abs(InputX)

    val inputSqr = xInput * xInput
    val mul = (inputSqr * -0.5f)
    val expValues = exp(mul)

    val xNPrimeofX = (expValues * inv_sqrt_2xPI)

    val xK20 = (xInput * 0.2316419f)
    val xK21 = (unit(1.0f) + xK20)
    val xK2 = (unit(1.0f) / xK21)

    val xK2_2 = (xK2 * xK2)

    val xK2_3 = (xK2_2 * xK2)

	  val xK2_4 = xK2_3 * xK2

    val xK2_5 = xK2_4 * xK2

    val xLocal_10 = xK2 * 0.319381530f

    val xLocal_20 = xK2_2 * (-0.356563782f)

    val xLocal_30 = xK2_3 * 1.781477937f

    val xLocal_21 = xLocal_20 + xLocal_30
    val xLocal_31 = xK2_4 * (-1.821255978f)

    val xLocal_22 = xLocal_21 + xLocal_31

    val xLocal_32 = xK2_5 * 1.330274429f

    val xLocal_23 = xLocal_22 + xLocal_32
    val xLocal_1 = xLocal_23 + xLocal_10

    val xLocal0   = xLocal_1 * xNPrimeofX
    val xLocal   = unit(1.0f) - xLocal0

    val subFromOne = unit(1.0f) - xLocal
    val OutputX  = mux(sign, subFromOne, xLocal)
    OutputX
  }

  def BlkSchlsEqEuroNoDiv(sptprice: Rep[Float], strike: Rep[Float], rate: Rep[Float], volatility:
		Rep[Float], time:
		Rep[Float], otype: Rep[FixPt]) = {

    val xStockPrice = sptprice
    val xStrikePrice = strike
    val xRiskFreeRate = rate
    val xVolatility = volatility
    val xTime = time;
    val xSqrtTime = sqrt(xTime)
    val div = sptprice / strike
    val logValues = log(div)
    val xLogTerm = logValues
    val volSqr = xVolatility * xVolatility
    val xPowerTerm = volSqr * 0.5f
    val xD10 = xRiskFreeRate + xPowerTerm
    val xD11 = xD10 * xTime
    val xD12 = xD11 + xLogTerm
    val xDen = xVolatility * xSqrtTime
    val xD13 = xD12 / xDen
    val xD14 = xD13 / xDen
    val xD2 = xD14 - xDen
    val d1 = xD14
    val d2 = xD2
    val NofXd1 = CNDF(d1)
    val NofXd2 = CNDF(d2)
    val rateTime = rate * time
    val negRateTime = unit(0.0f) - rateTime
    val expRateTime = exp(negRateTime)
    val FutureValueX = strike * expRateTime
    val mul1t = sptprice * NofXd1
    val mul2t = FutureValueX * NofXd2
    val OptionPrice1 = mul1t - mul2t

    val NegNofXd1 = unit(1.0f) - NofXd1
    val NegNofXd2 = unit(1.0f) - NofXd2
    val mul3t = FutureValueX * NegNofXd2
    val mul4t = sptprice * NegNofXd1
    val OptionPrice2 = mul3t - mul4t
    val cmp = (otype == 0)
    val OptionPrice = mux(cmp, OptionPrice2, OptionPrice1)
    OptionPrice
	}

  def main() = {
		val tileSize = 4
		val sNumOptions = 32

		val sotype 			= Seq.fill(sNumOptions)(Random.nextInt(100))
		val ssptprice 	= Seq.fill(sNumOptions)(Random.nextFloat())
		val sstrike 		= Seq.fill(sNumOptions)(Random.nextFloat())
		val srate	 			= Seq.fill(sNumOptions)(Random.nextFloat())
		val svolatility = Seq.fill(sNumOptions)(Random.nextFloat())
		val sotime 			= Seq.fill(sNumOptions)(Random.nextFloat())

		val numOptions = ArgIn[FixPt](sNumOptions).value
		val otype = OffChipMem[FixPt]("otype", sotype.map(i => i.toFixPt): _*)
		val sptprice = OffChipMem[Float]("sptprice", ssptprice.map(i => unit(i)): _*)
		val strike = OffChipMem[Float]("strke", sstrike.map(i => unit(i)): _*)
		val rate = OffChipMem[Float]("rate", srate.map(i => unit(i)): _*)
		val volatility = OffChipMem[Float]("volatility", svolatility.map(i => unit(i)): _*)
		val otime = OffChipMem[Float]("otime", sotime.map(i => unit(i)): _*)
		val optprice = OffChipMem[Float]("optprice", numOptions)

		//TODO: Need to duplicatae these for parallelization but if put in the function, have no
		//visibility
    val otypeRAM = BRAM[FixPt](tileSize)
    val sptpriceRAM = BRAM[Float](tileSize)
    val strikeRAM = BRAM[Float](tileSize)
    val rateRAM = BRAM[Float](tileSize)
    val volatilityRAM = BRAM[Float](tileSize)
    val otimeRAM = BRAM[Float](tileSize)
		val seqCounter = CounterChain(Counter(numOptions, tileSize))
		MetaPipe(1, seqCounter, {case i::_ => 
			Parallel({
				otype.ld(otypeRAM, i, tileSize)
				sptprice.ld(sptpriceRAM, i, tileSize)
				strike.ld(strikeRAM, i, tileSize)
				rate.ld(rateRAM, i, tileSize)
				volatility.ld(volatilityRAM, i, tileSize)
				otime.ld(otimeRAM, i, tileSize)
			})

      val optpriceRAM = BRAM[Float](tileSize)
			val bsCounter = CounterChain(Counter(tileSize, 1))
			Pipe(1, bsCounter, {case j::_ =>
				val sptprice_d = sptpriceRAM.ld(j)
				val strike_d = strikeRAM.ld(j)
				val rate_d = rateRAM.ld(j)
				val volatility_d = volatilityRAM.ld(j)
				val otime_d = otimeRAM.ld(j)
				val otype_d = otypeRAM.ld(j)
        val optPrice = BlkSchlsEqEuroNoDiv(sptprice_d, strike_d, rate_d, volatility_d, otime_d, otype_d)
				optpriceRAM.st(j, optPrice)
			})
			optprice.st(optpriceRAM, i, tileSize)
		})
		println(optprice.mkString)
	}
}

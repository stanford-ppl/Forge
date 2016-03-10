import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object TPCHQ6Compiler extends DHDLApplicationCompiler with TPCHQ6
object TPCHQ6Interpreter extends DHDLApplicationInterpreter with TPCHQ6
trait TPCHQ6 extends DHDLApplication {

  lazy val tileSize = stageArgOrElse[Int](0, 4)
  lazy val dataSize = ArgIn[Fix]("dataSize")
  lazy val minDate  = ArgIn[Fix]("minDate")
  lazy val maxDate  = ArgIn[Fix]("maxDate")

  def tpchq6(dates:  Rep[OffChipMem[Fix]], quants: Rep[OffChipMem[Fix]],
             discts: Rep[OffChipMem[Flt]], prices: Rep[OffChipMem[Flt]], out: Rep[Reg[Flt]]) {

    MetaPipe(dataSize by tileSize, out) { i =>
      val datesTile  = BRAM[Fix](tileSize)
      val quantsTile = BRAM[Fix](tileSize)
      val disctsTile = BRAM[Flt](tileSize)
      val pricesTile = BRAM[Flt](tileSize)
      Parallel {
        dates.ld(datesTile, i, tileSize)
        quants.ld(quantsTile, i, tileSize)
        discts.ld(disctsTile, i, tileSize)
        prices.ld(pricesTile, i, tileSize)
      }
      val accum = Reg[Flt]
      Pipe(tileSize by 1, accum){ j =>
        val date  = datesTile(j)
        val disct = disctsTile(j)
        val quant = quantsTile(j)
        val price = pricesTile(j)
        val valid = date > minDate && date < maxDate && disct >= 0.05f && disct <= 0.07f && quant < 24
        mux(valid, price * disct, 0.0f)
      }{_+_}
      accum.value
    }{_+_}
  }

  def main() {
    val N = 12
    val MIN_DATE = 64
    val MAX_DATE = 80

    val dates  = OffChipMem[Fix]("dates", N)
    val quants = OffChipMem[Fix]("quants", N)
    val discts = OffChipMem[Flt]("discounts", N)
    val prices = OffChipMem[Flt]("prices", N)
    val out = ArgOut[Flt]("out")

    val sDates  = Array.fill(N){randomFix(35) + 50}
    val sQuants = Array.fill(N){randomFix(50) }
    val sDiscts = Array.fill(N){random[Flt]}
    val sPrices = Array.fill(N){random[Flt] * 1000f}

    setArg(dataSize, N)
    setArg(minDate, MIN_DATE)
    setArg(maxDate, MAX_DATE)
    setMem(dates, sDates)
    setMem(quants, sQuants)
    setMem(discts, sDiscts)
    setMem(prices, sPrices)
    Accel { tpchq6(dates, quants, discts, prices, out) }

    // --- software version
    val conds = Array.tabulate(N){i => sDates(i) > MIN_DATE && sDates(i) < MAX_DATE &&
                                       sQuants(i) < 24 && sDiscts(i) >= 0.05f && sDiscts(i) <= 0.07f }

    val gold = Array.tabulate(N){i => if (conds(i)) sPrices(i) * sDiscts(i) else 0.0f.toFltPt }.reduce{_+_}

    val result = getArg(out)
    assert(result == gold)
	}
}

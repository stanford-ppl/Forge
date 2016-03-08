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
             discts: Rep[OffChipMem[Flt]], prices: Rep[OffChipMem[Flt]]): Rep[Flt] = {

    val out = ArgOut[Flt]("out")
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
    out.value
  }

  def main() {
    tpchq6 (
      OffChipMem[Fix]("dates", dataSize),
      OffChipMem[Fix]("quants", dataSize),
      OffChipMem[Flt]("discounts", dataSize),
      OffChipMem[Flt]("prices", dataSize)
    )
	}
}

object TPCHQ6TestCompiler extends DHDLApplicationCompiler with TPCHQ6Test
object TPCHQ6TestInterpreter extends DHDLApplicationInterpreter with TPCHQ6Test
trait TPCHQ6Test extends TPCHQ6 {
  override def stageArgNames = List("tileSize", "dataSize", "minDate", "maxDate")
  lazy val sdataSize = stageArgOrElse[Int](1, 12)
  lazy val sminDate  = stageArgOrElse[Int](2, 64)
  lazy val smaxDate  = stageArgOrElse[Int](3, 80)

  override def main() {
    val sDates = Seq.tabulate(sdataSize){i => util.Random.nextInt(35) + 50}
    val sQuants = Seq.tabulate(sdataSize){i => util.Random.nextInt(50)}
    val sDiscts = Seq.tabulate(sdataSize){i => util.Random.nextFloat() * 0.1f }
    val sPrices = Seq.tabulate(sdataSize){i => util.Random.nextFloat() * 1000f }
    val conds   = Seq.tabulate(sdataSize){i =>
      sDates(i) > sminDate && sDates(i) < smaxDate && sQuants(i) < 24 &&
      sDiscts(i) >= 0.05f && sDiscts(i) <= 0.07f }
    val gold = conds.zipWithIndex.filter(x => x._1).map{i => sPrices(i._2) * sDiscts(i._2) }.sum

    val dates  = OffChipMem.withInit1D("dates", sDates.map(_.toFixPt))
    val quants = OffChipMem.withInit1D("quants", sQuants.map(_.toFixPt))
    val discts = OffChipMem.withInit1D("discounts", sDiscts.map(_.toFltPt))
    val prices = OffChipMem.withInit1D("prices", sPrices.map(_.toFltPt))

    val result = tpchq6(dates, quants, discts, prices)
    assert(result == gold)
  }
}

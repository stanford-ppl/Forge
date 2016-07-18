import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object TPCHQ6Compiler extends DHDLApplicationCompiler with TPCHQ6
object TPCHQ6Interpreter extends DHDLApplicationInterpreter with TPCHQ6
trait TPCHQ6 extends DHDLApplication {
  type Array[T] = ForgeArray[T]

  val MIN_DATE = 64
  val MAX_DATE = 80

  def tpchq6(datesIn: Rep[Array[UInt]], quantsIn: Rep[Array[UInt]], disctsIn: Rep[Array[Flt]], pricesIn: Rep[Array[Flt]]): Rep[Flt] = {
    val N = datesIn.length
    bound(N) = 18720000

    val dates  = OffChipMem[UInt](N)
    val quants = OffChipMem[UInt](N)
    val discts = OffChipMem[Flt](N)
    val prices = OffChipMem[Flt](N)
    val dataSize = ArgIn[SInt]
    val minDateIn = ArgIn[UInt]
    val maxDateIn = ArgIn[UInt]
    val out = ArgOut[Flt]

    val tileSize = param(12);   domainOf(tileSize) = (96,192000,96)
    val outerPar = param(2);    domainOf(outerPar) = (1,6,1)
    val innerPar = param(2);    domainOf(innerPar) = (1,384,1)

    setArg(dataSize, N)
    setArg(minDateIn, MIN_DATE)
    setArg(maxDateIn, MAX_DATE)
    setMem(dates, datesIn)
    setMem(quants, quantsIn)
    setMem(discts, disctsIn)
    setMem(prices, pricesIn)

    Accel {
      val minDate = minDateIn.value
      val maxDate = maxDateIn.value

      Fold(dataSize by tileSize par outerPar)(out, 0.as[Flt]){ i =>
        val datesTile  = BRAM[UInt](tileSize)
        val quantsTile = BRAM[UInt](tileSize)
        val disctsTile = BRAM[Flt](tileSize)
        val pricesTile = BRAM[Flt](tileSize)
        Parallel {
          datesTile  := dates(i::i+tileSize, innerPar)
          quantsTile := quants(i::i+tileSize, innerPar)
          disctsTile := discts(i::i+tileSize, innerPar)
          pricesTile := prices(i::i+tileSize, innerPar)
        }
        Reduce(tileSize par innerPar)(0.as[Flt]){ j =>
          val date  = datesTile(j)
          val disct = disctsTile(j)
          val quant = quantsTile(j)
          val price = pricesTile(j)
          val valid = date > minDate && date < maxDate && disct >= 0.05f && disct <= 0.07f && quant < 24
          mux(valid, price * disct, 0.0f)
        }{_+_}
      }{_+_}
    }
    getArg(out)
  }

  def main() {
    val N = args(0).to[SInt]

    val dates  = Array.fill(N){random[UInt](20) + 65}
    val quants = Array.fill(N){random[UInt](25) }
    val discts = Array.fill(N){random[Flt] * 0.05f + 0.02f}
    val prices = Array.fill(N){random[Flt] * 1000f}

    val result = tpchq6(dates, quants, discts, prices)

    // --- software version
    val conds = Array.tabulate(N){i => dates(i) > MIN_DATE && dates(i) < MAX_DATE &&
                                       quants(i) < 24 && discts(i) >= 0.05f && discts(i) <= 0.07f }

    val gold = Array.tabulate(N){i => if (conds(i)) prices(i) * discts(i) else 0.0f.as[Flt] }.reduce{_+_}

    println("expected: " + gold.mkString)
    println("result: " + result.mkString)
    assert(result == gold)
  }
}

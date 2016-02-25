import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object TPCHQ6Compiler extends DHDLApplicationCompiler with TPCHQ6 
object TPCHQ6Interpreter extends DHDLApplicationInterpreter with TPCHQ6

trait TPCHQ6 extends DHDLApplication {
	def printUsage = {
		println("Usage: tpchq6 <dataSize> <tileSize>")
    exit(-1)
	}
  def main() = {
    val N = 12
		val tileSize = 4

    val sMinDate = 64; val sMaxDate = 80
    val sDates = Seq.tabulate(N){i => util.Random.nextInt(35) + 50}
    val sQuants = Seq.tabulate(N){i => util.Random.nextInt(50)}
    val sDiscts = Seq.tabulate(N){i => util.Random.nextFloat() * 0.1f }
    val sPrices = Seq.tabulate(N){i => util.Random.nextFloat() * 1000f }
		val conds = Seq.tabulate(N){i => 
			sDates(i) > sMinDate && sDates(i) < sMaxDate && sQuants(i) < 24 &&
      sDiscts(i) >= 0.05f && sDiscts(i) <= 0.07f}
    val gold = conds.zipWithIndex.filter(x=>x._1).map{i => sPrices(i._2) * sDiscts(i._2) }.sum

		val dataSize = ArgIn[Int](N).value
		val minDate = ArgIn[Int](sMinDate)
		val maxDate = ArgIn[Int](sMaxDate)

		val dates = OffChipMem[FixPt]("dates", sDates.map(i => i.toFixPt): _*)
		val quants = OffChipMem[FixPt]("quants", sQuants.map(i => i.toFixPt): _*)
		val discounts = OffChipMem[Float]("discounts", sDiscts.map(i => unit(i)): _*)
		val prices = OffChipMem[Float]("prices", sPrices.map(i => unit(i)): _*)
		val tileCounter = CounterChain(Counter(dataSize, tileSize)) 

		val outAccum = Reg[Float](unit(0.0f))
		MetaPipe[Float](true, tileCounter, outAccum, _+_, {case i::_ => 
			val datesBm = BRAM[FixPt](tileSize)
			val quantsBm = BRAM[FixPt](tileSize)
			val discountsBm = BRAM[Float](tileSize)
			val pricesBm = BRAM[Float](tileSize)
			Parallel({
				dates.ld(datesBm, i, tileSize)
				quants.ld(quantsBm, i, tileSize)
				discounts.ld(discountsBm, i, tileSize)
				prices.ld(pricesBm, i, tileSize)
			})
			val inCounter = CounterChain(Counter(max=tileSize))
			val inAccum = Reg[Float](0.0f)
			Pipe[Float](true, inCounter, inAccum, _+_, { case j::_ =>
				val date = datesBm.ld(j)
				val discount = discountsBm.ld(j)
				val quant = quantsBm.ld(j)
				val price = pricesBm.ld(j)
				val valid = ((date > minDate.value) && (date < maxDate.value) && (discount >= 0.05f) &&
				 						 (discount <= 0.07f) && (quant < FixPt(24)))
				mux(valid, price * discount, 0.0f)
			})
			inAccum.value
		})
		val result = ArgOut[Float](0.0f)
		result.write(outAccum.value)

		assert(unit(gold)==result.value)
	}
}

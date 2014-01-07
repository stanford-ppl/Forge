import optiwrangler.compiler._
import optiwrangler.library._
import optiwrangler.shared._

object ExamplesCompiler extends OptiWranglerApplicationCompiler with Examples
object ExamplesInterpreter extends OptiWranglerApplicationInterpreter with Examples

trait Examples extends OptiWranglerApplication {
  	
	def main() = {
		if (args.length < 1) printUsage

		println("Reading input table...")
		val inputTable = Table.fromFile(args(0), isValidLine)
		println("Reading input table... DONE")


		transformAndPrint (inputTable, args(1))
		println("Done!!")
	}

	def printUsage = {
	    println("Usage: Fill <input_file> <output_file>")
	    exit(-1)
  	}

	def transformAndPrint(tbl:Rep[Table], outFile:Rep[String]) = {

		tic(tbl)
		
		println("Applying transforms...")
		val outTable = tbl Split(col = "data", delimiter = ";", numOfSplits = 4) 
		                   Fill("up")
		                   Translate("left")
		                   Drop("data")
		                   SetName("data0", "renCol")
		                   Filter(Condition.apply)
		                   Merge(colsToMerge, "::")
		                   Transpose()
		println("Applying transforms... DONE")

		println("Printing output table...")
		writeTable(outTable, outFile)
		println("Printing output table...")

		toc(outTable)
	}

  def isValidLine(line: Rep[String]): Rep[Boolean] = {true}
}
import optiwrangler.compiler._
import optiwrangler.library._
import optiwrangler.shared._

object ExamplesCompiler extends OptiWranglerApplicationCompiler with Examples
object ExamplesInterpreter extends OptiWranglerApplicationInterpreter with Examples

trait Examples extends OptiWranglerApplication {
  	
	def main() = {
		if (args.length < 2) printUsage

		println("Reading input table...")
		val inputTable = Table.fromFile(args(0), isValidLine)
		println("Reading input table... DONE")


		transformAndPrint (inputTable, args(1))
		println("Done!!")
	}

	def printUsage = {
	    println("Usage: bin/delite ExamplesCompiler <input_file> <output_file>")
	    exit(-1)
  	}

	def transformAndPrint(tbl:Rep[Table], outFile:Rep[String]) = {

		tic("Applying transforms")
		println("Applying transforms...")

		val colsToMerge = "renCol,split1".fsplit(",")
		val outTable = (tbl Split(col = "data", delimiter = ";", numOfSplits = 4)
		                    Fill("up")
		                    Translate("left")
		                    Drop("data")
		                    SetName("split0", "renCol")
		                    Filter(Condition.apply)
		                    Merge(colsToMerge, "::")
		                    Transpose())

		println("Applying transforms... DONE")
		toc("Applying transforms")

		println("Printing output table...")
		writeToFile(outTable, outFile)
		println("Printing output table...")
	}

  def isValidLine(line: Rep[String]): Rep[Boolean] = {true}
}

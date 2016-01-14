import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object DotProductCompiler extends DHDLApplicationCompiler with DotProduct 
object DotProductInterpreter extends DHDLApplicationInterpreter with DotProduct 

trait DotProduct extends DHDLApplication {
	def printUsage = {
    println("Usage: dotprod")
    exit(-1)
	}
  def main() = {
		//FixPt a = FixPt(3,4)
		//println(a)
	}
}

import dhdl.compiler._
import dhdl.library._
import dhdl.shared._
import scala.util.Random

object KmeansCompiler extends DHDLApplicationCompiler with Kmeans 
object KmeansInterpreter extends DHDLApplicationInterpreter with Kmeans

trait Kmeans extends DHDLApplication {
	def printUsage = {
    println("Usage: kmeans")
    exit(-1)
	}
  def main() = {
	}
}

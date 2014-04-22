import optigraph.compiler._
import optigraph.library._
import optigraph.shared._

// This object lets us run the Delite version of the code
object TestSchedulerCompiler extends OptiGraphApplicationCompiler with TestScheduler

// This object lets us run the Scala library version of the code
object TestSchedulerInterpreter extends OptiGraphApplicationInterpreter with TestScheduler

trait TestScheduler extends OptiGraphApplication {
  def main() = {
    val nd  = NodeData.fromFunction(10,e => e)
    nd.print
    val nd_map1 = nd.map(e => e*2)
    println("Times 2")
    nd_map1.print
    val nd_map = nd.mapreduce[Int](e => 1, (a,b) => a+b, e=>true)
    println("flat map")
    val fm = nd.flatMap(e => nd_map1)
    fm.print
    println("mpreduce: " + nd_map)
    val f = fm.filter(e => e==0, e => e)
    f.print
    val zero = NodeData[Int](0).map(e => e)
    zero.print
    val zeroFilter = f.filter(e => e==69, e=>e)
    zeroFilter.print
  }
  def printUsage = {
    println("Usage: TestScheduler <path to input edge list file> <path to output file (to be created)>")
    exit(-1)
  }
}
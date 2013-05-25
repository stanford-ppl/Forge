import optiml.compiler._
import optiml.library._
import optiml.shared._

object GDACompiler extends OptiMLApplicationCompiler with GDA
object GDAInterpreter extends OptiMLApplicationInterpreter with GDA

trait GDA extends OptiMLApplication { 
  def printUsage = {
    println("Usage: GDA <input data file> <output label data file>")
    exit(-1)
  }

  def main() = {
    // if (args.length < 2) printUsage
    
    // testing:
    val z = DenseVector.zeros(10)
    z.pprint    
    val zf = DenseVector.zerosf(10)
    zf.pprint    
    val o = DenseVector.ones(10)
    o.pprint    
    val u = DenseVector.uniform(0,1.0,10)
    u.pprint    
    val r = DenseVector.rand(10)
    r.pprint
    
    val rowA = DenseVector(11., 22., 33.)
    val rowB = DenseVector(-5.3, -17.2, -131.)
    val rowD = DenseVector(-1.1, -6.2)
    val colC = DenseVector(7., 3.2, 13.3).t

    // A*B piecewise
    val ansVec = rowA*rowB
    ansVec.pprint
    // collect(check(ansVec, DenseVector(-58.3, -378.4, -4323.)))    
    
    val y = (o+5)+u
    y.pprint
    
    println("y(5): " + y(5))
    println("ys: ")
    val ys = y.slice(3,6)
    ys.pprint
        
    println("ys dot ys: " + (ys *:* ys))
    
    // testing flatten
    val vflat = DenseVector.flatten((DenseVector(rowA,rowB)))
    vflat.pprint
  }
}

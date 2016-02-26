import optima.compiler._
import optima.library._
import optima.shared._

object TestCompiler extends OptiMAApplicationCompiler with Test
object TestInterpreter extends OptiMAApplicationInterpreter with Test

trait Test extends OptiMAApplication {
  def main() = {

    if (stagingArgs.length != 0) {
      System.out.println("You lose! Good day sir!")
      sys.exit(-1)
    }

    val m = Array2D[Int](5, 5)
    m.pprint
  }
}
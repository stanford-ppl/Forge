import optima.compiler._
import optima.library._
import optima.shared._

object TestCompiler extends OptiMAApplicationCompiler with Test
object TestInterpreter extends OptiMAApplicationInterpreter with Test

trait Test extends OptiMAApplication {
  def main() = {

    if (stagingArgs.length != 0) {
      println("You lose!")
      sys.exit(-1)
    }


    val inds = Indices(1, 2, 3)
    println(inds(0))
    println(inds(2))
  }
}
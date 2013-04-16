import LOWERCASE_DSL_NAME.compiler._
import LOWERCASE_DSL_NAME.library._
import LOWERCASE_DSL_NAME.shared._

object HelloWorldCompiler extends HUMAN_DSL_NAMEApplicationCompiler with HelloWorld 
object HelloWorldInterpreter extends HUMAN_DSL_NAMEApplicationInterpreter with HelloWorld 

trait HelloWorld extends HUMAN_DSL_NAMEApplication { 
  def main() = {
    println("hello world")    
  }
}

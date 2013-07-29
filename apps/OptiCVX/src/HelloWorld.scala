import opticvx.compiler._
import opticvx.library._
import opticvx.shared._

// This object lets us run the Delite version of the code
object HelloWorldCompiler extends OptiCVXApplicationCompiler with HelloWorld 

// This object lets us run the Scala library version of the code
object HelloWorldInterpreter extends OptiCVXApplicationInterpreter with HelloWorld 

trait HelloWorld extends OptiCVXApplication { 
  def main() = {
    println("Hello, World!")    
    if(args.length > 0) println("You passed in: " + args(0))
    
    // create a new ParType
    // calls our static apply function
    val myParType = ParType[Int](10)
    
    var i = 1
    //  call our length and update methods
    while (i < myParType.length) {
      myParType(i) = i*3
      i += 1
    }

    // calls our zip function - this runs in parallel!
    val twiceAsBig = myParType + myParType

    // check out the fifth element. Hopefully it is 30!
    println("The Fifth Element : " + twiceAsBig(5))
  }
}

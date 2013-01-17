import simplevector.compiler._
import simplevector.library._
import simplevector.shared._

object HelloSimpleCompiler extends SimpleVectorApplicationCompiler with HelloSimple 
object HelloSimpleInterpreter extends SimpleVectorApplicationInterpreter with HelloSimple 

trait HelloSimple extends SimpleVectorApplication { 
  def main() = {
    println("hello world")    

    val v1 = Vector[Int](10)
    val v2 = Vector[Int](10)
    
    var i = 0    
    while (i < v1.length) {
      v1(i) = i*2
      v2(i) = i
      println("v1(" + i + "): " + v1(i))
      println("v1(" + i + "): " + v2(i))
      i += 1
    }
    
    val v3 = v1+v2
    i = 0
    println("v3 = v1+v2")
    while (i < v3.length) {
      println("v3(" + i + "): " + v3(i))
      i += 1
    }

    println("v4 = v3.slice(3,5)")
    val v4 = v3.slice(3,5)
    i = 0
    while (i < v4.length) {
      println("v4(" + i + "): " + v4(i))
      i += 1
    }    
  }
}

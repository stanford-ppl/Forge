import optiwrangle.compiler._
import optiwrangle.library._
import optiwrangle.shared._

object HelloWranglerCompiler extends OptiWrangleApplicationCompiler with HelloWrangler 
object HelloWranglerInterpreter extends OptiWrangleApplicationInterpreter with HelloWrangler 

trait HelloWrangler extends OptiWrangleApplication { 
  def main() = {
    println("hello world")    

    val flickr = new Table("~/ppl/data/flickr_test.txt")

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
  }
}

import simplevector.compiler._
import simplevector.library._
import simplevector.shared._

object ArrayPlayCompiler extends SimpleVectorApplicationCompiler with ArrayPlay
object ArrayPlayInterpreter extends SimpleVectorApplicationInterpreter with ArrayPlay

trait ArrayPlay extends SimpleVectorApplication { 
  def main() = {
    val a = Array(1,2,3,4)
    val b = Array(5.,6.,7.)

    var i = 0
    println("a: ")
    while (i < a.length) {
      println("  " + a(i))
      i += 1
    }

    i = 0
    println("b: ")
    while (i < b.length) {
      println("  " + b(i))
      i += 1
    }
    
    // mutability
    // not the best error message if we forget the type parameter
    val out = Array.empty[Array[Int]](10) // [ null, null, ... ]
    
    // no problem, because a is immutable
    out(0) = a
    println("out(0)(2) = " + out(0).apply(2)) // gets confused when we chain the applies.. :/
     
    val c = Array.empty[Int](10)
    i = 0
    while (i < c.length) {
      c(i) = -1*i
      i += 1
    }
    out(1) = c.Clone
        
    // test args:
    if (args.length > 0) println("got arg: " + args(0))
  }
}
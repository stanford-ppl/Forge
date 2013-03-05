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
    val out = Array.empty[Array[String]](10) // [ null, null, ... ]
    
    // no problem, because a is immutable
    //out(0) = a
    //println("out(0)(2) = " + out(0).apply(2)) // gets confused when we chain the applies.. :/
     
    val c = Array.empty[String](10)
    i = 0
    while (i < c.length) {
      c(i) = "hi"//.toString //(-1*i).toString
      i += 1
    }
    c(0) = 3.toString
    c(1) = (-1*i).toString
/*
    val d = c.map(addOne)
    out(0) = d.Clone
   
    println("out(0)(0) = " + out(0).apply(0)) // gets confused when we chain the applies.. :/
    println("out(0)(1) = " + out(0).apply(1)) // gets confused when we chain the applies.. :/
    println("out(0)(2) = " + out(0).apply(2)) // gets confused when we chain the applies.. :/
    // test args:
    if (args.length > 0) println("got arg: " + args(0))
*/
  }

  def addOne(x: String) : String = x + " one"
}

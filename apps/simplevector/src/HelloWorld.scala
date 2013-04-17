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
    
    // defs
    var i = 0    
    while (i < v1.length) {
      v1(i) = i*2
      v2(i) = i
      println("v1(" + i + "): " + v1(i))
      println("v2(" + i + "): " + v2(i))
      i += 1
    }
    
    // zip
    val v3 = v1+v2
    i = 0
    println("v3 = v1+v2")
    while (i < v3.length) {
      println("v3(" + i + "): " + v3(i))
      i += 1
    }

    // single
    println("v4 = v3.slice(3,5)")
    val v4 = v3.slice(3,5)
    i = 0
    while (i < v4.length) {
      println("v4(" + i + "): " + v4(i))
      i += 1
    }    
    
    // map
    println("v5 = v4*5")
    val v5 = v4*5
    i = 0
    while (i < v5.length) {
      println("v5(" + i + "): " + v5(i))
      i += 1
    }      
    
    // reduce
    println("v5.sum:")
    val z = v5.sum
    println(z)
 
    // basic
    val pp = v5.basic()
    val pq = v5.basic(2)
    val pr = v5.basic(2, 3)
    val ps = v5.basic(y=4, z=5)
    val pt = v5.basic(z=6, y=7)
    val pu = v5.basic(y=8)
    val pv = v5.basic(z=9)

    println("5 and 13?")
    println(pp + " " + pv)

    val vdouble = Vector[Double](3)   
    val vdz = vdouble.foo(77.7)
    println("0?")
    println(vdz(0))

    val all_ones = v5.set(1, 2)
    println("3?")
    println(all_ones(0))
    println("5?")
    println(v5.set(x=2).apply(0))
    
    // bar
    val vb = v2.bar(e => e*3)
    println("vb: ")
    vb.pprint
   
    // foreach
    println("v1.pprint:")
    v1.pprint
    
    // filter
    // TODO: not working yet
    //
    // println("v6 = v1.filter(_ < 5)")
    // val v6 = v1.filter(_ < 5)
    // v6.pprint
  
  }
   
}

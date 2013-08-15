import simpleintvector.compiler._
import simpleintvector.library._
import simpleintvector.shared._

object HelloSimpleIntCompiler extends SimpleIntVectorApplicationCompiler with HelloSimpleInt
object HelloSimpleIntInterpreter extends SimpleIntVectorApplicationInterpreter with HelloSimpleInt

trait HelloSimpleInt extends SimpleIntVectorApplication {
  def main() = {
    println("hello world")

    val v1 = Vector(10)
    val v2 = Vector(10)

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

    // foreach
    println("v1.pprint:")
    v1.pprint

    // map,reduce,mapreduce
    val vc = v1.map(e => e+2)
    println("vc.pprint:")
    vc.pprint
    val vc2 = vc.mapreduce(e => e-1, (a,b) => a+b)
    println("vc2: " + vc2)

    // filter
    println("v6 = v1.filter(_ < 5)")
    val v6 = v1.filter(_ < 5)
    v6.pprint

    // foo
    // val fooResult = foo[Int](i => i+100, 2, 13, i => i + 5, 0.7, d => d)
    // // a0 == a1 == ... an = 118
    // // z == 0.7
    // // y = 15
    // println("fooResult: " + fooResult)
  }

}

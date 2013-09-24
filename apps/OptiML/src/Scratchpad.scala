import optiml.compiler._
import optiml.library._
import optiml.shared._

import scala.virtualization.lms.common.Record
import scala.reflect.{SourceContext}

object ScratchpadCompiler extends OptiMLApplicationCompiler with Scratchpad
object ScratchpadInterpreter extends OptiMLApplicationInterpreter with Scratchpad

trait Scratchpad extends OptiMLApplication {

  def main() = {
    // lapack!

    val A = DenseMatrix.rand(10,5)
    val b = DenseVector.rand(10).t
    val x = A\b
    x.pprint

    // testing fusion of collect and foreach

    // fuses now (more composites to make multiloops and struct unwrapping visible, primitive rewrites, using same loop shapes for matrix constructor and buffered write, ...)
    // val m = DenseMatrix[Double](100,100)
    // val t = DenseMatrix.zeros(100,100).map(e => e+1)
    // t.write(m)
    // m.pprint

    // this does fuse (because matrix constructor is a foreach?):
    // val m = DenseMatrix[Double](100,100)
    // val t = (0::100,0::100) { (i,j) => i+j+.01 }
    // t.write(m)
    // m.pprint

    // testing fusion of untilconverged_buffered

    // this apparently does not:
    // def diff(m1: Rep[DenseMatrix[Double]], m2: Rep[DenseMatrix[Double]]) = unit(10.0)

    // val m = DenseMatrix.rand(100,100)*100
    // val out = untilconverged_buffered(m, maxIter = 10) { old =>
    //   old / 10
    // }(implicitly[Bufferable[DenseMatrix[Double]]],manifest[DenseMatrix[Double]],implicitly[SourceContext],diff)

    // out.pprint

    // if (args(0).toInt > 10) {
      // TEST: r should be hoisted, even though it is within the outer conditional
      // RESULT: it does still get hoisted. great, but why? is this due to summarizeEffects removing control dependencies?
      //         it should still be explicitly reflected by the conditional scope, no?

      // is it because the Reflect of the vector constructor node does *not* contain the inner effects, so that boundSyms
      // does not pick it up as as an effectful sym?

      // what is effectSyms(op.func) for the map?
      // the map should be a Reify(VectorDivide(a,b), u, effects = List(DenseMatrixOnes,MatrixGetRow,VectorDivide))

      // but somehow DenseMatrix2Object_Ones is actually pure. why?
      // DeliteOpLoops use:
      //    reflectEffect(d, re andAlso be)
      //
      // why bypasses the super toAtom call that adds the control dependency, since summarizeEffects has already stripped it.

      // what are the consequences? a Delite op loop can never be a control effect. Therefore, something like
      //
      //   if (foo) { unsafe_loop_not_foo } else { safe_loop_foo }
      //
      // can still break

      // what if we:
      //   1. make IfThenElse strip the control effect instead of summarizeEffects
      //   2. only add control effects one-level down (in reifyEffects)

      // the result should be: IndexVectorConstruct gets a control effect, but DenseMatrix.ones does not


    //   val z = (0::100) { i => val r = DenseMatrix.ones(100,100); r(i)/10 }
    //   println("z.sum: " + z.sum)
    // }

    // val m = DenseMatrix(DenseVector(1,2,3),DenseVector(4,5,6),DenseVector(7,8,9))
    // m.pprint

    // val v = DenseVector(DenseVector(1,2,3),DenseVector(4,5,6),DenseVector(7,8,9))
    // v.pprint

    // println("log(1,2,3,4,5) = ")
    // log(DenseVector(1,2,3,4,5)).pprint
    // println("mean(1,2,3,4,5) = " + mean(1,2,3,4,5))
    // println("sum(1,2,3,4,5) = " + sum(1,2,3,4,5))
    // println("pow(3.0,5.0) = " + pow(3.0,5.0))
    // println("min(7,9,0,5,-3) = " + min(7,9,0,5,-3))
    // println("minIndex(7,9,0,5,-3) = " + DenseVector(7,9,0,5,-3).minIndex)
    // println("max(7,9,0,5,-3) = " + max(DenseVector(7,9,0,5,-3)))

    // val v1 = DenseVector(1,2,3,4,5).toDouble
    // println("v1: ")
    // v1.pprint

    // val v2 = DenseVector(6,7,8,9,10).toDouble
    // println("v2: ")
    // v2.pprint

    // println("dist(v1,v2) = " + dist(v1,v2))
    // println("dist(v1,v2,SQUARE) = " + dist(v1,v2,SQUARE))

    // println("m1: ")
    // val m1 = (0::10,0::10) { (i,j) => i+j }
    // m1.t.pprint

    // println("m2: ")
    // val m2 = (0::10, *) { i => DenseVector.rand(5) }
    // m2.pprint

    // writeVector(v1, "v.dat")
    // val vv1 = readVector("v.dat")
    // println("re-read v1: ")
    // vv1.pprint

    // writeMatrix(m2, "m.dat")
    // val mm2 = readMatrix("m.dat")
    // println("re-read m2: ")
    // m2.pprint


    // val m = DenseMatrix.rand(100,50)
    // val v = DenseVector.rand(100).map(e => if (e > 0.5) true else false)
    // val t = TrainingSet(m,v)

    // println("numSamples: " + t.numSamples + ", numFeatures: " + t.numFeatures)
    // println("data: ")
    // t.data.pprint

    // println("labels: ")
    // t.labels.pprint

    // println("t(10,32): " + t(10,32))

    // println("t(15):")
    // t(15).pprint

    // val s = TestSet(DenseMatrix.rand(100,50))
    // println("s.numSamples: " + s.numSamples)

    // val strVec = readVector[String]("/tmp/foo.dat", tokens => tokens(0))
    // strVec.pprint

    // val m2 = readMatrix[Double]("/tmp/bar.dat", s => (s.fsplit("\\.")).apply(0).toInt, ";")
    // m2.pprint

    // type alias is for convenience
    // type MyStruct = Record{val data: Int; val name: String; val z: Rep[DenseVector[Int]]}

    // // method to construct a new instance of MyStruct, also for convenience
    // def newMyStruct(_data: Rep[Int], _name: Rep[String], _z: Rep[DenseVector[Int]]) =
    //   // a user-defined struct instance is declared as a new Record
    //   new Record {
    //     val data = _data
    //     val name = _name
    //     val z = _z
    //   }

    // // we can use our struct with normal OptiML data types
    // val v1 = (0::100) { i => newMyStruct(i, "struct " + i, DenseVector[Int](10)) }
    // v1.map(s => s.name).pprint
  }
}

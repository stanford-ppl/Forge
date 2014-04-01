/* Testing complex number functionality
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: 12/3/12
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

import optiml.compiler._
import optiml.shared._
import optiml.library._
import ppl.tests.scalatest._

object ComplexMathRunnerC extends ForgeTestRunnerCompiler with OptiMLApplicationCompiler with ComplexMath
object ComplexMathRunnerI extends ForgeTestRunnerInterpreter with OptiMLApplicationInterpreter with ComplexMath
trait ComplexMath extends ForgeTestModule with OptiMLApplication {
  def main() = {
    val a = Complex(5,2) 
    val b = Complex(3,-11) 
     
    val c1 = a+b
    collect(c1 == Complex(8,-9))
    val c2 = a-b
    collect(c2 == Complex(2,13))
    val c3 = a*b
    collect(c3 == Complex(37,-49))
    val c4 = a/b
    collect(abs(c4.real)-0.053846 < .01)
    collect(abs(c4.imag)-0.469231 < .01)
    val c5 = a.exp
    collect(abs(c5.real)-61.761667 < .01)
    collect(abs(c5.imag)-134.951704 < .01)
    val c6 = a.log
    collect(abs(c6.real)-1.68365 < .01)
    collect(abs(c6.imag)-0.38051 < .01)    
    val c7 = a.abs
    collect(abs(c7.real)-5.385165 < .01)
    val c8 = a.conj
    collect(c8 == Complex(5,-2))
    
    val m = DenseMatrix(((Complex(3,1),Complex(5,0),Complex(0,-2))),((Complex(2,-2),Complex(0,1),Complex(-7,-13))))
    val delta = (m.t.map(_.conj) - DenseMatrix(((Complex(3,-1),Complex(2,2))), ((Complex(5,-0), Complex(0,-1))), ((Complex(0,2), Complex(-7,13)))))
    collect(sum(abs(delta)).real < .01)

    mkReport
  }
}

class ComplexSuiteInterpreter extends ForgeSuiteInterpreter {
  def testComplexMath() { runTest(ComplexMathRunnerI) }
}
class ComplexSuiteCompiler extends ForgeSuiteCompiler {
  def testComplexMath() { runTest(ComplexMathRunnerC) }
}

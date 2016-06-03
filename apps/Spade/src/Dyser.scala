import spade.compiler._
import spade.library._
import spade.shared._
import scala.collection.mutable.ListBuffer

object DyserCompiler extends SpadeApplicationCompiler with Dyser
object DyserInterpreter extends SpadeApplicationInterpreter with Dyser

trait Dyser extends SpadeApplication {
  def main() = {
    val a00 = ALU()
    val s00 = Switch()
    val s01 = Switch()
    val s10 = Switch()
    val s11 = Switch()
    val a01 = ALU()
    val s02 = Switch()
    val s12 = Switch()
    val a10 = ALU()
    val a11 = ALU()
    val s20 = Switch()
    val s21 = Switch()
    val s22 = Switch()

    s00 <-> s01
    s00 <-> s10
    s01 <-> s02
    s01 <-> s11
    s10 <-> s11
    s11 <-> s12
    s02 <-> s12

    s10 <-> s20
    s11 <-> s21
    s12 <-> s22
    s20 <-> s21
    s21 <-> s22


    a00 <-> s00
    a00 <-> s01
    a00 <-> s10
    a00 <-> s11

    a01 <-> s01
    a01 <-> s02
    a01 <-> s11
    a01 <-> s12

    a10 <-> s10
    a10 <-> s11
    a10 <-> s20
    a10 <-> s21

    a11 <-> s11
    a11 <-> s12
    a11 <-> s21
    a11 <-> s22

  }
}

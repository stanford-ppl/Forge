package ppl.dsl.forge
package templates
package shared

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._
import core._
import Utilities._

trait BaseGenLifts extends ForgeCodeGenBase {
  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  def emitLifts(grp: Exp[DSLGroup], lifts: ArrayBuffer[Exp[DSLType]], stream: PrintWriter) {
    emitBlockComment("Lift", stream)
    stream.println()
    stream.println("trait Lift" + grp.name + " {")
    stream.println("  this: " + dsl + " => ")
    stream.println()
    for (tpe <- lifts) {
      stream.print("  implicit def " + grp.name + tpe.name + "ToRep")
      // tpe could be a type arg!
      tpe match {
        case Def(TpePar(n,b,s)) => stream.print(makeTpeParsWithBounds(List(tpe.asInstanceOf[Exp[TypePar]])))
        case _ => stream.print(makeTpeParsWithBounds(tpe.tpePars))
      }
      stream.println("(x: " + quote(tpe) + ") = unit(x)")
    }
    stream.println("}")
  }
}


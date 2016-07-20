package dhdl.graph.traversal

import dhdl.graph._
import dhdl._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set

class IRPrinter(implicit design: Design) extends DFSTraversal with Printer{

  override val stream = Printer.newStream("PIR.txt") 

  override def initPass() = {
    super.initPass
  }

  def getFields(node:Node):String = {
    val fields = ListBuffer[String]()
    node match {
      case n:ComputeUnit =>
        fields += s"parent=${n.parent}"
        fields += s"type=${n.tpe}"
      case n:Primitive => {
        fields += s"ctrler=${n.ctrler}"
        n match {
          case p:CounterChain =>
            fields += s"copy=${p.copy.getOrElse("None")}"
          case p:SRAM =>
            fields += s"size=${p.size}, RA=${p.readAddr}, WA=${p.writeAddr}, RP=${p.writeAddr}"
          case p:Stage =>
            fields += s"operands=[${p.operands.mkString(",")}], op=${p.op}, result=${p.result}"
          case p:Counter => 
            fields += s"min=${p.min}, max=${p.max}, step=${p.step}"
          case _ =>
        }
      }
      case w:Const => fields += s"${w.value}"
      case _ =>
    }
    s"(${if (fields.size>0) fields.reduce(_+", "+_) else ""})"
  }
  def emitBlock(node:Node):Unit = {
    emitBS
    super.visitNode(node)
    emitBE
  }

  override def visitNode(node: Node) : Unit = {
    emit(s"${node}${getFields(node)}")
    node match {
      case n:Const => emitln(s"")
      case n:SRAM => emitln(s"")
      case n:Stage => emitln(s"")
      case n:Counter => emitln(s"")
      case n => emitBlock(node)
    }
  }

  override def finPass() = {
    println("-------- Finishing IR Printing----------")
    close
  }
}

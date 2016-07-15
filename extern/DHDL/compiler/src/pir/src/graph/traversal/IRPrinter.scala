package dhdl.graph.traversal

import dhdl.graph._
import dhdl.Design
import dhdl.Config

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set

class IRPrinter(implicit design: Design) extends Traversal{

  override def initPass() = {
    super.initPass
  }

  var level = 0
  val tab = "  "
  def emit(s:String) = print(s"${tab*level}${s}")
  def emitln(s:String) = println(s"${tab*level}${s}")

  def emitBS = { println(s"{"); level += 1 }
  def emitBE = { level -= 1; emit(s"}") }

  def emitField(node:Node):Unit = {
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
            fields += s"min=${p.bound._1}, max=${p.bound._2}, step=${p.bound._3}"
          case _ =>
        }
      }
      case w:Const => fields += s"${w.value}"
      case _ =>
    }
    print(s"(${if (fields.size>0) fields.reduce(_+", "+_) else ""})")
  }
  def emitBlock(node:Node):Unit = {
    emitBS
    super.visitNode(node)
    emitBE
  }

  override def visitNode(node: Node) : Unit = {
    emit(s"${node}")
    emitField(node)
    node match {
      case n:Const => 
      case n:SRAM =>
      case n:Stage =>
      case n:Counter =>
      case n => emitBlock(node)
    }
    println(s"")
  }

  override def finPass() = {}
}

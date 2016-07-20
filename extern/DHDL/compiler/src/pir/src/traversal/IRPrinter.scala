package dhdl.graph.traversal

import dhdl.graph._
import dhdl._
import dhdl.PIRMisc._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

class IRPrinter(implicit design: Design) extends DFSTraversal with Printer{

  override val stream = Printer.newStream("PIR.txt") 

  override def initPass() = {
    super.initPass
  }

  def genFields(node:Node):String = {
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
          case p:Reg => p match {
            case r:PipeReg =>
            case r:Const => fields += s"${r.value}"
            case r:ArgIn =>
            case r:ArgOut =>
          }
          case _ =>
        }
      }
      case _ =>
    }
    s"(${if (fields.size>0) fields.reduce(_+", "+_) else ""})"
  }

  private def toStr(mp:Map[String, String], s:String, i:Int) = mp += (s -> i.toString)
  private def toStr(mp:Map[String, String], s:String, l:ListBuffer[_]) = 
    if (l.size > 0) mp += (s -> s"[${l.mkString(",")}]")
  private def toStr(mp:Map[String, String], s:String, m:Map[_,_]) = 
    if (m.size > 0) {
      val mstrs = m.map{case (k, v) => v match {
          case n:Set[_] => s"${k}->{${n.map{i => s"${i}"}.mkString(",")}}"
          case n:Map[_,_] => s"${k}->{${n.map{case (kk, vv) => s"${vv}"}.mkString(",")}}"
          case _ => s"${k}->${v}"
        } 
      }
      mp += (s -> s"[${mstrs.mkString(",")}]")
    }
  def regMapToStrs(p:Pipeline):Map[String, String] = {
    var m = HashMap[String, String]()
    toStr(m, "reduceReg" , p.reduceReg  )
    toStr(m, "vecIn"     , p.vecIn      )
    toStr(m, "vecOut"    , p.vecOut     )
    toStr(m, "scalarIns" , p.scalarIns  )
    toStr(m, "scalarOuts", p.scalarOuts )
    toStr(m, "loadRegs"  , p.loadRegs   )
    toStr(m, "storeRegs" , p.storeRegs  )
    toStr(m, "ctrRegs"   , p.ctrRegs    )
    toStr(m, "tempRegs"  , p.tempRegs   )
    toStr(m, "stageUses" , p.stageUses  )
    toStr(m, "stageDefs" , p.stageDefs  )
    toStr(m, "stagePRs"  , p.stagePRs   )
    m
  }

  def emitBlock(title:String, node:Node):Unit = {
    emitBS(title)
    node match {
      case n:ComputeUnit =>
        emitBS(s"mapping=")
        regMapToStrs(n.pipeline).foreach { case (k, v) =>
          emitln(s"${k}:${v}")
        }
        emitBE
      case _ =>
    }
    super.visitNode(node)
    emitBE
  }

  override def visitNode(node: Node) : Unit = {
    node match {
      case n:Controller => emitBlock(s"${node}${genFields(node)}", node)
      case n:CounterChain => emitBlock(s"${node}${genFields(node)}", node)
      case n:Pipeline => emitBlock(s"${node}${genFields(node)}", node)
      case _ => emitln(s"${node}${genFields(node)}")
    }
  }

  override def finPass() = {
    info("Finishing IR Printing")
    close
  }
}

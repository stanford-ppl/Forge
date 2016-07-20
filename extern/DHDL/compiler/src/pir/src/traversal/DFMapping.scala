package dhdl.graph.traversal
import dhdl.graph._
import dhdl.Design
import dhdl.Config
import dhdl.graph.mapping._

class DFMapping(implicit val design: Design) extends Traversal{

  val mapping = new CUMapping() 

  override def reset = {
    mapping.reset
  }

  override def traverse = {
    val (suc, hints) = mapping.map
    if (suc)
      println(s"--------- mapping successed --------")
    else
      println(s"--------- mapping failed -----------")
    hints.foreach { h =>
      println(h)
    }
  } 

  override def finPass = {
    println("-------- Finishing Depth First Mapping ----------")
    mapping.printMap
  }

}

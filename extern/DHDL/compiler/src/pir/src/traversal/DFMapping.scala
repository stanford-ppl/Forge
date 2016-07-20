package dhdl.graph.traversal
import dhdl.graph._
import dhdl._
import dhdl.graph.mapping._

class DFMapping(implicit val design: Design) extends Traversal{
  import DFMapping._

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
    close
  }
}
object DFMapping extends Printer {
  override val stream = Printer.newStream("Mapping.txt") 
}

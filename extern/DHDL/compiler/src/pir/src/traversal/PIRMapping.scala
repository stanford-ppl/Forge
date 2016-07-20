package dhdl.graph.traversal
import dhdl.graph._
import dhdl._
import dhdl.PIRMisc._
import dhdl.graph.mapping._

class PIRMapping(implicit val design: Design) extends Traversal{
  import PIRMapping._

  val mapping = new CUMapping() 

  override def reset = {
    mapping.reset
  }

  override def traverse = {
    val (suc, hints) = mapping.map
    if (suc)
      info(s"Mapping succeeded") 
    else
      info(s"Mapping failed")
    if (Config.debug) {
      hints.foreach { h =>
        dprintln(h)
      }
    }
  } 

  override def finPass = {
    info("Finishing PIR Mapping")
    mapping.printMap
    close
  }
}
object PIRMapping extends Printer {
  override val stream = Printer.newStream("Mapping.txt") 
}

package dhdl.codegen.dot

import dhdl.codegen.Codegen
import dhdl.graph._
import java.io.PrintWriter
import dhdl.Design

class GraphvizCodegen(file: String = "outfile")(implicit design: Design) extends Codegen {
  override val ext = "dot"

  // Constants
  val arrowSize = 0.6
  val edgeThickness = 0.5
  val memColor = "#6ce6e1"
  val regColor = "#8bd645"
  val offChipColor = "#1A0000"
  val dblbufBorderColor = "#4fb0b0"
  val ctrlColor = "red"
  val counterColor = "#e8e8e8"
  val counterInnerColor = "gray"
  val fontsize = 10
  val defaultShape = "square"
  val bgcolor = "white"

  // Metapipeline colors
  val mpFillColor = "#4FA1DB"
  val mpBorderColor = "#4FA1DB"
  val mpStageFillColor = "#BADDFF"
  val mpStageBorderColor = "none"

  // Parallel colors
  val parallelFillColor = "#4FDBC2"
  val parallelBorderColor = "#00AB8C"
  val parallelStageFillColor = "#CCFFF6"
  val parallelStageBorderColor = "none"

  val outfileName = s"$file.$ext"
  override def clearOldFiles = false

  // Initialized in initPass
  // TODO: Is there any way I can avoid using var?
  var pw: PrintWriter = null

  def fp(a: Any) = pw.println(a)


  override def run(node: Node) = {
    // Always call initPass first
    initPass()

    visitNode(node)

    finPass()
  }

  override def initPass() = {
    super.initPass()
  }

  override def finPass() = {
    fp(s"}")
    pw.flush()
    pw.close()
  }

  private def emitEdge(v1: Node, v2: Node, edgeProp: String = "") = {
  }

  override def visitNode(node: Node) : Unit = {
  }
}

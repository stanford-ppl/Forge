package dhdl

import graph._
import graph.traversal._
import plasticine._
import plasticine.config._
//import plasticine.graph._

//import analysis._

import codegen._
import codegen.dot._

import scala.language.implicitConversions
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import java.nio.file.{Paths, Files}
import scala.io.Source

import scala.collection.mutable.{Set,Map}

trait Design extends PIRMisc { self =>

  implicit val design: Design = self

  private var nextSym = 0
  def nextId = {nextSym += 1; nextSym }

  private val nodeStack = Stack[(Node => Boolean, ListBuffer[Node])]()
  val toUpdate = ListBuffer[(String, Node => Unit)]()
  val allNodes = ListBuffer[Node]()

  def reset() {
    nodeStack.foreach { case (f,i) => i.clear() }
    nodeStack.clear()
    allNodes.clear()
    nodeStack.push(((n:Node) => true), allNodes)
    toUpdate.clear()
    nextSym = 0
    top = null
    traversals.foreach(_.reset)
  }

  def addNode(n: Node) { 
    nodeStack.foreach { case (f,i) => if (f(n)) i+= n }
  }

  //def addBlock(block: => Any, f1:Node => Boolean, filters: Node => Boolean *):List[List[Node]] = {
  //  nodeStack.push((f1, ListBuffer[Node]()))
  //  filters.foreach { f => 
  //    nodeStack.push( (f, ListBuffer[Node]()) )
  //  }
  //  block
  //  (0 to filters.size).foldLeft(List[List[Node]]()) { case (a, i) =>
  //    nodeStack.pop()._2.toList :: a 
  //  }
  //}

  def addBlock[T](block: => Any, filter: Node => Boolean):List[T] = {
    nodeStack.push((filter, ListBuffer[Node]()))
    block
    nodeStack.pop()._2.toList.asInstanceOf[List[T]]
  }

  def addBlock[T1, T2](block: => Any,
                       f1: Node => Boolean,
                       f2: Node => Boolean
                       ):(List[T1], List[T2]) = {
    nodeStack.push((f1, ListBuffer[Node]()))
    nodeStack.push((f2, ListBuffer[Node]()))
    block
    val l2 = nodeStack.pop()._2.toList.asInstanceOf[List[T2]]
    val l1 = nodeStack.pop()._2.toList.asInstanceOf[List[T1]]
    (l1, l2)
  }

  def addBlock[T1, T2, T3](block: => Any,
                       f1: Node => Boolean, 
                       f2: Node => Boolean,
                       f3: Node => Boolean
                       ):(List[T1], List[T2], List[T3]) = {
    nodeStack.push((f1, ListBuffer[Node]()))
    nodeStack.push((f2, ListBuffer[Node]()))
    nodeStack.push((f3, ListBuffer[Node]()))
    block
    val l3 = nodeStack.pop()._2.toList.asInstanceOf[List[T3]]
    val l2 = nodeStack.pop()._2.toList.asInstanceOf[List[T2]]
    val l1 = nodeStack.pop()._2.toList.asInstanceOf[List[T1]]
    (l1, l2, l3)
  }

  def addBlock[T1, T2, T3, T4](block: => Any,
                       f1: Node => Boolean, 
                       f2: Node => Boolean,
                       f3: Node => Boolean,
                       f4: Node => Boolean
                       ):(List[T1], List[T2], List[T3], List[T4]) = {
    nodeStack.push((f1, ListBuffer[Node]()))
    nodeStack.push((f2, ListBuffer[Node]()))
    nodeStack.push((f3, ListBuffer[Node]()))
    nodeStack.push((f4, ListBuffer[Node]()))
    block
    val l4 = nodeStack.pop()._2.toList.asInstanceOf[List[T4]]
    val l3 = nodeStack.pop()._2.toList.asInstanceOf[List[T3]]
    val l2 = nodeStack.pop()._2.toList.asInstanceOf[List[T2]]
    val l1 = nodeStack.pop()._2.toList.asInstanceOf[List[T1]]
    (l1, l2, l3, l4)
  }


  def updateLater(s:String, f:Node => Unit) = { val u = (s,f); toUpdate += u }

  def msg(x: String) = if (Config.dse) () else println(x)
  val tab = "  "
  var level = 0

  val arch:Spade
  var top:Top = _

    //if (Config.genDot) {
    //  val origGraph = new GraphvizCodegen(s"orig")
    //  origGraph.run(top)
    //}

    //if (Config.genMaxJ) {
    //  if (Config.genDot) {
    //    msg("Generating dot graph")
    //    val dot = new GraphvizCodegen()
    //    dot.run(transformedTop)
    //  }
    //}
  val traversals = ListBuffer[Traversal]()
  traversals += new ForwardRef()
  traversals += new IRPrinter()
  val dfmapping = new DFMapping()
  traversals += dfmapping 

  reset()

  def run = traversals.foreach(_.run)

}

trait PIRApp extends Design{
  override val arch = Config0 

  def main(args: String*): Any 
  def main(args: Array[String]): Unit = {
    msg(args.mkString(", "))
    val ctrlNodes = addBlock(main(args:_*), (n:Node) => n.isInstanceOf[Controller])
    top = Top(ctrlNodes)
    println("-------- Finishing graph construction ----------")
    run
  }
}

trait PIRMisc {
  implicit def reg_to_port(reg:Reg):Port = {
    reg.out
  }
  implicit def ctr_to_port(ctr:Counter):Port = {
    ctr.out
  }
}


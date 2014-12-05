/* Unit tests for OptiGraph.
 *
 * author:   Christopher Aberger (caberger@stanford.edu)
 * created:  June 15, 2014
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import optigraph.compiler._
import optigraph.library._
import optigraph.shared._
import ppl.tests.scalatest._

/*
  Some starter tests for directed and undirected graphs.
  Indirectly the small chunks of code  below touch a large amount of 
  the OptiGraph code base through IO and helper traits.
*/

object UndirectedGraphI extends ForgeTestRunnerInterpreter with OptiGraphApplicationInterpreter with UndirectedGraphTest
object UndirectedGraphC extends ForgeTestRunnerCompiler with OptiGraphApplicationCompiler with UndirectedGraphTest
trait UndirectedGraphTest extends ForgeTestModule with OptiGraphApplication {
  def main() {
    val g = undirectedGraphFromEdgeList(createMeshEdgeList(7))
    val sum = sumOverNodes(g.nodes){n => n.id}
    val nd = g.mapNodes{n => n.id*2}

    collect(g.numNodes == 7 && g.numEdges == 42 
      && sum == 21 && g.isDirected == false && nd(0) == 0
      && nd(1) == 2 && nd(2) == 4 && nd(3) == 6 && nd(4) == 8
      && nd(5) == 10 && nd(6) == 12 && nd.length == 7)

    val nbr = g.neighbors(Node(1))
    val sumNbr = sumOverNeighbors(nbr){w => w.id}

    collect(nbr.length == 6 && sumNbr == 20)

    mkReport
  }
}

object DirectedGraphI extends ForgeTestRunnerInterpreter with OptiGraphApplicationInterpreter with DirectedGraphTest
object DirectedGraphC extends ForgeTestRunnerCompiler with OptiGraphApplicationCompiler with DirectedGraphTest
trait DirectedGraphTest extends ForgeTestModule with OptiGraphApplication {
  def main() {
    val g = directedGraphFromEdgeList(createMeshEdgeList(7))
    val sum = sumOverNodes(g.nodes){n => n.id}

    val nd = g.mapNodes{n => n.id*2}

    collect(g.numNodes == 7 && g.isDirected == true
      && sum == 21 && g.numEdges == 84 && nd(0) == 0
      && nd(1) == 2 && nd(2) == 4 && nd(3) == 6 && nd(4) == 8
      && nd(5) == 10 && nd(6) == 12 && nd.length == 7) 

    val inNbr = g.inNeighbors(Node(6))

    val sumDownNbr = sumOverNeighbors(inNbr){w => w.id}
    val sumUpNbrs = sumOverNeighbors(g.outNeighbors(Node(5))){w => w.id}

    collect(inNbr.length == 6 && sumUpNbrs == 16 && sumDownNbr == 15)

    mkReport
  }
}

class GraphSuiteInterpreter extends ForgeSuiteInterpreter {
  def testUndirectedGraph() { runTest(UndirectedGraphI) }
  def testDirectedGraph() { runTest(DirectedGraphI) }
}

class GraphSuiteCompiler extends ForgeSuiteCompiler {
  def testUndirectedGraph() { runTest(UndirectedGraphC) }
  def testDirectedGraph() { runTest(DirectedGraphC) }
}
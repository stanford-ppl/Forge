package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

// This object lets us build our DSL
object OptiGraphDSLRunner extends ForgeApplicationRunner with OptiGraphDSL

trait OptiGraphDSL extends ForgeApplication
    with GraphOps with DirectedGraphOps  with UndirectedGraphOps
    with NodeOps with EdgeOps  with NodeDataOps with NeighborViewOps
    with NodeIdViewOps  with AtomicIntArrayOps with AtomicBooleanOps 
    with IOGraphOps with CommunityOps with AtomicDoubleArrayOps {
  /**
   * The name of our DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  override def dslName = "OptiGraph"
    
  /**
   * The specification is the DSL definition (types, data structures, ops, code generators)
   */
  override def specification() = {
    /**
     * Include Scala ops
     */
    importScalaOps()
    importBitSetOps()
    /**
     * The main portion of our DSL
     */
    importNodeOps() //Primitives.scala
    importEdgeOps() //Primitives.scala
    importNodeDataOps() //NodeData.scala
    importAtomicBooleanOps() //Atomics.scala
    importAtomicIntArrayOps() //Atomics.scala
    importAtomicDoubleArrayOps() //Atomics.scala
    importNeighborViewOps() //NeighborView.scala
    importNodeIdViewOps()  //NodeIdView.scala
    importGraphAggregateOps() //Graph.scala
    importUndirectedGraphOps() //UndirectedGraph.scala
    importDirectedGraphOps() //DirectedGraph.scala
    importIOGraphOps() //IOGraph.scala
    importCommunityOps() //Community.scala
  }
} 

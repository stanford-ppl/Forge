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
    importKeyValueStoreOps()

    /**
     * Expose some arrays ops (used in PageRankCSR right now)
     */
    val T = tpePar("T")
    val R = tpePar("R")
    val GArray = grp("GArrays")
    direct (GArray) ("garray_fromfunction", T, (MInt, MInt ==> T) :: MArray(T)) implements composite ${ array_fromfunction($0, $1) }
    direct (GArray) ("garray_reduce", T, (MArray(T), (T,T) ==> T, T) :: T) implements composite ${ array_reduce($0, $1, $2) }    
    infix (GArray) ("map", (T,R), (MArray(T), T ==> R) :: MArray(R)) implements composite ${ array_map($0, $1) }

    /**
     * The main portion of our DSL
     */
    importNodeOps() //Primitives.scala
    importEdgeOps() //Primitives.scala
    importNodeDataOps() //NodeData.scala
    importNodeDataDBOps() //NodeData.scala
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

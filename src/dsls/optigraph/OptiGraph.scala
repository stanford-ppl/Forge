package ppl.dsl.forge
package dsls 
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

// This object lets us build our DSL
object OptiGraphDSLRunner extends ForgeApplicationRunner with OptiGraphDSL

trait OptiGraphDSL extends ForgeApplication
    with GraphOps with DirectedGraphOps  with UndirectedGraphOps with CSRGraphOps with AOAGraphOps with AOHashSetGraphOps
    with NodeOps with EdgeOps  with NodeDataOps with NodeDataViewOps with NodeCollectionOps
    with NodeIdViewOps  with AtomicIntArrayOps with AtomicBooleanOps with BitSetGraphOps
    with NodeSHashOps with IOGraphOps with SpecUndirectedGraphOps with ParBitSetOps {
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
    /**
     * The main portion of our DSL
     */
    importNodeOps()
    importEdgeOps()
    importNodeDataOps()
    importNodeSHashOps()
    importAtomicBooleanOps()
    importAtomicIntArrayOps()
    importNodeDataViewOps()
    importNodeIdViewOps()
    importParBitSetOps()
    importGraphAggregateOps()
    importUndirectedGraphOps()
    importNodeCollectionOps()
    importCSRGraphOps()
    importBitSetGraphOps()
    importAOAGraphOps()
    importAOHashSetGraphOps()
    importSpecUndirectedGraphOps()
    importDirectedGraphOps()
    importIOGraphOps()
  }
} 

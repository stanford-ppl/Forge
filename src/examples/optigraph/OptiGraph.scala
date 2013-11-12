package ppl.dsl.forge
package examples
package optigraph

import core.{ForgeApplication,ForgeApplicationRunner}

// This object lets us build our DSL
object OptiGraphDSLRunner extends ForgeApplicationRunner with OptiGraphDSL

trait OptiGraphDSL extends ForgeApplication with NodeViewOps with ArrayViewOps with AtomicIntArrayOps with GraphOps with GraphCollectionOps with NodeDataOps with NodeOps with EdgeOps {
  /**
   * The name of our DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  def dslName = "OptiGraph"
    
  /**
   * The specification is the DSL definition (types, data structures, ops, code generators)
   */
  def specification() = {
    /**
     * Include Scala ops
     */
    importScalaOps()
        
    /**
     * The main portion of our DSL
     */
    importNodeViewOps()
    importNodeDataOps()
    importArrayViewOps()
    importAtomicIntArrayOps()
	  importNodeOps()
	  importEdgeOps()
	  importGraphCollectionOps()
    importGraphOps()
  }
} 
